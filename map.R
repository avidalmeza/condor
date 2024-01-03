library(tidyverse)
require(move)
library(sf)
library(ggmap)

# Set Movebank username and password
username <- Sys.getenv('username')
password <- Sys.getenv('password')

# Login to Movebank
loginStored <- move::movebankLogin(username = username, password = password)

# Set Google Maps API key
google_maps_api <- Sys.getenv('google_maps_api')

# Enable Google services
ggmap::register_google(key = google_maps_api)
ggmap::has_google_key()

# Define study name
study_name <- 'Andean Condor Vultur gryphus Bariloche, Argentina, 2013-2018'

# Obtain study ID
study_id <- move::getMovebankID(study_name, login = loginStored)

# Obtain study metadata
meta <- move::getMovebank('individual', study_id = study_id, login = loginStored)

# Extract individual species ID
bird_id <- meta$id

# Define empty list
track_list <- list()

# Initialize index
index <- 1

# Add track for each individual species ID to list
for (i in bird_id){
  
  # Obtain track as data frame
  track <- move::getMovebank('event', study_id = study_id, individual_id = i, login = loginStored)
  
  # Remove rows where location points are NA
  track <- track %>% filter(!is.na(location_long) & !is.na(location_lat))
  
  # Add to track list
  track_list[[index]] <- track
  
  # Add index count
  index <- index + 1 
}

# Plot location points to select track of interest
ggplot(track_list[[6]], aes(x = location_long,
                            y = location_lat)) + geom_point()

# Select track of interest
track <- track_list[[6]]

track <- track %>% 
  # Convert timestamp to datetime and create date column
  mutate(timestamp = lubridate::as_datetime(timestamp),
         date = as.character(as.Date(timestamp))) %>%
  # Remove rows where time stamp is NA
  filter(!is.na(timestamp))

# Calculate mean location points by date
mean_track <- track %>%
  group_by(date) %>%
  summarise(lat = mean(location_lat),
            lon = mean(location_long)) %>%
  mutate(date = as.Date(date)) %>%
  # Order rows in ascending column value
  arrange(date) 

# Create sf object
p_track <- sf::st_as_sf(mean_track, coords = c('lon', 'lat'), crs = '+proj=longlat')
p_track <- sf::st_transform(p_track, crs = 4326)

# Create LINESTRING object
l_track <- p_track %>%
  # Order rows in ascending column value
  arrange(date) %>%
  # Set GIS union operation to FALSE
  summarize(do_union = FALSE, .groups = 'drop') %>%
  # Cast points to LINESTRING feature
  sf::st_cast('LINESTRING')

# Smooth feature with spline interpolation
sp_track <- smoothr::smooth(l_track, method = 'spline')

# Extract location points from smooth feature
sp_pts <- st_cast(sp_track, 'POINT')

# Join location points to nearest point from original sf object
output <- st_join(sp_pts, p_track, join = st_nearest_feature)

# Reorder location points 
output <- output %>%
  mutate(
    # Set first time stamp
    date = replace(date, 1, min(p_track$date)),
    # Set last time stamp
    date = replace(date, n(), max(p_track$date)),
    # Create time difference column for row n and n+1
    # Negative time difference value are errors
    difftime = difftime(date, lag(date)))

# Extract median time difference from original sf object
# Median time difference value is upper limit of interpolations
max_diff <- median(difftime(p_track$date, lag(p_track$date)), na.rm = T)

# Remove possible spline interpolation errors
output <- output %>%
  # Define time interpolation
  mutate(t_interp = case_when(
    # Set NA for large time differences
    difftime > max_diff ~ as.POSIXct(NA),
    # Set timestamp if it's likely not out of order
    difftime >= 0 ~ date,
    # Set timestamp if NA in time difference
    is.na(difftime) ~ date
  ))

# Perform linear interpolation for missing timestamps
output <- output %>%
  mutate(t_interp = as.POSIXct(zoo::na.approx(t_interp))) %>%
  dplyr::select(-difftime)

# View interpolated track
ggplot(data = st_as_sf(output), 
       aes(color = t_interp)) + 
  geom_sf() + 
  labs(color = 'Date')

# Transform output data to specified CRS
output <- st_transform(output, crs = 4326)

# Set bounding box for map
bbox <- st_bbox(output)

# Extract latitude and longitude
output$lat <- st_coordinates(output)[,2]
output$lon <- st_coordinates(output)[,1]

# Obtain satellite map of bounding box
argentina <- ggmap::get_map(
  location = c(left = bbox[[1]], bottom = bbox[[2]], 
               right = bbox[[3]], top = bbox[[4]]),
  zoom = 9, scale = 2,
  maptype = 'satellite', source = 'google'
)

# View map
ggmap::ggmap(argentina)

# Extract citation
citation <- citation('ggmap')

# Extract breakpoints
scale_dates <- pretty(output$t_interp)

# Set bounding box for plot
bbox[c(1, 2)] <- bbox[c(1, 2)] - 0.05
bbox[c(3, 4)] <- bbox[c(3, 4)] + 0.05
  
# Plot satellite map and tracks
ggmap::ggmap(argentina) + 
  ggplot2::coord_sf(crs = st_crs(4326),
           xlim = bbox[c(1,3)], ylim = bbox[c(2,4)], expand = TRUE) +
  ggplot2::geom_path(data = output,
            aes(x = lon, y = lat, color = t_interp),
            linewidth = 0.8, lineend = 'round', inherit.aes = FALSE) +
  ggplot2::scale_color_viridis_c(breaks = as.numeric(scale_dates),
                                 labels = format(scale_dates, format = '%b %d'),
                                 limits = c(min(as.numeric(scale_dates)), max(as.numeric(scale_dates))),
                                 guide = guide_colorbar(direction = 'horizontal',
                                                        label.position = 'bottom')) +
  theme(legend.position = c(0.25,0.25),
        legend.margin = margin(t = 0, unit = 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8, color = '#edede9'),
        legend.key = elementalist::element_rect_round(radius = unit(0.5, 'snpc')),
        legend.key.width = unit(1, 'cm'),
        legend.key.height = unit(0.1, 'cm'),
        legend.background = element_blank(),
        axis.title = element_blank())
