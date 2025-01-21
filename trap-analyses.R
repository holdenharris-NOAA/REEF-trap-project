########################################################################
## ---------------------------------------------------------------------
##
## R CODE FOR REEF Lionfish Trap Project
##
## Analyses:  Holden Earl Harris
##            
##
## ---------------------------------------------------------------------
########################################################################


## Working libraries
rm(list=ls()); gc() 
library(dplyr)
#windows()

## -----------------------------------------------------------------------------
## Trap retrieval analyses

## Load data
retrievals <- read.csv("./data/trap-deployment-retrievals.csv")

## Summarize number of traps with lionfish or bycatch by trap type
summ_catches <- retrievals %>%
  filter(lf_catch_num > 0 | bycatch_num > 0) %>% ## Filter rows where either lf_catch_num or bycatch_num are > 0
  group_by(trap_type) %>% 
  summarise(
    ret_with_catches = n(),
    ret_with_lfcount = sum(lf_catch_num > 0, na.rm = TRUE),  # Count TRUE values
    total_lf_catch = sum(lf_catch_num, na.rm = TRUE),
    ret_with_bycatch = sum(bycatch_num > 0, na.rm = TRUE),   # Corrected variable name
    total_bycatch = sum(bycatch_num, na.rm = TRUE)
  ); summ_catches
## --> ML caught 1 lf and 42 bycatch in 10 of 45 retrievals
## --> GT caught 0 lf and 1  byactch in  1 of 45 retrievals


## -----------------------------------------------------------------------------
## Sites

library(ggplot2)
library(sf)
library(maps)


## Read the CSV file (ensure correct encoding)
sites_raw <- read.csv("./data/site-info.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")

## Replace the degree symbol and preprocess
sites_raw <- sites_raw %>%
  mutate(
    lat = gsub("\xb0", "°", lat, fixed = TRUE),
    long = gsub("\xb0", "°", long, fixed = TRUE)
  )

## Clean and split latitude and longitude into degrees and minutes
sites <- sites_raw %>%
  mutate(
    # Remove directional letters (N, S, E, W) and any special characters like "'"
    clean_lat = gsub("['NS] ?", "", lat), # Remove "N", "S", and "'"
    clean_long = gsub("['EW] ?", "", long), # Remove "E", "W", and "'",
    
    # Extract degrees and minutes for latitude
    lat_deg = as.numeric(sub("°.*", "", clean_lat)), # Degrees before "°"
    lat_min = as.numeric(sub(".*°([0-9]+\\.?[0-9]*)", "\\1", clean_lat)), # Minutes after "°"
    
    # Extract degrees and minutes for longitude
    long_deg = as.numeric(sub("°.*", "", clean_long)), # Degrees before "°"
    long_min = as.numeric(sub(".*°([0-9]+\\.?[0-9]*)", "\\1", clean_long)) # Minutes after "°"
  )

sites$lat <- sites$long <- NULL ## Remove old lat and long, some of which were missing min symbol

## Calculate degree decimal lat and long, which will be needed for sf
sites <- sites %>%
  mutate(
    lat_dd = lat_deg + (lat_min / 60),
    long_dd = -(long_deg + (long_min / 60)) # Negative for western hemisphere
  ); head(sites)

## Write out site info data frame
write.csv(sites, "./data/sites-coords-cleaned.csv", row.names = F)

## -----------------------------------------------------------------------------
## Plot study sites

## Load necessary libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(marmap)
library(metR)


## Convert the dataset into a spatial object using sf
sites_sf <- st_as_sf(
  sites,
  coords = c("long_dd", "lat_dd"), # Specify longitude and latitude columns
  crs = 4326 # WGS84 coordinate reference system
)

## Get high-resolution landmass data
land <- ne_countries(scale = "medium", returnclass = "sf") # Natural Earth data

## Get bathymetry data using marmap
bathymetry <- getNOAA.bathy(lon1 = -81, lon2 = -80.2, lat1 = 24.6, lat2 = 25.1, resolution = 1)
bathymetry_df <- fortify.bathy(bathymetry) ## Convert bathymetry to a data frame for ggplot


## Plot the map with bathymetry and study sites
ggplot() +
  # Add bathymetry as contours
  geom_contour(
    data = bathymetry_df,
    aes(x = x, y = y, z = z),
    breaks = c(-5, -10, -20, -40, -80), # Specify bathymetry depths
    color = "blue", linetype = "dashed"
  ) +
  # Add bathymetry labels
  geom_text_contour(
    data = bathymetry_df,
    aes(x = x, y = y, z = z, label = ..level..), # Use contour levels as labels
    breaks = c(-5, -10, -20, -40, -80), # Match the same breaks as contour
    color = "blue",
    skip = 0 # Prevent skipping of labels
  ) +
  # Add landmass polygons
  geom_sf(data = land, fill = "lightgray", color = "black") +
  # Add site points
  geom_sf(data = sites_sf, size = 3, shape = 3) + # X shape for study sites
  # Adjust the map extent
  coord_sf(xlim = c(-81, -80.2), ylim = c(24.6, 25.1), expand = FALSE) +
  # Add labels and theme
  labs(
    title = "Study Sites",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  annotation_scale(location = "br") + # Add a scale bar
  annotation_north_arrow(location = "br", style = north_arrow_fancy_orienteering()) # Add a north arrow
