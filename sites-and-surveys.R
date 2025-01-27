########################################################################
## ---------------------------------------------------------------------
##
## Data processing, analyses, and visualizations for 
## REEF Lionfish Trap Project
##
## sites-and-surveys.R

## Working libraries
rm(list=ls()); gc(); windows()
library(dplyr)
library(ggplot2)
library(cowplot)
library(sf)
library(maps)


## -----------------------------------------------------------------------------
## Sites

## Read the CSV file (ensure correct encoding)
sites_raw <- read.csv("./data/raw/site-info.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")

## Replace the degree symbol and preprocess
sites_raw <- sites_raw %>%
  mutate( # Safely sanitize columns using tryCatch
    lat = tryCatch(gsub("\xb0", "°", lat, fixed = TRUE), error = function(e) lat),
    long = tryCatch(gsub("\xb0", "°", long, fixed = TRUE), error = function(e) long)
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
write.csv(sites, "./data/processed/sites-coords-cleaned.csv", row.names = F)


## -----------------------------------------------------------------------------
## Diver surveys

diver_surveys <- read.csv("./data/raw/diver_surveys.csv")
sites_subset <- sites[, c("site_name", "site_id", "lat_dd", "long_dd")] ## Subset site_id and coordinates
sites_subset <- sites_subset[!duplicated(sites_subset$site_name), ] ## Remove duplicates, keeping only the first occurrence of each site_name
diver_surveys$site_name[grep("Tims Mutton West", diver_surveys$site_name)] <- "Tims Mutton West" ## I don't know why this value wouldn't match, so mannually fix. 

## Right join site_id, lat_dd, and long_dd to the diver_surveys dataframe
diver_surveys <- merge(diver_surveys, sites_subset, by = "site_name", all.x = TRUE)
diver_surveys <- diver_surveys[order(diver_surveys$date, decreasing = FALSE), ]

## Write out merged datasheet
write.csv(diver_surveys, "./data/processed/diver_surveys_with_coords.csv", row.names = FALSE)

## Summary stats
summ_diver_surveys <- diver_surveys %>%
  dplyr::select(lf_count, relief_m, depth_m) %>% 
  na.omit() %>% 
  summarise(
    Mean_Lionfish = mean(lf_count),
    Median_Lionfish = median(lf_count),
    SD_Lionfish = sd(lf_count),
    Min_Lionfish = min(lf_count),
    Max_Lionfish = max(lf_count),
    Mean_Relief = mean(relief_m),
    Median_Relief = median(relief_m),
    SD_Relief = sd(relief_m),
    Min_Relief = min(relief_m),
    Max_Relief = max(relief_m),
    Mean_Depth = mean(depth_m),
    SD_Depth = sd(depth_m),
    Min_Depth = min(depth_m),
    Max_Depth = max(depth_m)
  ); print(summ_diver_surveys)


## Look at scatter plot for 1-to-1 comparison
plot(  diver_surveys$relief_m, diver_surveys$lf_count,  # x and y variables
  pch = 16, col = rgb(0, 0, 1, 0.7),  # Semi-transparent blue points
  xlab = "Reef Relief (m)", ylab = "Lionfish Count",
  cex = 1.2, bty = 'n'
); abline(lm(diver_surveys$lf_count ~ diver_surveys$relief_m), col = "red", lty = 2) ## Add a one-to-one trend line (linear fit)

## Examine linear relationshiope between lionfish count and reef releife
lf_relief_lm <- lm(diver_surveys$lf_count ~ diver_surveys$relief_m)
summary(lf_relief_lm)

## Conduct Spearman correlation test
correlation_test <- cor.test(diver_surveys$lf_count, diver_surveys$relief_m, method = "spearman")
print(correlation_test)

## Examine relationship between lionfish count and habitat type ----------------
diver_surveys_clean <- diver_surveys %>%
  filter(!is.na(lf_count)) %>% ## Remove rows with missing values in `lf_count`
  mutate(hab_class = as.factor(hab_class)) 

## Conduct ANOVA
anova_hab_class <- aov(lf_count ~ hab_class, data = diver_surveys_clean)
summary(anova_hab_class)
TukeyHSD(anova_hab_class)

## Examine ANOVA assumptions
qqnorm(residuals(anova_hab_class)); qqline(residuals(anova_hab_class)) ## Normality of residuals
shapiro.test(residuals(anova_hab_class)) ## Shapiro-Wilk test for normality
bartlett.test(lf_count ~ hab_class, data = diver_surveys_clean)  # Bartlett's test

## Conduct Kruskal Wallis test
kruskal.test(lf_count ~ hab_class, data = diver_surveys_clean)

## Make 4 plots for diver surveys ----------------------------------------------
## Set standard plot theme for standardization
standard_theme <- theme_minimal(base_size = 12) +  # Base font size for the plots
  theme(
    axis.text = element_text(size = 11, color = "black"),       # Standardize axis tick label size and color
    axis.title = element_text(size = 11, color = "black"),      # Standardize axis title size and color
    axis.line = element_line(color = "black", size = 0.8),      # Standardize axis line color and thickness
    panel.grid.major = element_line(color = "gray80"),          # Light gray major grid lines
    panel.grid.minor = element_line(color = "gray90"),          # Light gray minor grid lines
    panel.grid.major.x = element_blank(),                       # Remove major vertical grid lines
    panel.grid.minor.x = element_blank(),                       # Remove minor vertical grid lines
    panel.border = element_blank()                              # Remove panel borders
  )

## Find the maximum frequency across both histograms
max_lf  <- max(diver_surveys$lf_count, na.rm = T) 
max_rel <- max(diver_surveys$relief_m, na.rm = T) 

## Define the first histogram (Lionfish Count)
hist_lf <- ggplot(diver_surveys, aes(x = lf_count)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Lionfish Count", y = "Frequency") +
  standard_theme +
  ylim(0, max_lf) +
  scale_y_continuous(breaks = seq(0, max_lf, by = 2), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, max_lf, by = 3),expand = c(0,0))
hist_lf

## Define the second histogram (Reef Relief)
hist_rel <- ggplot(diver_surveys, aes(x = relief_m)) +
  geom_histogram(binwidth = 0.2, fill = "green", color = "black", alpha = 0.7) +
  labs(x = "Max reef relief (m)", y = "Frequency") +
  standard_theme +
  ylim(0, max_rel) +
  scale_y_continuous(breaks = seq(0, 10, by = 1), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5),expand = c(0,0))
hist_rel

## Scatter plot with a one-to-one trend line
scatter_plot <- ggplot(diver_surveys, aes(x = relief_m, y = lf_count)) +
  geom_point(color = "blue", alpha = 0.7, size = 2) +  # Semi-transparent blue points
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +  # Linear fit line
  geom_hline(yintercept = 0, color = "black", size = 0.8) +  # Add a horizontal line at y = 0
  geom_vline(xintercept = 0, color = "black", size = 0.8) +  # Add a vertical line at x = 0
  labs(x = "Max reef relief (m)", y = "Lionfish count") +
  theme_minimal(); scatter_plot

## Make bar plot
## Calculate mean and standard deviation of `lf_count` by `hab_class`
habitat_summary <- diver_surveys_clean %>%
  group_by(hab_class) %>%
  summarise(
    mean_lf_count = mean(lf_count),
    sd_lf_count = sd(lf_count),
    n = n()
  )

hab_class <- ggplot(habitat_summary, aes(x = hab_class, y = mean_lf_count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_errorbar(
    aes(
      ymin = pmax(mean_lf_count - sd_lf_count, 0),  # Ensure the lower bound doesn't go below 0
      ymax = mean_lf_count + sd_lf_count
    ),
    width = 0.2, color = "black"
  ) +
  labs(y = "Mean lionfish count", x = "") +
  standard_theme +
  theme(
    axis.text.x = element_text(size = 12, hjust = 0.5),
    panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
    panel.grid.minor.x = element_blank()   # Remove minor vertical grid lines
  ) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0, 12, by = 3), expand = c(0, 0))
hab_class


## Combine the plots
plots_diver_surveys <- plot_grid(
  hist_rel, hist_lf, scatter_plot, hab_class,
  labels = c("A", "B", "C", "D"), # Panel labels
  label_size = 14,          # Size of panel labels
  align = "v",              # Align vertically
  label_x = 0.85,            # Horizontal position of labels (inset)
  label_y = 0.95,           # Vertical position of labels (inset)
  ncol = 2                  # Two panels side-by-side
); print(plots_diver_surveys)

## Write out plot as a PNG image
ggsave(
  "./figures/plot_diver_surveys.png", plots_diver_surveys,
  width = 6.5, height = 6.5,  
  units = "in", dpi = 2000               
)


## -----------------------------------------------------------------------------
##
## Plot study sites

## Load necessary libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(marmap)
library(metR)
library(ggmap)
library(prettymapr)


## Define a bounding box for the Florida Keys
bbox <- c(left = -81.0, bottom = 24.6, right = -80.1, top = 25.1)

## Convert the dataset into a spatial object using sf
sites_sf <- st_as_sf(
  sites,
  coords = c("long_dd", "lat_dd"), # Specify longitude and latitude columns
  crs = 4326 # WGS84 coordinate reference system
)

## Convert the dataset into a spatial object using sf
diver_surveys_sf <- st_as_sf(
  diver_surveys,
  coords = c("long_dd", "lat_dd"), # Specify longitude and latitude columns
  crs = 4326 # WGS84 coordinate reference system
)

## Interactive map
library(leaflet)
leaflet(data = diver_surveys) %>%
  addTiles() %>% # Add OpenStreetMap tiles
  addMarkers(
    ~long_dd, ~lat_dd,
    popup = ~paste("Site:", site_name)
  ) %>%
  addScaleBar(position = "bottomleft")

## Get bathymetry data using marmap
bathymetry <- getNOAA.bathy(
  lon1 = bbox["left"], lon2 = bbox["right"], lat1 = bbox["bottom"], lat2 = bbox["top"], 
  resolution = 1  
)
bathymetry_df <- fortify.bathy(bathymetry) # Convert bathymetry to a data frame for ggplot

## Get high-resolution landmass data
land <- ne_countries(scale =  "large", returnclass = "sf") # Natural Earth data
contour_lines = c(-5, -10, -20, -40, -80)

## Make site plot -----------------------------------------------------------------
plot_sites <- ggplot() +
  geom_contour( ## Add bathymetry as contours
    data = bathymetry_df,
    aes(x = x, y = y, z = z),
    breaks = contour_lines,
    color = "blue", linetype = "dashed"
  ) +
  geom_text_contour( ## Add bathymetry labels
    data = bathymetry_df,
    aes(x = x, y = y, z = z, label = ..level..), ## Use contour levels as labels
    breaks = contour_lines,
    color = "blue",
    skip = 0 # 
  ) +
  geom_sf(data = land, fill = "lightgray", color = "black") +   ## Add landmass polygons
  geom_sf( ## Add site points with size and color aesthetics
    data = diver_surveys_sf,
    aes(size = lf_count, color = relief_m),
    shape = 15, alpha = 0.9,  # Circle with fill color
  ) +
  ## Adjust the map extent
  coord_sf(xlim = c(-81.1, -80.2), ylim = c(24.6, 25.1), expand = FALSE) +
  ## Add a continuous color scale (white to red)
  scale_color_gradient(low = "blue4", high = "yellow4", name = "Max relief") +
  # Add size scale
  scale_size_continuous(name = "Lionfish count") +
  ## Add labels and theme
  labs(
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  annotation_scale(location = "br") + ## Add a scale bar
  annotation_north_arrow(location = "br", style = north_arrow_fancy_orienteering()) ## Add a north arrow
print(plot_sites)

## Write out plot as a PNG image
ggsave(
  "./figures/plot_sites_with_lf-count_and_relief.png", plot_sites,
  width = 7.5, height = 5,  
  units = "in", dpi = 3000               
)
