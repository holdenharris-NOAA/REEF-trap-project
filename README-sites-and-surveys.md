# REEF Lionfish Trap Project

## Project Overview

This project is part of the REEF Lionfish Trap Project, aimed at analyzing study sites and diver surveys to test experimental lionfish traps. The code processes geographic and survey data, conducts statistical analyses, and generates visualizations for scientific exploration.

---

## Folder Structure

The following folder structure is assumed:
- `./data/raw/`: Contains raw input data (e.g., `site-info.csv`, `diver_surveys.csv`).
- `./data/processed/`: Stores processed data (e.g., cleaned coordinates, merged datasets).
- `./figures/`: Contains generated visualizations.

---

## Prerequisites

The following R libraries are required:

```r
library(dplyr)
library(ggplot2)
library(sf)
library(ggspatial)
library(marmap)
library(metR)
library(cowplot)
library(rnaturalearth)
library(leaflet)
```

Install missing packages with:

```r
install.packages(c("dplyr", "ggplot2", "sf", "ggspatial", "marmap", "metR", "cowplot", "rnaturalearth", "leaflet"))
```

---

## Code Structure

### 1. **Processing Site Data**
- Reads raw site data (`site-info.csv`), cleans latitude/longitude values, and converts to decimal degrees.
- Outputs cleaned site coordinates to `./data/processed/sites-coords-cleaned.csv`.

```r
# Read and preprocess site data
sites_raw <- read.csv("./data/raw/site-info.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")

# Data cleaning and transformation
sites <- sites_raw %>%
  mutate(
    clean_lat = gsub("['NS] ?", "", lat),
    clean_long = gsub("['EW] ?", "", long),
    lat_deg = as.numeric(sub("°.*", "", clean_lat)),
    lat_min = as.numeric(sub(".*°([0-9]+\.?[0-9]*)", "\\1", clean_lat)),
    long_deg = as.numeric(sub("°.*", "", clean_long)),
    long_min = as.numeric(sub(".*°([0-9]+\.?[0-9]*)", "\\1", clean_long)),
    lat_dd = lat_deg + (lat_min / 60),
    long_dd = -(long_deg + (long_min / 60)) # Negative for western hemisphere
  )

write.csv(sites, "./data/processed/sites-coords-cleaned.csv", row.names = FALSE)
```

---

### 2. **Processing Diver Survey Data**
- Merges cleaned site coordinates with diver survey data (`diver_surveys.csv`) using `site_name`.
- Outputs merged data to `./data/processed/diver_surveys_with_coords.csv`.

```r
# Merge site data with diver surveys
diver_surveys <- merge(diver_surveys, sites_subset, by = "site_name", all.x = TRUE)
write.csv(diver_surveys, "./data/processed/diver_surveys_with_coords.csv", row.names = FALSE)
```

---

### 3. **Statistical Analysis**

#### a. **Summary Statistics**
Calculates descriptive statistics for lionfish counts, reef relief, and depth.

```r
summ_diver_surveys <- diver_surveys %>%
  summarise(
    Mean_Lionfish = mean(lf_count),
    SD_Lionfish = sd(lf_count),
    Min_Lionfish = min(lf_count),
    Max_Lionfish = max(lf_count),
    Mean_Relief = mean(relief_m),
    SD_Relief = sd(relief_m),
    Min_Relief = min(relief_m),
    Max_Relief = max(relief_m),
    Mean_Depth = mean(depth_m),
    SD_Depth = sd(depth_m),
    Min_Depth = min(depth_m),
    Max_Depth = max(depth_m)
  )
print(summ_diver_surveys)
```

#### b. **Regression Analysis**
- Examines the relationship between reef relief and lionfish count using linear regression and Spearman correlation.

```r
lf_relief_lm <- lm(d`lf_count ~ relief_m, data = diver_surveys)
summary(lf_relief_lm)
correlation_test <- cor.test(diver_surveys$lf_count, diver_surveys$relief_m, method = "spearman")
print(correlation_test)
```

#### c. **ANOVA for Habitat Type**
- Compares lionfish counts across different habitat types.

```r
anova_hab_type <- aov(lf_count ~ hab_type, data = diver_surveys_clean)
summary(anova_hab_type)
```

##### Example Output:

```
            Df Sum Sq Mean Sq F value Pr(>F)
hab_type     2   19.6   9.807   0.311  0.734
Residuals   40 1261.0  31.525       
```

- Compares lionfish counts across different habitat types.

```r
anova_hab_type <- aov(lf_count ~ hab_type, data = diver_surveys_clean)
summary(anova_hab_type)
```

---

### 4. **Visualizations**

#### a. **Histograms**
- Generates histograms for lionfish count and reef relief.

#### b. **Scatter Plot**
- Creates a scatter plot with a linear trend line for reef relief vs. lionfish count.

#### c. **Bar Plot**
- Summarizes lionfish counts by habitat type.

#### d. **Combined Plots**
- Combines all visualizations into a 2x2 grid using `cowplot`.

```r
# Combine plots
plot_grid(hist_rel, hist_lf, scatter_plot, hab_type, ...)
ggsave("./figures/plot_diver_surveys.png", ...)
```

---

### 5. **Spatial Mapping**

#### a. **Interactive Map**
- Uses `leaflet` to display interactive study site locations.

#### b. **Static Map**
- Plots bathymetry, landmasses, and study sites using `ggplot2` and `sf`.

```r
# Static map
ggplot() +
  geom_contour(data = bathymetry_df, aes(x = x, y = y, z = z), ...) +
  geom_sf(data = diver_surveys_sf, aes(size = lf_count, color = relief_m), ...)
```

---

## Outputs

1. **Processed Data**:
   - `sites-coords-cleaned.csv`
   - `diver_surveys_with_coords.csv`

2. **Plots**:
   - `plot_diver_surveys.png`

3. **Interactive Map**:
   - Displays study site locations with site-specific details.

---

## Citation

Please cite this project as:

```
Your Name (2025). REEF Lionfish Trap Project. Analysis and Visualizations for Experimental Traps. Version 1.0.
