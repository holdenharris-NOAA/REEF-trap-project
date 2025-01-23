# Analyses of sites and surveys

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
```{r}
head(sites)
            site_name site_id trap_type site_dep depth_m  clean_lat  clean_long lat_deg lat_min long_deg long_min   lat_dd   long_dd
1  Grocery Ledge West     GLW        GT   GLW-GT    39.6 24°48.9551 080°38.6210      24 48.9551       80  38.6210 24.81592 -80.64368
2  Grocery Ledge West     GLW        ML   GLW-ML    39.6 24°48.9619 080°38.6205      24 48.9619       80  38.6205 24.81603 -80.64368
3 Grocery Ledge Patch     GLP        GT   GLP-GT    42.7 24°49.0455 080°38.5158      24 49.0455       80  38.5158 24.81743 -80.64193
4 Grocery Ledge Patch     GLP        ML   GLP-ML    42.7 24°49.0450 080°38.5130      24 49.0450       80  38.5130 24.81742 -80.64188
5              Higher      HI        GT    HI-GT    42.7 24°49.3453 080°38.2737      24 49.3453       80  38.2737 24.82242 -80.63790
6              Higher      HI        ML    HI-ML    42.7 24°49.3420 080°38.2699      24 49.3420       80  38.2699 24.82237 -80.63783
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
summ_diver_surveys
  Mean_Lionfish Median_Lionfish SD_Lionfish Min_Lionfish Max_Lionfish Mean_Relief Median_Relief SD_Relief
1       4.55814               3    5.521828            0           21   0.9116279           0.7 0.7436107
  Min_Relief Max_Relief Mean_Depth SD_Depth Min_Depth Max_Depth
1          0          3   37.32326 4.354409      30.7      46.6
```

#### b. **Regression Analysis**
- Examines the relationship between reef relief and lionfish count using linear regression and Spearman correlation.

```r
lf_relief_lm <- lm(d`lf_count ~ relief_m, data = diver_surveys)
summary(lf_relief_lm)

Call:
lm(formula = diver_surveys$lf_count ~ diver_surveys$relief_m)

Residuals:
   Min     1Q Median     3Q    Max 
-7.260 -2.979 -1.786  1.194 16.477 

Coefficients:
                       Estimate Std. Error t value Pr(>|t|)   
(Intercept)               1.786      1.239   1.442  0.15700   
diver_surveys$relief_m    3.041      1.058   2.874  0.00639 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.099 on 41 degrees of freedom
  (2 observations deleted due to missingness)
Multiple R-squared:  0.1677,	Adjusted R-squared:  0.1474 
F-statistic: 8.261 on 1 and 41 DF,  p-value: 0.006394
```

#### c. **ANOVA for Habitat Type**
- Compare lionfish counts across different habitat types.
```r
## Conduct ANOVA
> anova_hab_type <- aov(lf_count ~ hab_type, data = diver_surveys_clean)
> summary(anova_hab_type)
            Df Sum Sq Mean Sq F value Pr(>F)
hab_type     2   19.6   9.807   0.311  0.734
Residuals   40 1261.0  31.525               
> TukeyHSD(anova_hab_type)
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = lf_count ~ hab_type, data = diver_surveys_clean)

$hab_type
                                               diff       lwr      upr     p adj
low-relief hard bottom-artificial reef    -0.240000 -6.934802 6.454802 0.9958120
medium-relief reef-artificial reef        -1.661538 -8.852910 5.529833 0.8407389
medium-relief reef-low-relief hard bottom -1.421538 -6.094393 3.251316 0.7410841
```

### 4. **Visualizations**
#### a. **Histograms**
- Generates histograms for lionfish count and reef relief.
#### b. **Scatter Plot**
- Creates a scatter plot with a linear trend line for reef relief vs. lionfish count.
#### c. **Bar Plot**
- Summarizes lionfish counts by habitat type.
#### a. **Interactive Map**
- Uses `leaflet` to display interactive study site locations.
#### b. **Static Map**
- Plots bathymetry, landmasses, and study sites using `ggplot2` and `sf`.
