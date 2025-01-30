# README: REEF Lionfish Trap Project Video Analyses

## Overview
This repository contains data processing, analyses, and visualizations for the **REEF Lionfish Trap Project Video Data**. The objective of this study is to evaluate trap video recordings to assess factors influencing lionfish and bycatch observations.

---

## Data and Reproducibility
### Data Sources
The analysis uses **video observation data** from trap deployments. The raw data files are located in:
- `./data/raw/video-reads.csv`
- `./data/raw/diver_surveys.csv`

### Required R Packages
The analysis requires the following R packages:
```r
install.packages(c("ggplot2", "dplyr"))
```
```r
library(dplyr)
library(ggplot2)
```

- **Script Location**: `trap-videos.R`
- **Functions**: Any required custom functions are sourced from `./functions.R`.
- **Data Preparation**: Ensure the correct data structure by running `str(vids)` before analysis.

---

### Summary of Video Data
- **4,981 video reads** across **84 trap deployments** at **42 sites**.
- **Lionfish presence:** `lf_present` (binary) and `lf_count` (count of lionfish per video).
- **Bycatch species:** `bycatch_spp` with `abundance_nonlf` and `behavior_nonlf`.
- **Trap details:** `trap_type`, `trap_num`, `site_name`.
- **Video metadata:** `date_vid`, `length_vid_s`, `file_name_vid`.
- **Time of day:** `tod` categorizes as day or night.

## Statistical Analysis
### **Binomial Logistic Regression (Lionfish Presence in Videos)**
#### Mixed Effects Model
```r
bin_glmm_lfvid <- glmer(
  lf_present ~ trap_type + as.factor(days_deployed) + (1 | deployment),
  family = binomial(link = "logit"), data = vids)
summary(bin_glmm_lfvid)
```
**Key Findings:**
- Large variance (**16.29**) suggests significant variation in lionfish presence across deployments.
- **Trap type (MLT)** has a positive but non-significant effect (**p = 0.3814**).
- **Days deployed:**
  - **Day 2** significantly increases lionfish presence (**p = 0.0347**).
  - **Day 1 & Day 3** show no significant difference.

### **Binomial GLMM with Lionfish Abundance**
```r
bin_glmm_lfvid2 <- glmer(
  lf_present ~ trap_type + as.factor(days_deployed) + survey_lf_count + (1 | deployment),
  family = binomial(link = "logit"), data = vids)
summary(bin_glmm_lfvid2)
```
**Key Findings:**
- **Including site lionfish abundance (survey_lf_count)** improved model fit (**ΔAIC = -50**).
- **Days deployed:** Day 2 effect is now marginal (**p = 0.062**).
- **Surveyed lionfish abundance** positively correlates with lionfish presence in videos (**p = 0.098**).

---

## Conclusion
- **MLT traps** had a positive but non-significant effect on lionfish presence in videos.
- **Day 2 of deployment** showed a significant increase in lionfish presence.
- **Site-level lionfish abundance** was positively associated with video-based detections.
