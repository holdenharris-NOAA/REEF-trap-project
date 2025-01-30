# README: REEF Lionfish Trap Project Analyses

## Overview
This repository contains data processing, analyses, and visualizations for the **REEF Lionfish Trap Project**. The objective of this study is to evaluate trap retrievals and catches using different trap types and to assess factors influencing lionfish and bycatch capture rates.

---

## Data and Reproducibility
### Data Sources
The analysis uses **trap retrievals and catch data** from experimental deployments. The raw data file is located in:
- `./data/raw/trap-deployment-retrievals.csv`

### Required R Packages
The analysis requires the following R packages:
```r
install.packages(c("ggplot2", "dplyr", "tidyr", "cowplot", "lme4", "glmmTMB", "MASS"))
```
```r
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(lme4)
library(glmmTMB)
library(MASS)
```

### Reproducibility
- **Script Location**: `trap-retrievals-and-catches.R`
- **Functions**: Any required custom functions are sourced from `./functions.R`.
- **Data Preparation**: Ensure the correct data structure by running `str(retrievals)` before analysis.

---

## Data Processing and Summaries
### Load and Process Data
```r
retrievals <- read.csv("./data/raw/trap-deployment-retrievals.csv")
```

### Summarize Catches by Trap Type
```r
summ_catches <- retrievals %>%
  filter(lf_catch_num > 0 | bycatch_num > 0) %>%
  group_by(trap_type) %>%
  summarise(
    ret_with_catches = n(),
    ret_with_lfcount = sum(lf_catch_num > 0, na.rm = TRUE),
    total_lf_catch = sum(lf_catch_num, na.rm = TRUE),
    ret_with_bycatch = sum(bycatch_num > 0, na.rm = TRUE),
    total_bycatch = sum(bycatch_num, na.rm = TRUE)
  )
print(summ_catches)
```

**Results:**
- ML traps caught **1 lionfish** and **42 bycatch** in **10 of 45 retrievals**.
- GT traps caught **0 lionfish** and **1 bycatch** in **1 of 45 retrievals**.

---

## Visualization of Retrievals and Catches
### Reshape Data for Plotting
```r
plot_data_retrievals <- summ_catches %>%
  pivot_longer(cols = c(ret_with_lfcount, ret_with_bycatch),
               names_to = "catch_type",
               values_to = "retrieval_count")

plot_data_totals <- summ_catches %>%
  pivot_longer(cols = c(total_lf_catch, total_bycatch),
               names_to = "catch_type",
               values_to = "total_count")
```

### Generate Bar Plots
```r
plot_retrievals <- ggplot(plot_data_retrievals,
                          aes(x = trap_type, y = retrieval_count, fill = catch_type)) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
  theme_minimal() +
  scale_fill_manual(values = c("ret_with_lfcount" = "blue", "ret_with_bycatch" = "red"),
                    labels = c("Bycatch", "Lionfish")) +
  theme(
    legend.position = "none",
    axis.line.x = element_line(color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 10),
    axis.ticks.x = element_line(color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12, color = "black")
  )
```

```r
# Save plot
 ggsave("./figures/plot_retrieval_catches.png", plot_retrieval_catches,
        width = 7, height = 3, units = "in", dpi = 4000)
```

---

## Statistical Analysis
### Data Preparation
```r
ret <- retrievals %>%
  mutate(
    ret_with_lfcount = ifelse(lf_catch_num > 0, 1, 0),
    ret_with_nontarg = ifelse(bycatch_num > 0, 1, 0)
  )
ret$site_ID   <- as.factor(ret$site_ID)
ret$trap_type <- as.factor(ret$trap_type)
ret$pot_light <- as.factor(ret$pot_light)
```

### **Binomial Logistic Regression (Bycatch Occurrence)**
#### Mixed Effects Model
```r
bin_glmm_nt <- glmer(ret_with_nontarg ~ trap_type + pot_light + lf_num_surv + site_rel + (1 | site_ID),
                     family = binomial(link = "logit"), data = ret)
summary(bin_glmm_nt)
```
**Notes:**
- The model encountered a singularity issue (`boundary (singular) fit`), indicating that the random effect for `site_ID` was unnecessary.

#### Simplified Model Without Random Effect
```r
bin_glm_nt <- glm(ret_with_nontarg ~ trap_type + pot_light + lf_num_surv + site_rel,
                  family = binomial(link = "logit"), data = ret)
summary(bin_glm_nt)
```

**Key Findings:**
- **MLT traps** significantly increased the odds of a non-target catch (**p = 0.0223, OR = 12.1**).
- **Site relief** was marginally significant (**p = 0.0965**), suggesting that higher relief may reduce non-target catches.
- **Pot lights and lionfish abundance** were not significant predictors.

### **Negative Binomial Regression (Bycatch Counts)**
#### Attempted Mixed Effects Model (Convergence Issue)
```r
negbin_glmm_nt <- glmmTMB(
  bycatch_num ~ trap_type + pot_light + lf_num_surv + site_rel + (1 | site_ID),
  family = nbinom2, data = ret)
summary(negbin_glmm_nt)
```
**Result:** Model did not converge (`non-positive-definite Hessian matrix`).

#### Final Negative Binomial GLM (Without Random Effects)
```r
negbin_glm_nt <- glm.nb(bycatch_num ~ trap_type + pot_light + lf_num_surv + site_rel, data = ret)
summary(negbin_glm_nt)
```
**Findings:**
- The negative binomial model was successfully fitted.
- **Trap type remains the dominant predictor of bycatch numbers.**

---

## Conclusion
- **MLT traps** had significantly higher odds of non-target catches than GT traps.
- **Site relief** may influence bycatch occurrence, but further research is needed.
- **Negative binomial modeling was required for count-based analysis**, and mixed-effects models were unnecessary due to singularity issues.

This analysis provides key insights into lionfish trapping efficiency and non-target impacts. Future studies should explore the role of habitat structure and trap modifications on catch composition.

---

## Contact
For questions or collaboration inquiries, please contact:
- **Research Lead**: [Your Name]
- **Institution**: [Your Organization]
- **Email**: [Your Email]

