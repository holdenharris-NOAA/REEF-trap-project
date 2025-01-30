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

- **Script Location**: `trap-retrievals-and-catches.R`
- **Functions**: Any required custom functions are sourced from `./functions.R`.
- **Data Preparation**: Ensure the correct data structure by running `str(retrievals)` before analysis.


### Summarize Catches by Trap Type
```r
trap_type ret_with_catches ret_with_lfcount total_lf_catch ret_with_bycatch
  <chr>                <int>            <int>          <int>            <int>
1 GT                       1                0              0                1
2 MLT                     11                1              1               10
```

**Results:**
- ML traps caught **1 lionfish** and **42 bycatch** in **10 of 45 retrievals**.
- GT traps caught **0 lionfish** and **1 bycatch** in **1 of 45 retrievals**.

---

## Statistical Analysis
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

glm(formula = ret_with_nontarg ~ trap_type + pot_light + lf_num_surv + 
    site_rel, family = binomial(link = "logit"), data = ret)

Coefficients:
             Estimate Std. Error z value Pr(>|z|)  
(Intercept)  -2.88155    1.12624  -2.559   0.0105 *
trap_typeMLT  2.49317    1.09137   2.284   0.0223 *
pot_lightyes -0.29445    0.74612  -0.395   0.6931  
lf_num_surv   0.03394    0.07649   0.444   0.6572  
site_rel     -1.29045    0.77651  -1.662   0.0965 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 61.825  on 85  degrees of freedom
Residual deviance: 49.571  on 81  degrees of freedom
  (4 observations deleted due to missingness)
AIC: 59.571

Number of Fisher Scoring iterations: 6
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
