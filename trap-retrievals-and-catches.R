########################################################################
## ---------------------------------------------------------------------
##
## Data processing, analyses, and visualizations for 
## REEF Lionfish Trap Project
##
## trap-retrievals-and-catches.R

rm(list=ls()); gc(); windows()
source("./functions.R")
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

## Load data
retrievals <- read.csv("./data/raw/trap-deployment-retrievals.csv")

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

## Plot ---------------------------------------------------------

## Reshape data for plotting
plot_data_retrievals <- summ_catches %>%
  dplyr::select(trap_type, ret_with_lfcount, ret_with_bycatch) %>%
  pivot_longer(cols = c(ret_with_lfcount, ret_with_bycatch), 
               names_to = "catch_type", 
               values_to = "retrieval_count"); plot_data_retrievals

plot_data_totals <- summ_catches %>%
  dplyr::select(trap_type, total_lf_catch, total_bycatch) %>%
  pivot_longer(cols = c(total_lf_catch, total_bycatch), 
               names_to = "catch_type", 
               values_to = "total_count")

## Create the plot for retrieval counts
plot_retrievals <- ggplot(plot_data_retrievals, 
                          aes(x = trap_type, y = retrieval_count, fill = catch_type)) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
  labs(
    y = "Retrievals with catches (#)" # Remove x-axis title by excluding it here
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("ret_with_lfcount" = "blue", "ret_with_bycatch" = "red"),
                    labels = c("Bycatch", "Lionfish")) +
  theme(
    legend.position = "none",                      # Remove legend
    axis.line.x = element_line(color = "black"),  # Dark x-axis
    axis.text.x = element_text(size = 12, color = "black"),  # Larger, black x-axis labels
    axis.text.y = element_text(size = 10),        # Larger y-axis labels
    axis.ticks.x = element_line(color = "black"), # Ensure x-axis ticks are dark
    axis.title.x = element_blank(),                # Remove x-axis title
    axis.title.y = element_text(size = 12, color = "black")  # Larger, black x-axis labels
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5),       # Ensure y-axis breaks are whole numbers
    expand = c(0, 0)                              # Start 0 line at the x-axis
  )

## Create the plot for total counts
plot_catches <- ggplot(plot_data_totals, 
                       aes(x = trap_type, y = total_count, fill = catch_type)) +
  geom_bar(stat = "identity", position = position_dodge(), color = 'black') +
  labs(
    y = "Catches (#)", # Remove x-axis title by excluding it here
    fill = "Catch type"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("total_lf_catch" = "blue", "total_bycatch" = "red"),
                    labels = c("Bycatch", "Lionfish")) +
  theme(
    axis.line.x = element_line(color = "black"),  # Dark x-axis
    axis.text.x = element_text(size = 12, color = "black"),  # Larger, black x-axis labels
    axis.text.y = element_text(size = 10),        # Larger y-axis labels
    axis.ticks.x = element_line(color = "black"), # Ensure x-axis ticks are dark
    axis.title.x = element_blank(),                # Remove x-axis title
    axis.title.y = element_text(size = 12, color = "black")  # Larger, black x-axis labels
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5),       # Ensure y-axis breaks are whole numbers
    expand = c(0, 0)                              # Start 0 line at the x-axis
  ); plot_catches

## Combine the two plots side by side
## Combine the plots
plot_retrieval_catches<- plot_grid(
  plot_retrievals, plot_catches,
  labels = c("A", "B"),     # Panel labels
  label_size = 14,          # Size of panel labels
  align = "v",              # Align vertically
  label_x = 0.7,           # Horizontal position of labels (inset)
  label_y = 0.95,           # Vertical position of labels (inset)
  ncol = 2                  # Two panels side-by-side
); print(plot_retrieval_catches)

## Write out plot as a PNG image
ggsave(
  "./figures/plot_retrieval_catches.png", plot_retrieval_catches, 
  width = 7, height = 3,  
  units = "in", dpi = 4000               
)


################################################################################
##
## Analyses --------------------------------------------------------------------

library(lme4)
library(glmmTMB)
library(MASS)
source("./functions.R")

## Prepare data ----------------------------------------------------------------
##
## Subset retrievals datasheet
ret <- retrievals[, c("site_name", "site_ID", "dep_date", "ret_date", "site_rel",
                      "trap_type", "lf_num_surv", "lf_catch_num", "bycatch_num",
                      "pot_light", "bycatch_desc", "bycatch_size"
                      )]

## Add binary response column for whether catch of lionfish or bycatch occurred
ret <- ret %>%
  mutate(
    ret_with_lfcount = ifelse(lf_catch_num > 0, 1, 0),  # 1 if lionfish caught, else 0
    ret_with_nontarg = ifelse(bycatch_num > 0, 1, 0)    # 1 if bycatch caught, else 0
  )

## Make sure dataframe has correct data types
ret$site_ID          <- as.factor(ret$site_ID)
ret$trap_type        <- as.factor(ret$trap_type)
ret$pot_light        <- as.factor(ret$pot_light)
str(ret)

## Non-target catches ----------------------------------------------------------

## Notes: Both trap types were deployed at each site, 
## thus site_ID as a random effect to account for the paired nature of the data.


## Binomial regression analysis ------------------------------------------------
## Mixed effects model with site_ID
bin_glmm_nt <- glmer(ret_with_nontarg ~  trap_type + pot_light + lf_num_surv + site_rel ## Fixed effects - systematic variation
                    + (1 | site_ID), ## Random effect - site-specific variability
                    family = binomial(link = "logit"), 
                    data = ret)
summary(bin_glmm_nt)
## Notes --> model fitting indicates boundary (singular) fit: see help('isSingular')
## indicates that the random effects ((1 | site_ID)) are estimated as zero or near-zero variance. 
## This means that the model treats the random intercept for site_ID as unnecessary 
## because it doesn't explain additional variability in the response (ret_with_nontarg).

## Simplify model by removing the random effect and refitting
bin_glm_nt <- glm(ret_with_nontarg ~ trap_type + pot_light + lf_num_surv + site_rel, 
                  family = binomial(link = "logit"), 
                  data = ret)
summary(bin_glm_nt)
coeftab_glm(bin_glm_nt)
write.csv(coeftab_glm(bin_glm_nt), "./tables/glm-traps-retrievals.csv")

## --> Fixed effects estimates are the same as the mixed-effects model
## Estimate: -2.88155, p = 0.0105 (significant).
## Interpretation: The log-odds of a non-target catch when all predictors are at their reference levels 
## (GT traps, no lights, lf_num_surv = 0, site_rel = 0) is -2.88.
## Odds: exp(-2.88155) ≈ 0.056. This means the baseline probability of a non-target catch is very low (5.6%).

## trap_type (MLT vs GT):
## Estimate: 2.49317, p = 0.0223 (significant).
## Interpretation: MLT traps have significantly higher odds of a non-target catch compared to GT traps.
## Odds ratio: exp(2.49317) ≈ 12.1. MLT traps are approximately 12.1 times more likely to result in a non-target catch than GT traps.

## pot_light (Yes vs No; whether there was a pot light on the trap):
## Estimate: -0.29445, p = 0.6931 (not significant).
## Interpretation: The presence of lights does not significantly affect the odds of a non-target catch.

## lf_num_surv (Number of lionfish observed in the site survey):
## Estimate: 0.03394, p = 0.6572 (not significant).
## Interpretation: The number of lionfish observed does not significantly affect the odds of a non-target catch.

## site_rel (Relief at the site):
## Estimate: -1.29045, p = 0.0965 (marginally significant, p < 0.1).
## Interpretation: Sites with higher relief may have lower odds of non-target catches.
## Odds ratio: exp(-1.29045) ≈ 0.28. Higher relief is associated with approximately 72% lower odds of a non-target catch, 
## but the result is not significant at the 0.05 level.

## Negative binomial regression analysis ------------------------------------------------
## Mixed effects model with site_ID
negbin_glmm_nt <- glmmTMB(
  ret_with_nontarg ~  trap_type + pot_light + lf_num_surv + site_rel + (1 | site_ID),
  family = nbinom2, data = ret) ## Negative binomial distribution (variance = mean * dispersion)
summary(negbin_glmm_nt)


## --> Warning message:In finalizeTMB(TMBStruc, obj, fit, h, data.tmb.old) :Model convergence problem; non-positive-definite Hessian matrix. See vignette('troubleshooting')
## Model did not converge properly. Typically means that the optimization process encountered numerical issues, 
## possibly due to overfitting, multicollinearity, lack of variability in the data, or an overly complex random effects structure.
## --> Again, including random effect isn't possible. 

## Negative binomial GLM
negbin_glm_nt <- glm.nb(bycatch_num ~ trap_type + pot_light + lf_num_surv + site_rel, 
                        data = ret)
summary(negbin_glm_nt)
coeftab_glm(negbin_glm_nt)

