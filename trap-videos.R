########################################################################
## ---------------------------------------------------------------------
##
## Data processing, analyses, and visualizations for 
## REEF Lionfish Trap Project
##
## trap-videos.R

source("./functions.R")
library(dplyr)
library(ggplot2)

vids <- read.csv("./data/raw/video-reads.csv", stringsAsFactors = FALSE); str(vids)
vids <- vids %>% filter(!is.na(lf_count))
nrow(vids)

## About the video data set
## 4,981 rows (video reads) and 31 columns, with information on trap deployments, video metadata, 
## and observations of lionfish and bycatch
## Lionfish presence: Columns lf_present and lf_count
## Bycatch species: Column bycatch_spp with related data in abundance_nonlf and behavior_nonlf.
## Trap details: Columns like trap_type, trap_num, and site_name.
## Video metadata: Columns like date_vid, length_vid_s, and file_name_vid.
## Time of day: Column tod categorizes day or night

## Convert date columns from character to Date format
vids <- vids %>%
  mutate(
    date_dep = as.Date(date_dep, format = "%m/%d/%Y"),
    date_ret = as.Date(date_ret, format = "%m/%d/%Y"),
    date_vid = as.Date(date_vid, format = "%m/%d/%Y"),
    corrected_vid_date = as.Date(corrected_vid_date, format = "%m/%d/%Y")
  )

## Only "corrected_vid_date" is given for corrections; these need to be aggregated into a single column
## aggregate "corrected_vid_date" and "date_vid" into a single column
vids <- vids %>%
  mutate(
    corrected_vid_date = if_else(
      !is.na(corrected_vid_date),
      as.Date(corrected_vid_date, format = "%m/%d/%Y"),
      as.Date(date_vid, format = "%m/%d/%Y")
    )
  )

## Calculate days deployed
vids <- vids %>% mutate(days_deployed = as.numeric(corrected_vid_date - date_dep))

## Make binary column for lionfish present or not
vids <- vids %>% mutate(lf_present = ifelse(lf_count > 0, 1, 0))  # 1 if lionfish observed, else 0


################################################################################
## 
## Summary statistics

## Look at number of sites
n_distinct(vids$site_name)
n_distinct(vids$deployment)
site_summary <- vids %>%
  group_by(deployment) %>%
  summarize(rows_per_site = n()) %>%
  arrange(desc(rows_per_site)); site_summary
## --> Videos at 84 traps 42 different sites

## Summary statistics for the number of rows (video reads) by site
site_summary_stats <- vids %>%
  group_by(deployment) %>%
  summarize(rows_per_site = n()) %>%
  summarize(
    min_reads = min(rows_per_site),
    max_reads = max(rows_per_site),
    avg_reads = mean(rows_per_site), 
    std_reads = sd(rows_per_site)
  ); site_summary_stats
## Average of 59 videos per deployment

## Look at videos where lionfish were observed -----------------------

## Ensure lf_count is numeric and remove NAs
vids$lf_count <- as.numeric(vids$lf_count)

# Filter for video reads with lf_count > 1
lf_summary <- vids %>%
  filter(lf_count > 1) %>%
  group_by(lf_count) %>%
  summarize(count = n()) %>%
  arrange(lf_count)

## Frequency table for all lf_count values (including 0, 1, 2, or more)
lf_freq <- vids %>%
  group_by(lf_count) %>%
  summarize(count = n()) %>%
  arrange(lf_count); print(lf_freq)
## 38 videos had 1 lionfish, 1 video had 2 lionfish in them

## Look at videos with lionfish present --------------------------------
lf_present <- vids %>% 
  filter(lf_count > 0) %>% 
  group_by(site_name, trap_type, tod) %>%
  summarize(
    count = n()) %>%
  arrange(desc(count)); lf_present

## Look at summary of lionfish counts by days deployed
days_deployed_summary <- 
  vids %>% 
  filter(lf_count > 0) %>% 
  group_by(deployment, days_deployed) %>%
  summarize(
    count = n()) %>%
  arrange(desc(deployment)); days_deployed_summary

## Make plot
hist(days_deployed_summary$days_deployed)

## Bar plot of counts by days_deployed
library(ggplot2)
library(scales)

## Plot of counts by days_deployed
plot_days_deployed <- 
  ggplot(days_deployed_summary, aes(x = factor(days_deployed), y = count, fill = deployment)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    theme_minimal() +
    labs(
      x = "Time deployed (# days)",
      y = "Video samples (#)",
      fill = "Deployment:\ntrap type and site"
    ) +
    theme(
      axis.text = element_text(color = "black"),  # Black axis text
      axis.title = element_text(color = "black"),  # Black axis labels
      axis.line = element_line(color = "black"),  # Dark axis lines
      panel.grid.major.y = element_line(color = "lightgray", size = 0.5),  # Light gray horizontal grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      panel.grid.major.x = element_blank()  # Remove vertical grid lines
    ) +
    scale_y_continuous(
      breaks = seq(0, max(days_deployed_summary$count, na.rm = TRUE), by = 1),  # Y-axis breaks at every 1 unit
      expand = c(0, 0)  # Removes space between axis and data
    ) +
    scale_x_discrete(
      expand = c(0, 0)  # Removes space between axis and data
    ); plot_days_deployed

## Write out plot as a PNG image
ggsave(
  "./figures/plot_days_deployed.png", plot_days_deployed,
  width = 6.5, height = 3,  
  units = "in", dpi = 3000               
)

################################################################################
##
## Analysis

## Mixed effects model with deployment as random effect
bin_glmm_lfvid <- 
  glmer(lf_present ~ trap_type + as.factor(days_deployed)
                     + (1 | deployment), ## Random effect -> Incoporate same camera on deployment
                     family = binomial(link = "logit"), data = vids)
summary(bin_glmm_lfvid)
coeftab_glm(bin_glmm_lfvid)
write.csv(coeftab_glm(bin_glmm_lfvid), "./tables/glmm-vid-lfpresence.csv", row.names = FALSE) ## Write out results table

## GLMM random effect. The large variance (16.29) indicates considerable variability across 
## deployments in lionfish presence, suggesting the importance of including deployment as a random effect.
##
## Intercept: logit of -10.14 corresponds to an extremely low probability of lionfish presence 
## under the baseline conditions (trap_type = GT and days_deployed = 0).
##
## Trap type. coefficient for trap_typeMLT is positive (1.08), suggesting lionfish may be more likely 
## to be observed near MLT traps than GT traps, but this effect is not statistically significant (p = 0.3814).
##
## Days deployed. 
## Day 1 - No significant difference in lionfish presence compared to baseline (p = 0.9831).
## Day 2 - Statistically significant increase in the probability of lionfish presence compared to baseline (p = 0.0347). The logit of 1.09 translates to an increased odds ratio (OR ≈ 2.97).
## Day 3 - No significant difference compared to baseline (p = 0.7404).
