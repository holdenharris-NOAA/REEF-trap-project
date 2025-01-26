library(dplyr)

vids <- read.csv("./data/raw/video-reads.csv", stringsAsFactors = FALSE); str(vids)
vids <- vids %>% filter(!is.na(lf_count))
nrow(vids)

## About
## 4,981 rows (video reads) and 31 columns, with information on trap deployments, video metadata, 
## and observations of lionfish and bycatch
## Lionfish presence: Columns lf_present and lf_count
## Bycatch species: Column bycatch_spp with related data in abundance_nonlf and behavior_nonlf.
## Trap details: Columns like trap_type, trap_num, and site_name.
## Video metadata: Columns like date_vid, length_vid_s, and file_name_vid.
## Time of day: Column tod categorizes day or night

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
lf_present <- vids %>% filter(lf_count > 0)

lf_present %>% 
  summarize(
    unique_deployments = n_distinct(paste(trap_num, date_dep)),
    unique_sites = n_distinct(site_name)
  )

lf_present %>% 
  group_by(deployment, trap_type, site_name) %>%
  summarize(
    count = n()) %>%
  arrange(desc(count))
