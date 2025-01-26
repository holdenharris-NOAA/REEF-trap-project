########################################################################
## ---------------------------------------------------------------------
##
## Data processing, analyses, and visualizations for 
## REEF Lionfish Trap Project
##
## trap-retrievals-and-catches.R
##
## ---------------------------------------------------------------------
########################################################################


## -----------------------------------------------------------------------------
## Trap retrieval analyses

library(ggplot2)
library(dplyr)
library(tidyr)

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
  select(trap_type, ret_with_lfcount, ret_with_bycatch) %>%
  pivot_longer(cols = c(ret_with_lfcount, ret_with_bycatch), 
               names_to = "catch_type", 
               values_to = "retrieval_count"); plot_data_retrievals

plot_data_totals <- summ_catches %>%
  select(trap_type, total_lf_catch, total_bycatch) %>%
  pivot_longer(cols = c(total_lf_catch, total_bycatch), 
               names_to = "catch_type", 
               values_to = "total_count")

# Create the plot for retrieval counts
plot_retrievals <- ggplot(plot_data_retrievals, 
                          aes(x = trap_type, y = retrieval_count, fill = catch_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
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

# Create the plot for total counts
plot_catches <- ggplot(plot_data_totals, 
                       aes(x = trap_type, y = total_count, fill = catch_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
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
  )

# Combine the two plots side by side
comb_bar_plots <- plot_retrievals + plot_catches; comb_bar_plots

