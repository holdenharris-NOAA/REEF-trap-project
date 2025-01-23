
## -----------------------------------------------------------------------------
## Trap retrieval analyses

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
