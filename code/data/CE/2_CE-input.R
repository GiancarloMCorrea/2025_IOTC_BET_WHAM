rm(list = ls())
require(tidyr)

# Read auxiliary functions:
source('code/auxiliary_functions.R')

# Read raw CE data after preprocessing:
data = read.csv(file.path('data/processed', 'CE_grid.csv'))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Get area information:
data$Area = get_1Aarea_from_lonlat(data$long, data$lat)
table(data$Area)

# Filter by year:
data = data %>% filter(Year >= mod_str_yr, Year <= mod_end_yr)

# Aggregate:
work = data	%>% 
  group_by(Year, Quarter, FisheryCode) %>% 
  summarise(Catch = sum(Catch)) %>% as.data.frame() %>%	
  mutate(qtr = yearqtr2qtr(Year, Quarter, 1950, 13)) %>%
  select(qtr, FisheryCode, Catch)

# Format for WHAM:
ce_df = pivot_wider(work, names_from = FisheryCode, values_from = Catch, values_fill = 0)
ce_df = ce_df %>% arrange(qtr)
# Make sure you have all the qtrs:
identical(as.integer(ce_df$qtr), mod_str_qt:mod_end_qt)
# Order columns:
ce_df = ce_df %>% select(-qtr)
ce_df = ce_df[, fleet_order]

# Save WHAM catch input
write.csv(ce_df, file = file.path('data/processed', 'CE_input.csv'), row.names = FALSE)
