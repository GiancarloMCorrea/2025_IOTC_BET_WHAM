require(readxl)
require(dplyr)
rm(list = ls())

# Read auxiliary functions:
source('code/auxiliary_functions.R')

# -------------------------------------------------------------------------
# LL index:

# Read raw data in:
dat <- read_xlsx(file.path('data/raw/CPUE', 'IOTC_BET_CPUE_DL_2025_06_11.xlsx'))

dat_R1n <- dat[,1:2] %>% mutate(area = 'R1n') %>% na.omit
dat_R1s <- dat[,3:4] %>% mutate(area = 'R1s') %>% na.omit
dat_R2 <- dat[,5:6] %>% mutate(area = 'R2') %>% na.omit
dat_R3 <- dat[,7:8] %>% mutate(area = 'R3') %>% na.omit
colnames(dat_R1n) = c('Yr', 'Obs', 'Area')
colnames(dat_R1s) = c('Yr', 'Obs', 'Area')
colnames(dat_R2) = c('Yr', 'Obs', 'Area')
colnames(dat_R3) = c('Yr', 'Obs', 'Area')

# Merge:
all_data = rbind(dat_R1n, dat_R1s, dat_R2, dat_R3)
all_data = all_data %>% mutate(Quarter = (Yr-floor(Yr))*4+1,
                               qtr = yearqtr2qtr(floor(Yr), Quarter, 1950, 13)) 

# Save this object for analyses:
write.csv(all_data, file = file.path('data/processed', 'CPUE-LL_unscaled.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# LS index:

# Read raw data in:
dat_ls <- read.csv(file.path('data/raw/CPUE', 'long_index.csv'))
dat_ls <- dat_ls %>% select(year, quarter, est, cv)
colnames(dat_ls) = c('Yr', 'Quarter', 'Obs', 'CV')

# Create qtr column and mean CV = 0.2:
all_data = dat_ls %>% mutate(qtr = yearqtr2qtr(Yr, Quarter, 1950, 13)) 
all_data$CV = all_data$CV/mean(all_data$CV)*0.2

# Save this object for analyses:
write.csv(all_data, file = file.path('data/processed', 'CPUE-LS_unscaled.csv'), row.names = FALSE)
