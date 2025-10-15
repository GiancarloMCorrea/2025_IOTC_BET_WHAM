rm(list = ls())
require(dplyr)

# Read auxiliary functions:
source('code/auxiliary_functions.R')

# Initial length bins (correct) in IOTC dataset:
L_labels  =  c(Paste("L0",seq(10,98,2)), Paste("L",seq(100,198,2))) 

# Length bins in the WHAM model (4 cm length bin):
L_labels_WHAM  =  c(Paste("L0",seq(10,98,4)), Paste("L",seq(102,198,4)))


# -------------------------------------------------------------------------
# Read original LF data after preprocessing:
data = read.csv(file.path('data/processed', 'LF_grid.csv'))

# Get area information:
data$Area = get_1Aarea_from_lonlat(data$Long, data$Lat)
table(data$Area)

# Filter by year:
data = data %>% filter(Year >= mod_str_yr, Year <= mod_end_yr)

# Filter out messy data:
data_filt = data %>%  ## additional filtering of spurious length data for the assessment - this should be included in the final LF dataset for the assessment. I have not completely reviewed all of these - this is something that should be investigated in the future.
  filter(!(Fleet %in% c('COM') & Gear == 'TROL' & Year > 2021))  %>%
  filter(!(Fleet %in% c('TWN','SYC') & Gear == 'LL')) %>%
  filter(!(Fleet == "IDN" & Gear == "GILL" & Year > 2018)) %>%  # "SF strata to delete" from IOTC-2022-WPTT24(DP)-DATA14-SA_BET-01-summary.xlsx
  filter(!(Fleet == "IDN" & Gear == "TROL" & Year > 2019)) %>%
  filter(!(Fleet == "IDN" & Gear == "HAND" & Year > 2020))  %>%
  filter(!(Fleet == "IDN" & Gear == "LLCO" & Year > 2020))  %>%
  filter(!(Fleet == "COM" & Gear == "HAND" & Year > 2021)) %>%
  filter(!(Fleet == "COM" & Gear == "LLCO" & Year > 2021)) %>%
  filter(!(Fleet == "IRN" & Gear == "GILL" & Year > 2013)) %>%
  filter(!(Fleet == "IRN" & Gear == "PS" & Year > 2017)) %>%
  filter(!(Fleet == "KOR" & Gear == "LL" & Year %in% c(1991:2005))) %>%
  filter(!(Fleet == "LKA" & Gear == "G/L" & Year %in% c(2005:2007))) %>%
  filter(!(Fleet == "LKA" & Gear == "GILL" & Year %in% c(1988:2024))) %>%
  filter(!(Fleet == "LKA" & Gear == "HAND" & Year %in% 2004)) %>%
  filter(!(Fleet == "LKA" & Gear == "HATR" & Year %in% 1999)) %>%
  filter(!(Fleet == "LKA" & Gear == "TROL" & Year %in% c(1997:2003))) %>%
  filter(!(Fleet == "MDV" & Gear == "BB" & Year %in% c(2012:2024))) %>%
  filter(!(Fleet == "MDV" & Gear == "BBOF" & Year %in% c(2021:2024))) %>%
  filter(!(Fleet == "MDV" & Gear == "HAND" & Year %in% 2015)) %>%
  filter(!(Fleet == "MDV" & Gear == "LL" & Year %in% c(2014:2016))) %>%
  filter(!(Fleet == "MDV" & Gear == "TROL" & Year %in% 2015)) %>%
  filter(!(Fleet == "MOZ" & Gear == "HAND" & Year %in% 2015)) %>%
  filter(!(Fleet == "MUS" & Gear == "HATR" & Year %in% 2017)) %>%
  filter(!(Fleet == "MUS" & Gear == "PS" & Year %in% c(1988:2000,2014,2020:2024))) %>%
  filter(!(Fleet == "MUS" & Gear == "PSOB" & Year %in% c(2018:2024))) %>%
  filter(!(Fleet == "SYC" & Year %in% 2024))
  # filter(!(ModelFishery %in% c("FL2") & Year < 2012)) %>%
  # filter(!(ModelFishery %in% c("OT1N","OT2","BB1N"))) %>%
  # dplyr::filter(!(ModelFishery %in% c("LINE2") & Year > 2018)) %>%
  

# Aggregate data:
# Remove Month, SchoolType, Grid:
agg_data = data_filt %>% group_by(Year, Quarter, FisheryCode) %>%
  summarise_at('Quality', list(mean)) %>%
  inner_join(data_filt %>% group_by(Year, Quarter, FisheryCode) %>%
               summarise_at(c('Nfish_samp', L_labels), list(sum)))

# Filter data based on some criteria (original dataset):
# work = filter_LF_1A(agg_data)
work = agg_data

# 1. Do the aggregation for length bins (traditional way):
work1 = work %>%
  dplyr::group_by(FisheryCode,Year) %>% 
  dplyr::summarise_at(L_labels,list(Sum)) %>%
  as.data.frame() %>%
  tidyr::gather(length,total,L010:L198) %>%
  dplyr::mutate(length=as.numeric(substr(length,2,4))) %>%
  dplyr::mutate(length=length-(length-10) %% 4) %>%
  dplyr::mutate(length=ifelse(length<100,Paste("L0",length),Paste("L",length))) %>%
  dplyr::group_by(FisheryCode,Year,length) %>% 
  dplyr::summarise_at("total",list(Sum)) %>% 
  tidyr::spread(length,total,fill=0) %>% 
  as.data.frame()
# 2. Do the aggregation for reporting quality (simple mean)
work2 = work %>%
  dplyr::group_by(FisheryCode,Year) %>%
  summarise(RepQual = mean(Quality, na.rm=TRUE)) # WARNING: there is NAs in quality
# Merge both:
work = left_join(work1, work2)
work = work %>% relocate(RepQual, .after = Year) 

# Fill NAs quality with 6:
work$RepQual[is.na(work$RepQual)] = 6

# Continue:
work = work %>%
  dplyr::mutate(Nsamp = RepQual, .before = 'RepQual') %>%
  dplyr::select(Year, FisheryCode, Nsamp, L010:L198) 		
work[, L_labels_WHAM] = round(work[,L_labels_WHAM],1)

# WHAM format:
lf_input = array(0, dim = c(length(fleet_order), length(mod_str_yr:mod_end_yr), length(L_labels_WHAM)))
lf_samp = matrix(0, ncol = length(fleet_order), nrow = length(mod_str_yr:mod_end_yr))
lf_use = matrix(0, ncol = length(fleet_order), nrow = length(mod_str_yr:mod_end_yr))
for(k in seq_along(fleet_order)) {
  tmp = work %>% filter(FisheryCode == fleet_order[k])
  tmp_mat = as.matrix(tmp[,4:ncol(tmp)])
  tmp_mat_t = t(apply(tmp_mat, 1, function(i) i/sum(i)))
  row_pos = match(tmp$Year, mod_str_yr:mod_end_yr)
  lf_input[k,row_pos,] = tmp_mat_t
  lf_samp[row_pos, k] = tmp$Nsamp
  lf_use[row_pos, k] = 1
}


# -------------------------------------------------------------------------
# Perform some filtering after analyzing the data:

# Remove LF data before yr 250 (too sparse!) for fleets 5, 6 and 7 
pos_rem = mod_str_yr:mod_end_yr < 2008
lf_use[pos_rem, c(5, 6, 7)] = 0
# Do not use year 2020 for PSFS, too weird:
pos_rem = mod_str_yr:mod_end_yr == 2020
lf_use[pos_rem, 2] = 0

# -------------------------------------------------------------------------
# Save WHAM LF input
save(lf_input, file = file.path('data/processed', 'LF_input-year.RData'))
save(lf_samp, file = file.path('data/processed', 'LF_samp-year.RData'))
save(lf_use, file = file.path('data/processed', 'LF_use-year.RData'))
