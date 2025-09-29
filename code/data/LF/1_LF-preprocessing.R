# This script will do the preprocessing of the CE data
rm(list = ls())
require(dplyr)
require(readxl)
require(stringr)
source('code/auxiliary_functions.R')


# -------------------------------------------------------------------------
# Read data:
RawData = read.csv(file.path('data/raw/LF',  'IOTC-DATASETS-SF-BET-1965-2024-MELTED-FORMAT.csv'))
head(RawData)

# Filtering:
Data = RawData %>% 
  dplyr::select(YEAR,MONTH_START,FLEET_CODE,GEAR_CODE,FISHING_GROUND_CODE,SCHOOL_TYPE_CODE,CLASS_LOW,CLASS_HIGH,REPORTING_QUALITY,FISH_COUNT) %>% 
  rename(Year = YEAR, Month=MONTH_START, Fleet=FLEET_CODE, Gear = GEAR_CODE, SchoolType =SCHOOL_TYPE_CODE, Grid=FISHING_GROUND_CODE, LowBin=CLASS_LOW, HighBin=CLASS_HIGH, Quality=REPORTING_QUALITY, Nfish=FISH_COUNT)  
  
dHelper = read.csv(file.path('data/raw', "fleets_model_fleets.csv"))
dHelper = dHelper %>% 
  mutate(Fleet=FLEET, Gear=GEAR_CODE, SchoolType=SCHOOL_TYPE_CODE,FisheryCode=FISHERY) %>% 
  dplyr::select(Fleet,Gear,SchoolType,FisheryCode)
  
# Remove blank spaces in characters:
dHelper$Fleet = str_trim(string = dHelper$Fleet)
dHelper$Gear = str_trim(string = dHelper$Gear)
dHelper$SchoolType = str_trim(string = dHelper$SchoolType)
Data$Fleet = str_trim(string = Data$Fleet)
Data$Gear = str_trim(string = Data$Gear)
Data$SchoolType = str_trim(string = Data$SchoolType)

# Merge:
Data = Data %>% 
  dplyr::filter(!(Gear == 'HOOK' | Gear == 'HATR' | Gear=='PSOB' | (Gear == 'PS' & SchoolType == 'UNCL'))) %>%  # I exclude HATR here but please check again if the HATR LF is good enough now
  dplyr::filter(!(Grid == 'NJA_SYC')) %>%
  dplyr::left_join(dHelper,by=c('Gear', 'Fleet', 'SchoolType')) %>% 
  dplyr::mutate(Quarter = floor((Month-1)/3) + 1)
  
# Check:
table(substring(Data$Grid, 1, 1))
which(is.na(Data$FisheryCode))

# Fill gaps FisheryCode
Data$FisheryCode[which(is.na(Data$FisheryCode))] = 'FL'
which(is.na(Data$FisheryCode))

# Remove samples from entire IO:
Data = Data %>% filter(!(substring(Data$Grid, 1, 1) == 'I'))
table(substring(Data$Grid, 1, 1))

# Continue..
Data = plyr::ddply(Data, "Grid", .fun = function(d) {
  lat = get.lat.from.id(d$Grid[1]);
  long = get.long.from.id(d$Grid[1]);
  d$Lat = lat
  d$Long = long
  return(d)}
)		
  
# Select..
Data = Data %>% dplyr::select(Year,Quarter,Month,Grid,Lat,Long,Fleet,Gear,SchoolType,FisheryCode,LowBin,Quality,Nfish)
  
# Aggregate:
Data = Data %>% 
  dplyr::group_by(Year,Quarter,Month,Grid,Lat,Long,Fleet,Gear,SchoolType,FisheryCode,LowBin) %>% 
  dplyr::summarise(Nfish = sum(Nfish),Quality=mean(Quality)) 
  
# Now aggregate largest and smallest length bin:
out_data = Data %>% 
  dplyr::mutate(LenBin=if_else(LowBin<198,LowBin,198)) %>% # aggregate largest bins
  dplyr::mutate(LenBin=if_else(LenBin>10,LenBin,10)) %>% # aggregate largest bins
  group_by(Year,Quarter,Month,Grid,Lat,Long,Fleet,Gear,SchoolType,FisheryCode,LenBin) %>% 
  dplyr::summarise(Nfish = sum(Nfish),Quality=mean(Quality))
  
# Make sure we have all length bins:
identical(sort(unique(out_data$LenBin)), seq(from = 10, to = 198, by = 2))
  
# Now change the format to have everything in the original format (see irregular code):
out_data = out_data %>% mutate(LenBin = if_else(LenBin<100, paste0('L0', LenBin), paste0('L', LenBin)))
out_data = out_data %>% tidyr::spread(LenBin,Nfish,fill=0)
out_data = out_data %>% mutate(Nfish_samp=rowSums(across(L010:L198)), .before = 'L010')
  
# Save this object for analyses:
write.csv(out_data, file = file.path('data/processed', 'LF_grid.csv'), row.names = FALSE)
