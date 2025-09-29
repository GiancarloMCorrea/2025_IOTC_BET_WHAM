# This script will do the preprocessing of the CE data
rm(list = ls())
require(dplyr)
require(stringr)
require(readxl)
source('code/auxiliary_functions.R')

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Data come from https://iotc.org/WPTT/27DP/Data/03-CE
RawData = read.csv(file.path('data/raw/CE', "IOTC-WPTT27-DP-BET-CEraised_1950_2023.csv"))

Data = RawData %>% 
  filter(Species == 'BET') %>%
  select(Year,Month,Fleet,Gear,SchoolType,Grid,NCmtFish) %>% 
  rename(Catch=NCmtFish) 

# Fishery mapping data provided by Genevieve:
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
  dplyr::left_join(dHelper,by=c('Gear', 'Fleet', 'SchoolType')) %>% 
  dplyr::mutate(Grid=as.character(Grid),Quarter = floor((Month-1)/3) + 1)

# Check:
table(substring(Data$Grid, 1, 1))
which(is.na(Data$FisheryCode))

# Check again:
which(is.na(Data$FisheryCode))

# Add lon lat info:
Data = plyr::ddply(Data,"Grid",.fun = function(d) {
  lat = get.lat.from.id(d$Grid[1]);
  long = get.long.from.id(d$Grid[1]);
  d$lat =lat
  d$long = long
  return(d)}
)

# Save this object for analyses:
write.csv(Data, file = file.path('data/processed', 'CE_grid.csv'), row.names = FALSE)
