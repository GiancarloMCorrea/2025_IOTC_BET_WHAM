rm(list = ls())

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

# Aggregate data:
# Remove Month, SchoolType, Grid:
agg_data = data %>% group_by(Year, Quarter, FisheryCode) %>%
  summarise_at('Quality', list(mean)) %>%
  inner_join(data %>% group_by(Year, Quarter, FisheryCode) %>%
               summarise_at(c('Nfish_samp', L_labels), list(sum)))

# Filter data based on some criteria (original dataset):
# work = filter_LF_1A(agg_data)
work = agg_data

# 1. Do the aggregation for length bins (traditional way):
work1 = work %>%
  dplyr::group_by(FisheryCode,Year,Quarter) %>% 
  dplyr::summarise_at(L_labels,list(Sum)) %>%
  as.data.frame() %>%
  tidyr::gather(length,total,L010:L198) %>%
  dplyr::mutate(length=as.numeric(substr(length,2,4))) %>%
  dplyr::mutate(length=length-(length-10) %% 4) %>%
  dplyr::mutate(length=ifelse(length<100,Paste("L0",length),Paste("L",length))) %>%
  dplyr::group_by(FisheryCode,Year,Quarter,length) %>% 
  dplyr::summarise_at("total",list(Sum)) %>% 
  tidyr::spread(length,total,fill=0) %>% 
  as.data.frame()
# 2. Do the aggregation for reporting quality (simple mean)
work2 = work %>%
  dplyr::group_by(FisheryCode,Year,Quarter) %>%
  summarise(RepQual = mean(Quality, na.rm=TRUE)) # WARNING: there is NAs in quality
# Merge both:
work = left_join(work1, work2)
work = work %>% relocate(RepQual, .after = Quarter) 

# Fill NAs quality with 6:
work$RepQual[is.na(work$RepQual)] = 6

# Continue:
work = work %>%
  dplyr::mutate(Yr = yearqtr2qtr(Year,Quarter,1950,13), Nsamp = RepQual, .before = 'RepQual') %>%
  dplyr::select(Yr, FisheryCode, Nsamp, L010:L198) 		
work[, L_labels_WHAM] = round(work[,L_labels_WHAM],1)

# WHAM format:
lf_input = array(0, dim = c(length(fleet_order), length(mod_str_qt:mod_end_qt), length(L_labels_WHAM)))
lf_samp = matrix(0, ncol = length(fleet_order), nrow = length(mod_str_qt:mod_end_qt))
lf_use = matrix(0, ncol = length(fleet_order), nrow = length(mod_str_qt:mod_end_qt))
for(k in seq_along(fleet_order)) {
  tmp = work %>% filter(FisheryCode == fleet_order[k])
  tmp_mat = as.matrix(tmp[,4:ncol(tmp)])
  tmp_mat_t = t(apply(tmp_mat, 1, function(i) i/sum(i)))
  row_pos = match(tmp$Yr, mod_str_qt:mod_end_qt)
  lf_input[k,row_pos,] = tmp_mat_t
  lf_samp[row_pos, k] = tmp$Nsamp
  lf_use[row_pos, k] = 1
}

# Perform some filtering:
# Remove LF data before yr 250 (too sparse!) for fleets 5, 6 and 7 
pos_rem = mod_str_qt:mod_end_qt < 250
lf_use[pos_rem, c(5, 6, 7)] = 0

# Save SS catch input
save(lf_input, file = file.path('data/processed', 'LF_input.RData'))
save(lf_samp, file = file.path('data/processed', 'LF_samp.RData'))
save(lf_use, file = file.path('data/processed', 'LF_use.RData'))
