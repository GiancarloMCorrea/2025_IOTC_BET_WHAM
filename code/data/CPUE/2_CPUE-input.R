rm(list = ls())

# Read auxiliary functions:
source('code/auxiliary_functions.R')

# Select first year (when model starts):
mod_str_yr = 1975
mod_end_yr = 2023
# First and last quarter:
mod_str_qt = yearqtr2qtr(mod_str_yr, 1, 1950, 13)
mod_end_qt = yearqtr2qtr(mod_end_yr, 4, 1950, 13)

# WHAM input:
cpue_input = matrix(0, ncol = 2, nrow = length(mod_str_qt:mod_end_qt))
cpue_use = matrix(0, ncol = 2, nrow = length(mod_str_qt:mod_end_qt))
cpue_cv = matrix(0, ncol = 2, nrow = length(mod_str_qt:mod_end_qt))

# -------------------------------------------------------------------------

# LL index:
Data79 = read.csv(file.path('data/processed', 'CPUE-LL_unscaled.csv'))
Data79 = Data79 %>% filter(Yr >= mod_str_yr, floor(Yr) <= mod_end_yr)

# Perform scaling:
this_area = 'R1n'
data79 = Data79
data79$pr_7994_m8 = data79$Obs
index = data79$Area==this_area
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$Yr>=1979 & work$Yr<= 1994,'pr_7994_m8']) * betwts[['7994 m8']][this_area]
# calculate var for each region and qter considering the same weighting to the var as to the prior
# var(a/b X)= (a/b)^2 var(x)
data79[index,] = work

# Perform scaling:
this_area = 'R1s'
data79 = Data79
data79$pr_7994_m8 = data79$Obs
index = data79$Area==this_area
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$Yr>=1979 & work$Yr<= 1994,'pr_7994_m8']) * betwts[['7994 m8']][this_area]
# calculate var for each region and qter considering the same weighting to the var as to the prior
# var(a/b X)= (a/b)^2 var(x)
data79[index,] = work

# Perform scaling:
this_area = 'R2'
data79 = Data79
data79$pr_7994_m8 = data79$Obs
index = data79$Area==this_area
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$Yr>=1979 & work$Yr<= 1994,'pr_7994_m8']) * betwts[['7994 m8']][this_area]
# calculate var for each region and qter considering the same weighting to the var as to the prior
# var(a/b X)= (a/b)^2 var(x)
data79[index,] = work

# Perform scaling:
this_area = 'R3'
data79 = Data79
data79$pr_7994_m8 = data79$Obs
index = data79$Area==this_area
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$Yr>=1979 & work$Yr<= 1994,'pr_7994_m8']) * betwts[['7994 m8']][this_area]
# calculate var for each region and qter considering the same weighting to the var as to the prior
# var(a/b X)= (a/b)^2 var(x)
data79[index,] = work

# Aggregate by assessment area:
data <- data79 %>% group_by(qtr) %>% summarize(pr7994_m8_2R=sum(pr_7994_m8)) %>% mutate(cv = 0.2)
data = data %>% arrange(qtr)

# Add CPUE input:
pos_row = match(data$qtr, mod_str_qt:mod_end_qt)
cpue_input[pos_row, 1] = data$pr7994_m8_2R
cpue_use[pos_row, 1] = 1
cpue_cv[pos_row, 1] = 0.2

# -------------------------------------------------------------------------

# LS index:
Data79 = read.csv(file.path('data/processed', 'CPUE-LS_unscaled.csv'))
Data79 = Data79 %>% filter(Yr >= mod_str_yr, Yr <= mod_end_yr)
data = Data79

# Add CPUE input:
pos_row = match(data$qtr, mod_str_qt:mod_end_qt)
cpue_input[pos_row, 2] = data$Obs
cpue_use[pos_row, 2] = 1
cpue_cv[pos_row, 2] = 0.2


# -------------------------------------------------------------------------

# Save data:
save(cpue_input, file = file.path('data/processed', 'CPUE-input.RData'))
save(cpue_use, file = file.path('data/processed', 'CPUE-use.RData'))
save(cpue_cv, file = file.path('data/processed', 'CPUE-cv.RData'))
