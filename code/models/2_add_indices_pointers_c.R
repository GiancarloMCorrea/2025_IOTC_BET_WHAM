
# LL selex two blocks, LL CPUE index ------------------------------

input_data$n_indices = 1 # number of CPUEs
# Aggregated indices:
input_data$agg_indices = matrix(cpue_input[,1]*1e+03, ncol = input_data$n_indices, nrow = n_years) # Obs
input_data$index_cv = matrix(cpue_cv[,1], ncol = input_data$n_indices, nrow = n_years) # Obs error
input_data$use_indices = matrix(cpue_use[,1], ncol = input_data$n_indices, nrow = n_years)

# Selex index pointers: 1 = LL, 3 = PSLS
input_data$selblock_pointer_indices = matrix(c( rep(1L, each = length(mod_str_yr:2002)), 
                                                rep(6L, each = length(2003:mod_end_yr)) ), 
                                             ncol = input_data$n_indices, nrow = n_years)

# Selex fleet pointers: 
fleet_order
input_data$selblock_pointer_fleets = matrix(c( rep(1L, each = length(mod_str_yr:2002)), 
                                               rep(6L, each = length(2003:mod_end_yr)),
                                               rep(c(2L, 3L, 4L, 5L, 3L, 2L), each = n_years) ), 
                                            ncol = input_data$n_fleets, nrow = n_years)

# Input selectivity parameters:
input_selex = list(model = c('len-double-normal', 
                             'len-splines',
                             'len-double-normal',
                             'len-logistic',
                             'len-logistic',
                             'len-logistic'),
                   initial_pars = list(c(113, 0, 6.4, 1, -5, -1.1),
                                       c(0.001, 0.9, 0.2, 0.99, 0.65), 
                                       c(45, -3, 7.5, 4, 9, -5),
                                       c(100, 2),
                                       c(100, 6),
                                       c(100, 2)),
                   fix_pars = list(c(2,5), c(1,3), c(2,3,5,6), NULL, 2, NULL),
                   n_selblocks = 6)

# -------------------------------------------------------------------------
# Additional information:
input_data$units_indices = matrix(1L, nrow = n_years, ncol = input_data$n_indices) # 0 = numbers, 1 = biomass
input_data$fracyr_indices = matrix(0.5, ncol = input_data$n_indices, nrow = n_years) # fraction of the year when survey occurs
# Length comps (index):
input_data$use_index_pal = matrix(0, nrow = n_years, ncol = input_data$n_indices) # 1 = fit, 0 = don't fit
# Dont forget to turn off the use of paa (default):
input_data$use_index_paa = matrix(0, nrow = n_years, ncol = input_data$n_indices) # 1 = fit, 0 = don't fit

# WAA pointers:
input_data$waa_pointer_indices = rep(2, times = input_data$n_indices)