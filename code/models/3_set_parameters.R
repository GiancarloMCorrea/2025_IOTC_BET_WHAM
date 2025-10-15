# For selectivity pointers:
# Pointer 1: LL: len-logistic
# Pointer 2: FS and OTHER: len-splines
# Pointer 3: LS and BB: len-double-normal
# Pointer 4: FL: len-logistic
# Pointer 5: LINE: len-logistic

# Create WHAM input: 
input_wham = wham::prepare_wham_input(model_name = "Bet_1",
                                  basic_info = input_data, 
                                  NAA_re = list(N1_model = 1, N1_pars = c(5e+04, 0.1),
                                                #N1_model = 0, N1_pars = c(34600, 19300, 11700, 7400, 4800, 3200, 2100, 1400, 1000, 3200),
                                                recruit_model = 3, recruit_pars = c(h_init, 4e+04), use_steepness = TRUE,
                                                #recruit_model = 2, recruit_pars = 3.5e+04,
                                                sigma = 'rec', cor = 'iid'), # Recruitment parameters
                                  M = list(model = 'Lorenzen', 
                                           initial_means = (5.4/Amax)), # M parameter
                                  selectivity = input_selex, # Selectivity parameters
                                  catchability = list(initial_q = rep(0.1, times = input_data$n_indices),
                                                      q_lower = rep(0, times = input_data$n_indices), 
                                                      q_upper = rep(10, times = input_data$n_indices)), # Catchability parameter
                                  LAA = list(model = 'vB_classic', 
                                             init_vals = c(0.3, 170.8, 30), # k, Linf, Lmin. for age 1 should be around 58
                                             SD_vals = c(7.8, 14.7)), # SD on Lmin, SD on plus group
                                  WAA = list(model = 'Allometric',
                                             init_vals = c(2.217e-05, 3.012)),
                                  maturity = list(model = 'len-logistic',
                                                  init_vals = c(0.3, 110))
) 

# Fix some parameters:
input_wham$par$log_NAA_sigma = log(0.2) # closer to best estimate
# input_wham$map$log_NAA_sigma = factor(NA)
# input_wham$random = NULL
input_wham$map$mean_rec_pars <- factor(c(NA, 1)) # Fix steepness
input_wham$par$log_F1 = log(c(0.02, rep(0.0001, times = 6))) # initial values for F1
# input_wham$map$log_N1_pars = factor(c(1, NA))
# input_wham$map$log_N1_pars = factor(rep(NA, times = length(input_wham$par$log_N1_pars)))
