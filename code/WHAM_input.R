rm(list = ls())
# remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='tuna')
# devtools::install_local(path = 'C:/Use/GitHub/wham', force = TRUE)
library(readxl)
library(wham)
library(ggplot2)
library(dplyr)
source('code/auxiliary_functions.R')

#  Read data ----
catch_df = read.csv(file.path('data/processed', 'CE_input.csv'))
load(file.path('data/processed', 'CPUE-input.RData'))
load(file.path('data/processed', 'CPUE-use.RData'))
load(file.path('data/processed', 'CPUE-cv.RData'))
load(file.path('data/processed', 'LF_input.RData'))
load(file.path('data/processed', 'LF_samp.RData'))
load(file.path('data/processed', 'LF_use.RData'))

# Make input data:
input_data = list()
input_data$ages = 1:40 # ages
input_data$years = mod_str_qt:mod_end_qt # years
input_data$lengths = seq(from = 10, to = 198, by = 4) # length bins
input_data$n_fleets = 7 # number of fleets
input_data$n_indices = 1 # number of CPUEs
n_years = length(input_data$years)
n_ages = length(input_data$ages)
n_lengths = length(input_data$lengths)
# Agg catch:
input_data$agg_catch = as.matrix(catch_df) # Obs
input_data$catch_cv = matrix(0.05, ncol = input_data$n_fleets, nrow = n_years) # Obs error
# Length comps (fishery):
input_data$catch_pal = lf_input # Obs
input_data$catch_NeffL = scales::rescale(lf_samp, to=c(6, 2)) # Obs error
input_data$use_catch_pal = lf_use # 1 = fit, 0 = don't fit
# Agg index:
input_data$agg_indices = matrix(cpue_input[,1]*1e+06, ncol = input_data$n_indices, nrow = n_years) # Obs
input_data$index_cv = matrix(cpue_cv[,1], ncol = input_data$n_indices, nrow = n_years) # Obs error
input_data$use_indices = matrix(cpue_use[,1], ncol = input_data$n_indices, nrow = n_years)
# Additional information:
input_data$units_indices = matrix(1L, nrow = n_years, ncol = input_data$n_indices) # 0 = numbers, 1 = biomass
input_data$fracyr_indices = matrix(0.5, ncol = input_data$n_indices, nrow = n_years) # fraction of the year when survey occurs
# Length comps (index):
input_data$use_index_pal = matrix(0, nrow = n_years, ncol = input_data$n_indices) # 1 = fit, 0 = don't fit
# Dont forget to turn off the use of paa (default):
input_data$use_catch_paa = matrix(0, nrow = n_years, ncol = input_data$n_fleets) # 1 = fit, 0 = don't fit
input_data$use_index_paa = matrix(0, nrow = n_years, ncol = input_data$n_indices) # 1 = fit, 0 = don't fit
# Selex pointers:
fleet_order
input_data$selblock_pointer_fleets = matrix(rep(c(1L, 2L, 3L, 4L, 5L, 3L, 2L), each = n_years), ncol = input_data$n_fleets, nrow = n_years)
input_data$selblock_pointer_indices = matrix(rep(c(1L), each = n_years), ncol = input_data$n_indices, nrow = n_years)
# weight-at-age pointers information:
input_data$waa_pointer_fleets = c(2, 2, 2, 2, 2, 2, 2)
input_data$waa_pointer_indices = 2
input_data$waa_pointer_totcatch = 2
input_data$waa_pointer_ssb = 1
input_data$waa_pointer_jan1 = 1
# More information:
input_data$fracyr_SSB = matrix(0, ncol = 1, nrow = n_years) # spawning fraction (0 = spawn at beginning of year)
input_data$Fbar_ages = 1:38 # ages to include in mean F calculation 
input_data$bias_correct_process = 1 # do process bias correction, 0 = no, 1 = yes
input_data$bias_correct_observation = 1 # do obs bias correction, 0 = no, 1 = yes 
# Max Age:
Amax = 14.7 # in years
input_data$Lorenzen_Age = 15 # in quarters
# Age Nodes:
# input_data$Age_Nodes = c(2, 3, 6, 16, 20)
input_data$Len_Nodes = c(30, 50, 82, 122, 158)

# For selectivity pointers:
# Pointer 1: LL: len-logistic
# Pointer 2: FS and OTHER: len-splines
# Pointer 3: LS and BB: len-double-normal
# Pointer 4: FL: len-logistic
# Pointer 5: LINE: len-logistic

# Create WHAM input: 
input1 = wham::prepare_wham_input(model_name = "Bet_1",
                                  basic_info = input_data, 
                                  NAA_re = list(N1_model = 1, N1_pars = c(1.4e+04, 0.03),
                                                # N1_model = 0, N1_pars = c(11555,	9569,	8016,	6792,	5822,	5048, 
                                                #               4427, 3927, 3523, 3198,	2936, 2726, 2560, 
                                                #               2403, 2252, 2109, 1974, 1847, 1728, 1618, 1515, 
                                                #               1419, 1329, 1245, 1166, 1092, 1023,	959, 898, 841, 788, 739, 
                                                #               692,	648, 608, 569, 533, 500, 468, 6964),
                                                recruit_model = 3, recruit_pars = c(0.8, 1.1e+04),
                                                use_steepness = TRUE,
                                                sigma = 'rec', cor = 'iid'), # Recruitment parameters
                                  M = list(model = 'Lorenzen', 
                                           initial_means = (5.4/Amax)/4), # M parameter
                                  selectivity = list(model = c('logistic', 
                                                               'len-splines',
                                                               'double-normal',
                                                               'logistic',
                                                               'logistic'),
                                                     initial_pars = list(c(14, 1),
                                                                         c(0.001, 0.9, 0.2, 0.99, 0.65), 
                                                                         c(2.5, -5, 0, 1, -5, -5),
                                                                         c(16, 2),
                                                                         c(15, 2)),
                                                     fix_pars = list(NULL, c(1, 3, 5), c(2,5,6), 2, 2),
                                                     n_selblocks = 5), # Selectivity parameter
                                  catchability = list(initial_q = c(0.33),
                                                      q_lower = 0, q_upper = 10), # Catchability parameter
                                  LAA = list(model = 'vB_classic', 
                                             init_vals = c(0.3/4, 170.8, 30), # k, Linf, Lmin
                                             SD_vals = c(3, 14.7)), # SD on Lmin, SD on plus group
                                  WAA = list(model = 'Allometric',
                                             init_vals = c(2.217e-05, 3.012)),
                                  maturity = list(model = 'len-logistic',
                                                  init_vals = c(0.3, 110))
) 

# Fix some parameters:
input1$par$log_NAA_sigma = log(0.55)
input1$map$log_NAA_sigma = factor(NA)
input1$random = NULL
input1$map$mean_rec_pars <- factor(c(NA, 1)) # Fix steepness
input1$par$log_F1 = log(c(0.05, rep(0.001, times = 6)))
# input1$map$log_N1_pars = factor(rep(NA, times = length(input1$par$log_N1_pars)))


# Run model (do not fit):
fit1 = wham::fit_wham(input = input1, do.retro = FALSE, do.osa = FALSE, 
                           do.sdrep = FALSE, do.fit = FALSE)
fit1$rep[grep('nll',names(fit1$rep))] %>% lapply(sum) %>% unlist
plot(fit1$rep$SSB, type = 'l')
plot(fit1$rep$NAA[1,], type = 'l')

plot(fit1$rep$selAA[[1]][1,], type = 'b')
plot(fit1$rep$selAA[[2]][1,], type = 'b')
plot(fit1$rep$selAA[[3]][1,], type = 'b')
plot(fit1$rep$selAA[[4]][1,], type = 'b')
plot(fit1$rep$selAA[[5]][1,], type = 'b')

plot(fit1$rep$MAA[1,])
plot(fit1$rep$jan1LAA[1,])
matplot(fit1$rep$jan1_phi_mat[,,1], type = 'l')
matplot(fit1$rep$catch_phi_mat[,,1], type = 'l')
plot(fit1$rep$pred_waa[1,1,], type = 'b')
lines(fit1$rep$pred_waa[2,1,], type = 'l', col = 2)
plot(fit1$rep$mat_at_len[1,], type = 'l')
plot(input1$data$lengths, fit1$rep$pred_catch_pal[180, 1, ], type = 'l')
plot(input1$data$lengths, fit1$rep$pred_catch_pal[180, 2, ], type = 'l')
plot(input1$data$lengths, fit1$rep$pred_catch_pal[180, 3, ], type = 'l')

# Run model and fit:
fit1 = wham::fit_wham(input = input1, do.retro = FALSE, do.osa = FALSE, 
                           do.sdrep = TRUE, n.newton = 0)
max(abs(fit1$final_gradient))
# saveRDS(fit1, file = "models/fit1.RDS")
# fit1 = readRDS("models/fit1.RDS")

save_dir = getwd()
save_plots = 'figures/fit1'
setwd(save_plots)
plot_wham_output(mod = fit1, out.type = 'html')
setwd(save_dir)

plot(fit1$rep$NAA[1, ])
NAAvec = fit1$rep$NAA[1, ]
log_N1pars = fit1$opt$par[names(fit1$opt$par) == "log_N1_pars"]
FAAtot = fit1$rep$FAA_tot[1, ]
MAAvec = fit1$rep$MAA[1,]

a = 40
NAAvec[a-1]*exp(-MAAvec[a] - exp(log_N1pars[2]) * FAAtot[a]/FAAtot[fit1$env$data$which_F_age[1]])
NAAvec
NAAvec[a-1]/(1 + exp(-MAAvec[a] - exp(log_N1pars[2]) * FAAtot[a]/FAAtot[fit1$env$data$which_F_age[1]]))


