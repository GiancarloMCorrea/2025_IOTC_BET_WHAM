#  Read data ----
catch_df = read.csv(file.path('data/processed', 'CE_input-year.csv'))
load(file.path('data/processed', 'CPUE-input-year.RData'))
load(file.path('data/processed', 'CPUE-use-year.RData'))
load(file.path('data/processed', 'CPUE-cv-year.RData'))
load(file.path('data/processed', 'LF_input-year.RData'))
load(file.path('data/processed', 'LF_samp-year.RData'))
load(file.path('data/processed', 'LF_use-year.RData'))


# -------------------------------------------------------------------------
# Make input data:
input_data = list()
input_data$ages = 1:10 # ages
input_data$years = mod_str_yr:mod_end_yr # years
input_data$lengths = seq(from = 10, to = 198, by = 4) # length bins
input_data$n_fleets = 7 # number of fleets
n_years = length(input_data$years)
n_ages = length(input_data$ages)
n_lengths = length(input_data$lengths)
# Agg catch:
input_data$agg_catch = as.matrix(catch_df) # Obs
input_data$catch_cv = matrix(0.1, ncol = input_data$n_fleets, nrow = n_years) # Obs error
# Length comps (fishery):
input_data$catch_pal = lf_input # Obs
input_data$catch_NeffL = scales::rescale(lf_samp, to=c(40, 10)) # Obs error, original c(8,2)
input_data$use_catch_pal = lf_use # 1 = fit, 0 = don't fit
# Dont forget to turn off the use of paa (default):
input_data$use_catch_paa = matrix(0, nrow = n_years, ncol = input_data$n_fleets) # 1 = fit, 0 = don't fit
# weight-at-age pointers information:
input_data$waa_pointer_fleets = c(2, 2, 2, 2, 2, 2, 2)
input_data$waa_pointer_totcatch = 2
input_data$waa_pointer_ssb = 1
input_data$waa_pointer_jan1 = 1
# More information:
input_data$fracyr_SSB = matrix(0, ncol = 1, nrow = n_years) # spawning fraction (0 = spawn at beginning of year)
input_data$Fbar_ages = 1:5 # ages to include in mean F calculation 
input_data$bias_correct_process = 1 # do process bias correction, 0 = no, 1 = yes
input_data$bias_correct_observation = 1 # do obs bias correction, 0 = no, 1 = yes 
# Max Age:
Amax = 14.7 # in years
input_data$Lorenzen_Age = 3.75 # in quarters
# Age Nodes:
# input_data$Age_Nodes = c(2, 3, 6, 16, 20)
input_data$Len_Nodes = c(30, 50, 82, 122, 158)
# init values for FMSY:
input_data$FMSY_init = c(rep(0.05, times = 5), 
                         rep(0.1, times = n_years-5)) # Important initial values to get ADREPORT