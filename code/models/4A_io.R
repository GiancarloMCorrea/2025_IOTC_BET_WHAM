rm(list = ls())

# Sharepoint path:
source('sharepoint_path.R')
source('code/auxiliary_functions.R')

# Spatial config
spat_config = '4A_io'

# SS base files path (in working folder):
SS_base = file.path('models/base', spat_config)

# SS configuration path (in working folder):
SS_config = file.path('models/configurations', spat_config)

# SS input data path (in working folder):
SS_data = file.path('data/ss3_inputs', spat_config)

# Specify if you just want to create folders with SS3 inputs, or 
# also run SS3 models and make plots
# Consider that running all models and making plots may considerably increase the computational time 
run_model = FALSE
make_plots = FALSE

# -------------------------------------------------------------------------

# Read base SS inputs
base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))


# -------------------------------------------------------------------------
# Start implementing configurations ---------------------------------------

# 1_BaseCase

# Temporary files:
dat_tmp = base_dat
ctl_tmp = base_ctl
fore_tmp = base_fore
start_tmp = base_start

# Config def:
config_name = '1_BaseCase'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}

# -------------------------------------------------------------------------

# 2_updateCatch

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss_new'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '2_updateCatch'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Assign fleet names
fish_names = get_fisheries(spat_config)$fleet_name
dat_tmp$fleetinfo$fleetname <- c(fish_names, paste0('CPUE_', fish_names[c(7,10,11,13)]))
# Updated catch data frame:
catch_df = read.csv(file.path(shrpoint_path, SS_data, 'catch.csv'))
dat_tmp$catch = catch_df
dat_tmp$endyr = 308 # last year = 2023
# Change also forecast file:
fore_tmp$Bmark_years<- c(308,308,301,308,301,308,296,308,13,308)
fore_tmp$Fcast_years <- c(308,308,308,308,13,308) 
fore_tmp$FirstYear_for_caps_and_allocations <- 309
fore_tmp$Ydecl <- 308
fore_tmp$Yinit <- 308

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 3_updateCPUE

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '3_updateCPUE'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

#update cpue
cpue_df = read.csv(file.path(shrpoint_path, SS_data, 'cpue-ll.csv'))
cpue_df$se_log = 0.2 # assuming cv = 0.2
dat_tmp$CPUE <- cpue_df

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}

# -------------------------------------------------------------------------

# 4_updateLength

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '4_updateLength'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

#update length
length_df = read.csv(file.path(shrpoint_path, SS_data, 'size-original.csv'))
dat_tmp$lencomp = length_df
dat_tmp$lencomp$Nsamp = 5 # assume Nsamp = 5 as in 2021

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 5_updateWarnings

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '5_updateWarnings'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# fleets info: surveytime read as: 1 normally is -1 for fishing fleet:
dat_tmp$fleetinfo$surveytiming[1:dat_tmp$Nfleet] <- -1
# Adjustment: reset upper end of F_reporting_ages to be nages-2 
start_tmp$F_age_range[2] <- dat_tmp$Nages-2
# Block:1 2 ends in: 336 after retroyr+1:  305
ctl_tmp$Block_Design[[1]][4] <- dat_tmp$endyr+1
ctl_tmp$Block_Design[[4]][4] <- dat_tmp$endyr+1
# 1st iteration warning: catch logL > 50% total logL; check configuration; suggest start with larger R0
ctl_tmp$SR_parms["SR_LN(R0)","INIT"] <- 12 
# wanning:Note 2 Information:  N parameters that are on or within 1% of min-max bound: 1; check results, variance may be suspect
ctl_tmp$age_selex_parms["AgeSel_P_1_OT_4(14)","LO"] <- 0

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 6_updateM

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '6_updateM'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Update M:
ctl_tmp$natM_type <- 2
ctl_tmp$Lorenzen_refage <- 4.07*4 #fully mature
ctl_tmp$natM <- NULL
ctl_tmp$MG_parms <- ctl_tmp$MG_parms %>% add_row(ctl_tmp$MG_parms[1,], .before=1)
row.names(ctl_tmp$MG_parms[1,]) <-c("NatM_Lorenzen_Fem_GP_1")
ctl_tmp$MG_parms[1,1:7] <- c(0.1, 0.6, 0.462, 0.462, 0, 0, -2)

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 7_updateGrowth

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '7_updateGrowth'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Update growth:
dat_tmp$Nages <- 40
start_tmp$F_age_range[2] <- dat_tmp$Nages - 2
# extendeing maturity at age until age 40:
extMat<- c(ctl_tmp$Age_Maturity, rep(1, dat_tmp$Nages-28))
names(extMat) <- paste0("Age_", 0:dat_tmp$Nages)
extMat <- as.data.frame(extMat)
ctl_tmp$Age_Maturity <- extMat
# Growth settings:
ctl_tmp$Growth_Age_for_L2 <- 999
ctl_tmp$MG_parms["L_at_Amax_Fem_GP_1","INIT"] <- 167.47
ctl_tmp$MG_parms["L_at_Amin_Fem_GP_1","INIT"] <- 29.9463
ctl_tmp$MG_parms["VonBert_K_Fem_GP_1","INIT"] <- k_vec_Farley[1]
ctl_tmp$MG_parms["Age_K_2_Fem_GP_1","INIT"] <- k_vec_Farley[2]
ctl_tmp$MG_parms["Age_K_3_Fem_GP_1","INIT"] <- k_vec_Farley[3]
ctl_tmp$MG_parms["Age_K_4_Fem_GP_1","INIT"] <- k_vec_Farley[4]
ctl_tmp$MG_parms["Age_K_5_Fem_GP_1","INIT"] <- k_vec_Farley[5]
ctl_tmp$MG_parms["Age_K_6_Fem_GP_1","INIT"] <- k_vec_Farley[6]
ctl_tmp$MG_parms["Age_K_7_Fem_GP_1","INIT"] <- k_vec_Farley[7]
ctl_tmp$MG_parms["Age_K_8_Fem_GP_1","INIT"] <- k_vec_Farley[8]
ctl_tmp$MG_parms["Age_K_9_Fem_GP_1","INIT"] <- k_vec_Farley[9]
ctl_tmp$MG_parms["Age_K_10_Fem_GP_1","INIT"] <- k_vec_Farley[10]
ctl_tmp$MG_parms["Age_K_11_Fem_GP_1","INIT"] <- k_vec_Farley[11]
ctl_tmp$MG_parms["Age_K_12_Fem_GP_1","INIT"] <- k_vec_Farley[12]
ctl_tmp$MG_parms["Age_K_13_Fem_GP_1","INIT"] <- k_vec_Farley[13]

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 8_updateGrowthTagging

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '8_updateGrowthTagging'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Update tagging data as well:
tag_rel_farley = read.csv(file.path(shrpoint_path, SS_data, 'tag-release.csv'))
tag_rec_farley = read.csv(file.path(shrpoint_path, SS_data, 'tag-recapture.csv'))
names(tag_rel_farley) = names(dat_tmp$tag_releases)
names(tag_rec_farley) = names(dat_tmp$tag_recaps)
dat_tmp$tag_releases = tag_rel_farley
dat_tmp$tag_recaps = tag_rec_farley
dat_tmp$N_tag_groups = max(dat_tmp$tag_releases$TG)
dat_tmp$N_recap_events = nrow(dat_tmp$tag_recaps)
ctl_tmp$N_tag_groups <- max(dat_tmp$tag_releases$TG)
ctl_tmp$TG_Loss_init<- ctl_tmp$TG_Loss_init[1:ctl_tmp$N_tag_groups,]
ctl_tmp$TG_Loss_chronic<- ctl_tmp$TG_Loss_chronic[1:ctl_tmp$N_tag_groups,]
ctl_tmp$TG_overdispersion<- ctl_tmp$TG_overdispersion[1:ctl_tmp$N_tag_groups,]

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 9_updateMaturity

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '9_updateMaturity'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Update maturity length based:
ctl_tmp$maturity_option <- 1
ctl_tmp$First_Mature_Age <- 1
ctl_tmp$Age_Maturity <- NULL
ctl_tmp$MG_parms["Mat50%_Fem_GP_1","LO"] <- 50
ctl_tmp$MG_parms["Mat50%_Fem_GP_1","HI"] <- 150
ctl_tmp$MG_parms["Mat50%_Fem_GP_1","INIT"] <- 101.7
ctl_tmp$MG_parms["Mat50%_Fem_GP_1","PRIOR"] <- 101.7
ctl_tmp$MG_parms["Mat_slope_Fem_GP_1","LO"] <- -1
ctl_tmp$MG_parms["Mat_slope_Fem_GP_1","HI"] <- 0
ctl_tmp$MG_parms["Mat_slope_Fem_GP_1","INIT"] <- -0.09
ctl_tmp$MG_parms["Mat_slope_Fem_GP_1","PRIOR"] <- -0.09

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 10_updatePSSelectivity

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '10_updatePSSelectivity'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Update selectivity:
ctl_tmp$size_selex_parms["SizeSel_Spline_Code_FS_1b(6)",1:3] <- c(0,2,2)
ctl_tmp$size_selex_parms["SizeSel_Spline_Code_LS_1b(8)",1:3] <- c(0,2,2)

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model (this is mandatory to estimate nodes):
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-maxfn 0 -phase 50 -nohess')

# Now update nodes:
new_ctltmp = SS_readctl(file = file.path(tmp_dir, 'control.ss_new'), datlist = dat_tmp)
# Fishery 6 and 8:
ctl_tmp$size_selex_parms["SizeSel_Spline_Code_FS_1b(6)",1:3] <- c(0,2,0)
ctl_tmp$size_selex_parms["SizeSel_Spline_Code_LS_1b(8)",1:3] <- c(0,2,0)
# replace parameters for both fisheries:
ctl_tmp$size_selex_parms[c(2:13, 15:26), ] = new_ctltmp$size_selex_parms[c(2:13, 15:26), ]

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess', skipfinished = FALSE)
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 11_updateBoundaries

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss_new'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '11_updateBoundaries'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Boundaries:
idx <- 1:length(ctl_tmp$age_selex_parms$LO)
for(i in idx){
  pr <- ctl_tmp$age_selex_parms[i,3]
  sd <- abs(0.2*pr)
  ctl_tmp$age_selex_parms[i,c(1:5)] <- c(min(pr-4*sd,pr+4*sd),max(pr-4*sd,pr+4*sd),pr,pr,sd)
}
idxRep <- grep(paste0("GI_1a", collapse="|"), row.names(ctl_tmp$age_selex_parms), value=FALSE)
ctl_tmp$age_selex_parms[idxRep[3],1:2] <- c(-2,2)

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 12_updateRecDevs

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '12_updateRecDevs'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Recdevs:
ctl_tmp$MG_parms["RecrDist_GP_1_area_4_month_1",11] <- 300
ctl_tmp$MainRdevYrLast <- 300
ctl_tmp$last_yr_fullbias_adj <-303
ctl_tmp$first_recent_yr_nobias_adj <- 308

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 13_addRepQual

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '13_addRepQual'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Use RQ (2 or 5, based on RQ):
length_df = read.csv(file.path(shrpoint_path, SS_data, 'size-original.csv'))
dat_tmp$lencomp = length_df
dat_tmp$lencomp$Nsamp = ifelse(length_df$Nsamp <= 2, 5, 2)

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 14_useCPW5x5

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '14_useCPW5x5'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Use cwp5x5 regular grid
length_df = read.csv(file.path(shrpoint_path, SS_data, 'size-cwp55.csv'))
dat_tmp$lencomp = length_df
dat_tmp$lencomp$Nsamp = 5 # use 5 Nsamp

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 15_CWP5x5RepQual

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '15_CWP5x5RepQual'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Use cwp5x5 and repquality
length_df = read.csv(file.path(shrpoint_path, SS_data, 'size-cwp55.csv'))
dat_tmp$lencomp = length_df
dat_tmp$lencomp$Nsamp = ifelse(length_df$Nsamp <= 2, 5, 2)
# Change some selex params:
idxRep <- grep(paste0("GI_1a", collapse="|"), row.names(ctl_tmp$age_selex_parms), value=FALSE)
ctl_tmp$age_selex_parms[idxRep[3],1:2] <- c(-2.5,2)

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 16_freeLL3par2

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '16_freeLL3par2'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Free parameter:
idxRep <- grep(paste0("LL_3",collapse="|"), row.names(ctl_tmp$age_selex_parms), value=FALSE)
ctl_tmp$age_selex_parms[idxRep[2],7] <- 3

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 17_OneBlock_LLsel
# This is RM1: bias correction ramp implemented

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss_new'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '17_OneBlock_LLsel'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Bias ramp:
ctl_tmp$last_early_yr_nobias_adj  <- 69.5
ctl_tmp$first_yr_fullbias_adj <- 166.2
ctl_tmp$last_yr_fullbias_adj <- 304.4
ctl_tmp$first_recent_yr_nobias_adj <- 308.7
ctl_tmp$max_bias_adj <- 0.669

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}

# -------------------------------------------------------------------------

# 18_TwoBlock_LLsel
# This is RM2

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss_new'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def:
config_name = '18_TwoBlock_LLsel'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
tmp_dir_rm2 = tmp_dir # save for remaining configs
dir.create(tmp_dir, showWarnings = FALSE)

# Modify data file
dat_tmp$Nfleet = dat_tmp$Nfleet + 3 # fisheries
dat_tmp$Nfleets = dat_tmp$Nfleet + 4 # fisheries and indices
base_LL_names = dat_tmp$fleetinfo$fleetname[c(7, 10, 13)]
dat_tmp$fleetinfo$fleetname[c(7, 10, 13)] = paste0(base_LL_names, '_P2000')
dat_tmp$fleetinfo = dat_tmp$fleetinfo %>% add_row(data.frame(type = 1, surveytiming = -1, area = c(1,2,4), 
                                                  units = 1, need_catch_mult = 0, 
                                                  fleetname = paste0(base_LL_names, '_A2000')), .after = 21)
dat_tmp$fleetnames = dat_tmp$fleetinfo$fleetname
# start from qtr = 213 (2000) to change fleet
dat_tmp$catch = dat_tmp$catch %>% mutate(fleet = if_else(fleet == 7 & year >= 213, 22, fleet)) # LL1b
dat_tmp$catch = dat_tmp$catch %>% mutate(fleet = if_else(fleet == 10 & year >= 213, 23, fleet)) # LL2
dat_tmp$catch = dat_tmp$catch %>% mutate(fleet = if_else(fleet == 13 & year >= 213, 24, fleet)) # LL4
dat_tmp$CPUEinfo = dat_tmp$CPUEinfo %>% add_row(data.frame(fleet = 22:24, units = 1, errtype = 0, SD_report = 0), .after = 21)
dat_tmp$CPUE$index = dat_tmp$CPUE$index + 3
dat_tmp$len_info = dat_tmp$len_info %>% add_row(dat_tmp$len_info[c(7, 10, 13), ], .after = 21)
dat_tmp$lencomp = dat_tmp$lencomp %>% mutate(fleet = if_else(fleet == 7 & year >= 213, 22, fleet)) # LL1b
dat_tmp$lencomp = dat_tmp$lencomp %>% mutate(fleet = if_else(fleet == 10 & year >= 213, 23, fleet)) # LL2
dat_tmp$lencomp = dat_tmp$lencomp %>% mutate(fleet = if_else(fleet == 13 & year >= 213, 24, fleet)) # LL4
# Update Nsamp = 5 for all LL (see all fleet in the next lines)
dat_tmp$lencomp$Nsamp[dat_tmp$lencomp$fleet %in% c(3,7,10,11,13)] = 5
dat_tmp$tag_recaps = dat_tmp$tag_recaps %>% mutate(fleet = if_else(fleet == 7 & year >= 213, 22, fleet)) # LL1b
dat_tmp$tag_recaps = dat_tmp$tag_recaps %>% mutate(fleet = if_else(fleet == 10 & year >= 213, 23, fleet)) # LL2
dat_tmp$tag_recaps = dat_tmp$tag_recaps %>% mutate(fleet = if_else(fleet == 13 & year >= 213, 24, fleet)) # LL4

# Modify control file
ctl_tmp$Block_Design[[1]][4] = 349
ctl_tmp$Block_Design[[4]][4] = 349
ctl_tmp$Q_options$fleet = ctl_tmp$Q_options$fleet + 3
ctl_tmp$Q_options$link_info[2:4] = ctl_tmp$Q_options$link_info[2:4] + 3
ctl_tmp$size_selex_types = ctl_tmp$size_selex_types %>% add_row(ctl_tmp$size_selex_types[c(7, 10, 13), ], .after = 21)
ctl_tmp$age_selex_types = ctl_tmp$age_selex_types %>% add_row(ctl_tmp$age_selex_types[c(7, 10, 13), ], .after = 21)
ctl_tmp$age_selex_types$Pattern[c(7,13)] = 20 # double normal

# First, move LL age selex logistic to the end (after 2000)
ctl_tmp$age_selex_parms = ctl_tmp$age_selex_parms %>% add_row(ctl_tmp$age_selex_parms[c(23:24,31:32,41:42), ])
# Add selex double normal for LL 1b before 2000
ctl_tmp$age_selex_parms = ctl_tmp$age_selex_parms %>% slice(-c(23,24)) # remove old 
tmp_sel_pars = data.frame(LO = c(2,-2,-8,-20,-12,-32), HI = c(19,0,0,0,-1,-3),
                          INIT = c(9.2,-0.85,-3,-10,-6,-15.6), 
                          PRIOR = c(9.2,-0.85,-5.6,-19,-6,-15.6),
                          PR_SD = c(1.8,0.17,1.12,3.8,1.2,3.12),
                          PR_type = 6, PHASE = c(3,-5,4,4,-5,5),
                          env_varlink=0, dev_link=0, dev_minyr=0,dev_maxyr=0,
                          dev_PH=0,Block=0,Block_Fxn=0)
colnames(tmp_sel_pars) = colnames(ctl_tmp$age_selex_parms)
ctl_tmp$age_selex_parms = ctl_tmp$age_selex_parms %>% add_row(tmp_sel_pars, .after = 22)
# Add selex double normal for LL 4 before 2000
ctl_tmp$age_selex_parms = ctl_tmp$age_selex_parms %>% slice(-c(45,46)) # remove old 
tmp_sel_pars = data.frame(LO = c(1,-2,-40,-20,-12,-25), HI = c(15,0,0,0,-1,-3),
                          INIT = c(7.07,-0.68,-21,-1,-6,-12), 
                          PRIOR = c(7.07,-0.68,-21,-6,-6,-12),
                          PR_SD = c(1.41,0.14,4.2,0.2,1.2,2.4),
                          PR_type = 6, PHASE = c(3,-5,4,4,-5,5),
                          env_varlink=0, dev_link=0, dev_minyr=0,dev_maxyr=0,
                          dev_PH=0,Block=0,Block_Fxn=0)
colnames(tmp_sel_pars) = colnames(ctl_tmp$age_selex_parms)
ctl_tmp$age_selex_parms = ctl_tmp$age_selex_parms %>% add_row(tmp_sel_pars, .after = 44)

# Tagging params
ctl_tmp$TG_Report_fleet = ctl_tmp$TG_Report_fleet %>% add_row(ctl_tmp$TG_Report_fleet[c(7, 10, 13), ])
ctl_tmp$TG_Report_fleet_decay = ctl_tmp$TG_Report_fleet_decay %>% add_row(ctl_tmp$TG_Report_fleet_decay[c(7, 10, 13), ])

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}

# -------------------------------------------------------------------------

# 19_TwoBlockCPUE
# This is RM3, run from RM2

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir_rm2, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir_rm2, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir_rm2, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir_rm2, 'starter.ss'))

# Config def:
config_name = '19_TwoBlockCPUE'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Modify data file
dat_tmp$Nfleets = dat_tmp$Nfleets + 3 # fisheries and indices
base_LL_names = dat_tmp$fleetinfo$fleetname[c(25,26,28)]
dat_tmp$fleetinfo$fleetname[c(25,26,28)] = paste0(base_LL_names, '_P2000')
dat_tmp$fleetinfo = dat_tmp$fleetinfo %>% add_row(data.frame(type = 3, surveytiming = 1, area = c(1,2,4), 
                                                             units = 2, need_catch_mult = 0, 
                                                             fleetname = paste0(base_LL_names, '_A2000')))
dat_tmp$fleetnames = dat_tmp$fleetinfo$fleetname
# start from qtr = 213 (2000) to change fleet
dat_tmp$CPUEinfo = dat_tmp$CPUEinfo %>% add_row(data.frame(fleet = 29:31, units = 0, errtype = 0, SD_report = 0))
dat_tmp$CPUE = dat_tmp$CPUE %>% mutate(index = if_else(index == 25 & year >= 213, 29, index)) # LL1b
dat_tmp$CPUE = dat_tmp$CPUE %>% mutate(index = if_else(index == 26 & year >= 213, 30, index)) # LL2
dat_tmp$CPUE = dat_tmp$CPUE %>% mutate(index = if_else(index == 28 & year >= 213, 31, index)) # LL4
dat_tmp$len_info = dat_tmp$len_info %>% add_row(dat_tmp$len_info[c(25, 26, 28), ])

# Modify control file
ctl_tmp$Q_options = ctl_tmp$Q_options %>% add_row(data.frame(fleet = 29:31, link = 2, link_info = 25, extra_se = 0, biasadj = 1, float = 0))
ctl_tmp$Q_parms = ctl_tmp$Q_parms %>% add_row(ctl_tmp$Q_parms[2:4, ])
ctl_tmp$size_selex_types = ctl_tmp$size_selex_types %>% add_row(ctl_tmp$size_selex_types[c(25, 26, 28), ])
ctl_tmp$age_selex_types = ctl_tmp$age_selex_types %>% add_row(data.frame(Pattern = 15, Discard = 0, Male = 0, Special = 22:24))

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}



# -------------------------------------------------------------------------

# 20_Dwtag01
# This is RM4, run from RM2

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir_rm2, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir_rm2, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir_rm2, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir_rm2, 'starter.ss'))

# Config def:
config_name = '20_Dwtag01'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Downweight tagging data
ctl_tmp$lambdas = rbind(#data.frame(like_comp = 5, fleet = 1:16, phase = 2, value = 0, sizefreq_method = 1), # age
  data.frame(like_comp = 15, fleet = 1:dat_tmp$N_tag_groups, phase = 2, value = 0.1, sizefreq_method = 1), # tag
  data.frame(like_comp = 16, fleet = 1:dat_tmp$N_tag_groups, phase = 2, value = 0.1, sizefreq_method = 1), # tag negative binom
  ctl_tmp$lambdas)
ctl_tmp$N_lambdas = nrow(ctl_tmp$lambdas)

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 21_EffortCreep
# This is RM5, run from RM2

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir_rm2, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir_rm2, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir_rm2, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir_rm2, 'starter.ss'))

# Config def:
config_name = '21_EffortCreep'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Apply effort creep 0.5%:
cpue_dat_effcreep = apply_eff_creep(dat_tmp$CPUE, yr_col = 'year', fleet_col = 'index',
                                    cpue_col = 'obs', cv_col = 'se_log', rate = 0.005)
cpue_dat_effcreep = cpue_dat_effcreep %>% mutate(month = 1, .after = 1)
dat_tmp$CPUE = cpue_dat_effcreep

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}