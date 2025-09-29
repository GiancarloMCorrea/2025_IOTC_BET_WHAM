rm(list = ls())

# Sharepoint path and aux functions:
source('sharepoint_path.R')
source('code/auxiliary_functions.R')

# Run or just create folders?
run_model = TRUE
# Make plots?
make_plot = FALSE

# Spatial configuration:
spat_config = '2A_io'

# IMPORTANT: select what spatial aggregation type want to use:
# spat_subconfig = 'agg'
spat_subconfig = 'aaf'

# SS base files path (in Sharepoint):
SS_base = file.path('models/base', spat_config, spat_subconfig)

# SS configuration path (in Sharepoint):
SS_config = file.path('models/configurations', spat_config, spat_subconfig)

# -------------------------------------------------------------------------

# Read base SS inputs (from 2021 assessment)
base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
base_fore$Forecast = -1 # dont do forecast for now

# -------------------------------------------------------------------------
# Start implementing configurations ---------------------------------------

# 1: catch + cpue + length  ------------------------------------------------

config_name = '1_BC'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Temporary files:
dat_tmp = base_dat
ctl_tmp = base_ctl
fore_tmp = base_fore
start_tmp = base_start

# Turn off age and tag:
ctl_tmp$lambdas = rbind(data.frame(like_comp = 5, fleet = 1:dat_tmp$Nfleet, phase = 2, value = 0, sizefreq_method = 1), # age
                        data.frame(like_comp = 15, fleet = 1:dat_tmp$N_tag_groups, phase = 2, value = 0, sizefreq_method = 1), # tag
                        data.frame(like_comp = 16, fleet = 1:dat_tmp$N_tag_groups, phase = 2, value = 0, sizefreq_method = 1), # tag negative binom
                        ctl_tmp$lambdas)
ctl_tmp$N_lambdas = nrow(ctl_tmp$lambdas)
ctl_tmp$TG_Report_fleet[,7] = -6 # fix all tag params

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
# Plot results
if(make_plot) {
  tmp_mod = SS_output(dir = tmp_dir, covar = FALSE, forecast = FALSE)
  SS_plots(tmp_mod)
}


# 2: add tagging 0.1 -------------------------------------------------

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config name
config_name = '2_addTag'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Activate tagging:
ctl_tmp$lambdas$value[ctl_tmp$lambdas$like_comp %in% c(15,16)] = 0.1
ctl_tmp$TG_Report_fleet = base_ctl$TG_Report_fleet # estimate tag params again

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
# Plot results
if(make_plot) {
  tmp_mod = SS_output(dir = tmp_dir, covar = FALSE, forecast = FALSE)
  SS_plots(tmp_mod)
}

# 3: add CAAL data ------------------------------------------------------

# Read previous input files:
dat_tmp = SS_readdat(file = file.path(tmp_dir, 'data.ss'))
ctl_tmp = SS_readctl(file = file.path(tmp_dir, 'control.ss'), datlist = dat_tmp)
fore_tmp = SS_readforecast(file = file.path(tmp_dir, 'forecast.ss'))
start_tmp = SS_readstarter(file = file.path(tmp_dir, 'starter.ss'))

# Config def
config_name = '3_addCAAL'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Activate CAAL:
ctl_tmp$lambdas$value[ctl_tmp$lambdas$like_comp == 5] = 1

# Write SS files:
SS_writedat(dat_tmp, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_tmp, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_tmp, dir = tmp_dir, overwrite = T)
SS_writestarter(start_tmp, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
# Plot results
if(make_plot) {
  tmp_mod = SS_output(dir = tmp_dir, covar = FALSE, forecast = FALSE)
  SS_plots(tmp_mod)
}