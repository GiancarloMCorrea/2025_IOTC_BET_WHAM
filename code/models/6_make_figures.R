rm(list = ls())
library(wham)
library(ggplot2)
library(dplyr)
source('code/auxiliary_functions.R')

# Model codes (ie numbers):
mod_codes = as.character(1:4)

# -------------------------------------------------------------------------
# Make standard figures in loop:

for(j in seq_along(mod_codes)) {
  
  tmp_fit = readRDS(paste0('models/fit', mod_codes[j], '.rds'))
  #if(tmp_fit$opt$convergence == 0) {
    save_plots = paste0('figures/fit', mod_codes[j])
    if(tmp_fit$env$data$n_indices == 1) index_lab = c('LL')
    if(tmp_fit$env$data$n_indices == 2) index_lab = c('LL', 'PSLS')
    tryCatch(plot_wham_output(mod = tmp_fit, dir.main = save_plots, out.type = 'png', res = 150,
                     fleet.labels = fleet_order, index.labels = index_lab),
             error = function(e) conditionMessage(e))
    rm(tmp_fit)
  #}
  
}


# -------------------------------------------------------------------------
# Make figures to compare among models:

# Compare 1: one vs two blocks selex LL
fit1 = readRDS('models/fit1.rds')
fit2 = readRDS('models/fit2.rds')
fit3 = readRDS('models/fit3.rds')
fit4 = readRDS('models/fit4.rds')

all_fits = list(fit1 = fit1, fit2 = fit2, fit3 = fit3, fit4 = fit4)
names(all_fits) = model_labels
dir.create('figures/compare', showWarnings = FALSE)
wham::compare_wham_models(mods = all_fits, fdir = 'figures/compare',
                          plot.opts = list(refpt = 'MSY', which = c(1,8,9,10)),
                          table.opts = list(calc.aic = TRUE,
                                            calc.rho = TRUE))

