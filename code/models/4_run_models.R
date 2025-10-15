rm(list = ls())
# remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='tuna')
# devtools::install_local(path = 'C:/Use/GitHub/wham', force = TRUE)
library(readxl)
library(wham)
library(ggplot2)
library(dplyr)
require(doParallel)
require(foreach)
require(snowfall)
dir.create('models', showWarnings = FALSE)
dir.create('figures', showWarnings = FALSE)

# Create data frame to run models:
run_df = data.frame(i = as.character(1:4),
                    index_type = c('a', 'b', 'c', 'd'), times = 3)
saveRDS(run_df, file = 'data/run_df.rds')

# -------------------------------------------------------------------------
# Run models in parallel:

run_iter <- function(mod_i){
  cmd <- paste("Rscript --vanilla code/models/fit_model.R", mod_i)
  system(cmd)
}

snowfall::sfInit(parallel=TRUE, cpus=3) # modify this
snowfall::sfExportAll()
trash = snowfall::sfLapply(2:4, function(mod_i) run_iter(mod_i))
snowfall::sfStop()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Check model:
# this_mod = tmp_fit
# max(abs(this_mod$final_gradient))
# this_mod$opt$convergence
# this_mod$sdrep
# sdrep = summary(this_mod$sdrep)
# sdrep[which(rownames(sdrep) == 'log_SSB'),]
# exp(this_mod$parList$log_NAA_sigma)
# this_mod$rep$log_FMSY
# plot(this_mod$rep$SSB, type = 'l', ylim = c(0.3e+06, 1.6e+06))

