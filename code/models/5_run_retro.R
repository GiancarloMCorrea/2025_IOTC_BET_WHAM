rm(list = ls())
library(wham)
require(doParallel)
require(foreach)
require(snowfall)

# -------------------------------------------------------------------------
# Run retro in parallel:

snowfall::sfInit(parallel=TRUE, cpus=4)
trash <- snowfall::sfLapply(1:4, function(i){
  require(wham)
  main_dir = here::here() 
  tmp_fit = readRDS(file.path(main_dir, 'models', paste0('fit', i, '.RDS')))
  tmp_retro = retro(model = tmp_fit, n.peels = 5, n.newton = 0, MakeADFun.silent = TRUE)
  tmp_fit$peels = tmp_retro
  saveRDS(tmp_fit, file = file.path(main_dir, 'models', paste0('fit', i, '.RDS')))
})
snowfall::sfStop()