rm(list = ls())
library(wham)
require(doParallel)
require(foreach)
require(snowfall)

# -------------------------------------------------------------------------
# Run profile in parallel:
# M values: seq(from = 0.28, to = 0.42, by = 2.5)
# Number of CPU and loops equal to number of M values

# par_vec = seq(from = 0.28, to = 0.42, by = 0.02) # for M
par_vec = seq(from = 35000, to = 44000, by = 1000) # for R0

dir.create('models/profile_R0', showWarnings = FALSE)
snowfall::sfInit(parallel=TRUE, cpus=min(10, length(par_vec)))
snowfall::sfExportAll()
trash <- snowfall::sfLapply(1:length(par_vec), function(i){
  require(wham)
  main_dir = here::here() 
  tmp_fit = readRDS(file.path(main_dir, 'models', 'fit3.rds')) # here select your model
  tmp_input = tmp_fit$input
  # tmp_input$par$M_a = log(par_vec[i])
  tmp_input$par$mean_rec_pars[2] = log(par_vec[i])
  tmp_input$map$mean_rec_pars = factor(c(NA,NA))
  new_fit = fit_wham(input = tmp_input, do.retro = FALSE, do.osa = FALSE, 
                     do.sdrep = FALSE, do.fit = TRUE, 
                     n.newton = 3, do.proj = FALSE, MakeADFun.silent = FALSE)
  saveRDS(new_fit, file = file.path(main_dir, 'models', 'profile_R0', paste0('prof', i, '.RDS')))
})
snowfall::sfStop()


# -------------------------------------------------------------------------
# Make plot:
dir.create('figures/profile_R0')

fit3 = readRDS('models/fit3.rds')
all_files = list.files('models/profile_M')
save_df = list()
for(i in seq_along(all_files)) {
  
  tmp = readRDS(file.path('models/profile_M', all_files[i]))
  save_df[[i]] = data.frame(nll = tmp$opt$objective,
                             nll_catch = sum(tmp$rep$nll_agg_catch),
                             nll_index = sum(tmp$rep$nll_agg_indices),
                             nll_len = sum(tmp$rep$nll_catch_lcomp),
                             # par = exp(tmp$parList$mean_rec_pars[2]) # R0
                             par = exp(tmp$parList$M_a) # M
                  )
}

# Make plot:
plot_data = bind_rows(save_df)
plot_data = plot_data %>% mutate(rel_nll = nll - min(nll),
                                 rel_nll_catch = nll_catch - min(nll_catch),
                                 rel_nll_index = nll_index - min(nll_index),
                                 rel_nll_len = nll_len - min(nll_len))

p1 = ggplot(data = plot_data, aes(x = par, y = rel_nll)) +
  geom_line() +
  #geom_line(data = plot_data, aes(x = par, y = rel_nll_catch), color = 'blue') +
  #geom_line(data = plot_data, aes(x = par, y = rel_nll_index), color = 'red') +
  #geom_line(data = plot_data, aes(x = par, y = rel_nll_len), color = 'green') +
  geom_vline(xintercept = exp(fit3$parList$M_a), linetype = 'dashed', lwd = 1) +
  xlab('M value') + ylab('Change in total neg LL') +
  theme_bw()
ggsave('figures/profile_M/nLL.png', plot = p1, width = 100, height = 80, units = 'mm', dpi = 400)
