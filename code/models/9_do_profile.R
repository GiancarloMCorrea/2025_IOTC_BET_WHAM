rm(list = ls())
library(wham)
require(doParallel)
require(foreach)
require(dplyr)
require(snowfall)
require(ggplot2)

# -------------------------------------------------------------------------
# Run profile in parallel:
# M values: seq(from = 0.28, to = 0.42, by = 2.5)
# Number of CPU and loops equal to number of M values

folder_name = 'profile_N1'
# par_vec = seq(from = 0.28, to = 0.42, by = 0.02) # for M
par_vec = seq(from = 9.7, to = 10.6, by = 0.1) # for N1, in log-scale

dir.create(file.path('models', folder_name), showWarnings = FALSE)
snowfall::sfInit(parallel=TRUE, cpus=min(10, length(par_vec)))
snowfall::sfExportAll()
trash <- snowfall::sfLapply(1:length(par_vec), function(i){
  require(wham)
  main_dir = here::here() 
  tmp_fit = readRDS(file.path(main_dir, 'models', 'fit3.rds')) # here select your model
  tmp_input = tmp_fit$input
  # tmp_input$par$M_a = log(par_vec[i])
  tmp_input$par$log_N1_pars[1] = par_vec[i]
  tmp_input$map$log_N1_pars = factor(c(NA,1))
  new_fit = fit_wham(input = tmp_input, do.retro = FALSE, do.osa = FALSE, 
                     do.sdrep = FALSE, do.fit = TRUE, 
                     n.newton = 3, do.proj = FALSE, MakeADFun.silent = FALSE)
  saveRDS(new_fit, file = file.path(main_dir, 'models', folder_name, 
                                    paste0('prof', i, '.RDS')))
})
snowfall::sfStop()


# -------------------------------------------------------------------------
# Make plot:
dir.create(file.path('figures', folder_name), showWarnings = FALSE)

fit2 = readRDS('models/fit2.rds')
all_files = list.files(file.path('models', folder_name))
save_df = list()
for(i in seq_along(all_files)) {
  
  tmp = readRDS(file.path('models', folder_name, all_files[i]))
  save_df[[i]] = data.frame(nll = tmp$opt$objective,
                             nll_catch = sum(tmp$rep$nll_agg_catch),
                             nll_naa = tmp$rep$nll_NAA,
                             nll_index = sum(tmp$rep$nll_agg_indices),
                             nll_index_1 = sum(tmp$rep$nll_agg_indices[,1]),
                             nll_index_2 = sum(tmp$rep$nll_agg_indices[,2]),
                             nll_len = sum(tmp$rep$nll_catch_lcomp),
                             nll_len_1 = sum(tmp$rep$nll_catch_lcomp[,1]),
                             nll_len_2 = sum(tmp$rep$nll_catch_lcomp[,2]),
                             nll_len_3 = sum(tmp$rep$nll_catch_lcomp[,3]),
                             nll_len_4 = sum(tmp$rep$nll_catch_lcomp[,4]),
                             nll_len_5 = sum(tmp$rep$nll_catch_lcomp[,5]),
                             nll_len_6 = sum(tmp$rep$nll_catch_lcomp[,6]),
                             nll_len_7 = sum(tmp$rep$nll_catch_lcomp[,7]),
                             # par = exp(tmp$parList$mean_rec_pars[2]) # R0
                             par = tmp$parList$log_N1_pars[1]
                  )
}

# Make plot:
plot_data = bind_rows(save_df)
plot_data = plot_data %>% mutate(Total = nll - min(nll),
                                 Tot_Catch = nll_catch - min(nll_catch),
                                 NAA = nll_naa - min(nll_naa),
                                 Index = nll_index - min(nll_index),
                                 rel_nll_index1 = nll_index_1 - min(nll_index_1),
                                 rel_nll_index2 = nll_index_2 - min(nll_index_2),
                                 Len_comps = nll_len - min(nll_len),
                                 rel_nll_len1 = nll_len_1 - min(nll_len_1),
                                 rel_nll_len2 = nll_len_2 - min(nll_len_2),
                                 rel_nll_len3 = nll_len_3 - min(nll_len_3),
                                 rel_nll_len4 = nll_len_4 - min(nll_len_4),
                                 rel_nll_len5 = nll_len_5 - min(nll_len_5),
                                 rel_nll_len6 = nll_len_6 - min(nll_len_6),
                                 rel_nll_len7 = nll_len_7 - min(nll_len_7))

colors <- c("Total" = "black", "Tot_Catch" = "blue", "NAA" = "red",
            "Index" = "green", "Len_comps" = "orange")

p1 = ggplot(data = plot_data, aes(x = par)) +
  geom_line(aes(y = Total, color = "Total")) +
  geom_line(aes(y = Tot_Catch, color = 'Tot_Catch')) +
  geom_line(aes(y = NAA, color = 'NAA')) +
  geom_line(aes(y = Index, color = 'Index')) +
  geom_line(aes(y = Len_comps, color = 'Len_comps')) +
  scale_color_manual(values = colors) +
  coord_cartesian(ylim = c(0, 6)) +
  geom_vline(xintercept = fit2$parList$log_N1_pars[1], linetype = 'dashed', lwd = 1) +
  xlab('log(N1)') + ylab('Change in neg LL') + labs(color = NULL) +
  theme_bw()
ggsave(file.path('figures', folder_name, 'nLL.png'), plot = p1, 
       width = 140, height = 110, units = 'mm', dpi = 400)
