rm(list = ls())
# remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='tuna')
# devtools::install_local(path = 'C:/Use/GitHub/wham', force = TRUE)
library(readxl)
library(wham)
library(ggplot2)
library(dplyr)
source('code/auxiliary_functions.R')
h_init = 0.8

# Run model: LL index + one block LL
source('code/models/1_prepare_wham_data.R')
source('code/models/2_add_indices_pointers_a.R')
source('code/models/3_set_parameters.R')
# Run model:
fit1 = wham::fit_wham(input = input1, do.retro = FALSE, do.osa = FALSE, 
                      do.sdrep = FALSE, do.fit = TRUE, n.newton = 0, 
                      n.peels = 5)

# Run model: LL-PSLS index + one block LL
source('code/models/1_prepare_wham_data.R')
source('code/models/2_add_indices_pointers_b.R')
source('code/models/3_set_parameters.R')
# Run model:
fit2 = wham::fit_wham(input = input1, do.retro = FALSE, do.osa = FALSE, 
                      do.sdrep = FALSE, do.fit = TRUE, n.newton = 3)

# Run model: LL index + two block LL
source('code/models/1_prepare_wham_data.R')
source('code/models/2_add_indices_pointers_c.R')
source('code/models/3_set_parameters.R')
# Run model:
fit3 = wham::fit_wham(input = input1, do.retro = FALSE, do.osa = FALSE, 
                      do.sdrep = FALSE, do.fit = TRUE, n.newton = 3)

# Run model: LL-PSLS index + two block LL
source('code/models/1_prepare_wham_data.R')
source('code/models/2_add_indices_pointers_d.R')
source('code/models/3_set_parameters.R')
# Run model:
fit4 = wham::fit_wham(input = input1, do.retro = FALSE, do.osa = FALSE, 
                      do.sdrep = FALSE, do.fit = TRUE, n.newton = 3)


fit1$opt$convergence
fit2$opt$convergence
fit3$opt$convergence
fit4$opt$convergence

fit1$opt$objective
fit2$opt$objective
fit3$opt$objective
fit4$opt$objective

plot(fit1$rep$SSB, type = 'l', ylim = c(0.3e+06, 1.6e+06))
lines(exp(fit1$rep$log_SSB_MSY), col = 2)

plot(fit2$rep$SSB, type = 'l', ylim = c(0.3e+06, 1.6e+06))
lines(exp(fit2$rep$log_SSB_MSY), col = 2)

plot(fit3$rep$SSB, type = 'l', ylim = c(0.3e+06, 1.6e+06))
lines(exp(fit3$rep$log_SSB_MSY), col = 2)

plot(fit4$rep$SSB, type = 'l', ylim = c(0.3e+06, 1.6e+06))
lines(exp(fit4$rep$log_SSB_MSY), col = 2)


# -------------------------------------------------------------------------
# Run projections:
proj1 = wham::project_wham(model = fit1, proj.opts = list(n.yrs = 3),
                           do.sdrep = FALSE)


# -------------------------------------------------------------------------



fit1$rep[grep('nll',names(fit1$rep))] %>% lapply(sum) %>% unlist
fit1$final_gradient
max(abs(fit1$final_gradient))
fit1$sdrep
fit1$opt$message
fit1$opt$convergence
plot(fit1$rep$SSB, type = 'l', ylim = c(0.3e+06, 1.6e+06))
lines(exp(fit1$rep$log_SSB_MSY), col = 2)
plot(fit1$rep$NAA[1,], type = 'l')
plot(fit1$rep$NAA[,1], type = 'l')
plot(exp(fit1$rep$log_MSY), type = 'b')
exp(fit1$parList$log_NAA_sigma)
sdrep = summary(fit1$sdrep)
sdrep[which(rownames(sdrep) == 'log_SSB'),]
sdrep[which(rownames(sdrep) == 'log_SSB_MSY'),]
sdrep[which(rownames(sdrep) == 'log_FMSY'),]

plot(fit1$rep$selAL[[1]][1,], type = 'b')
plot(fit1$rep$selAL[[2]][1,], type = 'b')
plot(fit1$rep$selAL[[3]][1,], type = 'b')
plot(fit1$rep$selAL[[4]][1,], type = 'b')
plot(fit1$rep$selAL[[5]][1,], type = 'b')
plot(fit1$rep$selAL[[6]][1,], type = 'b')

plot(fit1$rep$MAA[1,], type = 'b')
plot(fit1$rep$jan1LAA[1,], type = 'b')
matplot(fit1$rep$jan1_phi_mat[,,1], type = 'l')
matplot(fit1$rep$catch_phi_mat[,,1], type = 'l')
plot(fit1$rep$pred_waa[1,1,], type = 'b')
lines(fit1$rep$pred_waa[2,1,], type = 'l', col = 2)
plot(fit1$rep$mat_at_len[1,], type = 'l')

# saveRDS(fit1, file = "models/year/fit1.RDS")
fit1 = readRDS("models/fit1.RDS")
fit2 = readRDS("models/fit2.RDS")
fit3 = readRDS("models/fit3.RDS")
fit4 = readRDS("models/fit4.RDS")

# Make plots:
save_plots = 'figures/fit1'
plot_wham_output(mod = fit1, dir.main = save_plots, out.type = 'png')
save_plots = 'figures/fit2'
plot_wham_output(mod = fit2, dir.main = save_plots, out.type = 'png')
save_plots = 'figures/fit3'
plot_wham_output(mod = fit3, dir.main = save_plots, out.type = 'png')
save_plots = 'figures/fit4'
plot_wham_output(mod = fit4, dir.main = save_plots, out.type = 'png')
