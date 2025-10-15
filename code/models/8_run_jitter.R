rm(list = ls())
library(wham)
library(dplyr)
require(ggplot2)
source('code/auxiliary_functions.R')

# Model codes (ie numbers):
mod_codes = as.character(1:4)

# -------------------------------------------------------------------------
# Run jitter in parallel:
for(i in seq_along(mod_codes)) {
  this_model = mod_codes[i]
  save_folder = paste0('models/jitter', this_model)
  dir.create(save_folder)
  jitter_wham(fit_RDS = paste0('models/fit', this_model, '.rds'), n_cores = 10, res_dir = save_folder)
}


# -------------------------------------------------------------------------
# Analyze jitter results:
n_sim = 10 # number jitter
mod_codes = as.character(1:4)
dir.create('figures/jitter', showWarnings = FALSE)

save_ts = list()
save_par = list()
save_like = list()
count_i = 1
for(j in seq_along(mod_codes)) {
  for(i in 1:n_sim) {
    jit = readRDS(file.path('models', paste0('jitter', mod_codes[j]), paste0('jitter_sim_', i, '.RDS')))
    save_ts[[count_i]] = data.frame(year = mod_str_yr:mod_end_yr, i = i,
                                    model = mod_codes[j],
                                    SSB = jit$rep$SSB,
                                    SSBmsy = jit$rep$SSB/exp(jit$rep$log_SSB_MSY))
    save_par[[count_i]] = data.frame(par = names(jit$opt$par), i = i, model = mod_codes[j], 
                                     value = as.vector(jit$opt$par))
    save_like[[count_i]] = data.frame(i = i, model = mod_codes[j], 
                                      like = jit$opt$objective)
    count_i = count_i + 1
  }
}

# Prepare plot data frame:
plot_ts = bind_rows(save_ts) %>% mutate(model = factor(model, levels = mod_codes, labels = model_labels))
plot_par = bind_rows(save_par) %>% mutate(model = factor(model, levels = mod_codes, labels = model_labels))
plot_like = bind_rows(save_like) %>% mutate(model = factor(model, levels = mod_codes, labels = model_labels))

p1 = ggplot(data = plot_ts, aes(x = year, y = SSB/1000)) +
  geom_line(aes(group = i), color = 'gray50') +
  xlab('Year') + ylab("SSB ('000 mt)") +
  theme_classic() +
  facet_wrap(~ model)
ggsave('figures/jitter/SSB.png', plot = p1, width = 170, height = 130, units = 'mm', dpi = 400)

p2 = ggplot(data = plot_like, aes(x = i, y = like)) +
  geom_point(color = 'gray50') +
  scale_x_continuous(breaks = 1:n_sim) +
  xlab('Jitter') + ylab("neg LL") +
  theme_bw() +
  facet_wrap(~ model)
ggsave('figures/jitter/nLL.png', plot = p2, width = 170, height = 130, units = 'mm', dpi = 400)
