rm(list = ls())
library(wham)
library(ggplot2)
library(dplyr)
source('code/auxiliary_functions.R')

# Model codes (ie numbers):
mod_codes = as.character(1:4)

# -------------------------------------------------------------------------
# Run projs and save projected quantities: SSB, SSB/SSBmsy, total catch
save_ts = list()
for(j in seq_along(mod_codes)) {
  
  save_ts[[j]] = NULL
  tmp_fit = readRDS(paste0('models/fit', mod_codes[j], '.rds'))
  #if(tmp_fit$opt$convergence == 0) {
    tmp_proj = project_wham(model = tmp_fit, proj.opts = list(n.yrs = 5))
    sd_rep = summary(tmp_proj$sdrep)
    # SSB projections
    x = sd_rep[rownames(sd_rep) == "log_SSB",] # SSB estimates with SE
    mat = exp(cbind(x, x[,1] + qnorm(0.975)*cbind(-x[,2],x[,2]))) # calculate 95% CI
    colnames(mat) <- c("est","se","lwr","upr")
    mat = as.data.frame(mat)
    mat$year = tmp_proj$years_full
    mat$model = mod_codes[j]
    mat$type = 'SSB'
    save_ts[[j]] = mat
    # SSBmsy projections
    x = sd_rep[rownames(sd_rep) == "log_SSB_MSY",] # SSBmsy estimates with SE
    mat = exp(cbind(x, x[,1] + qnorm(0.975)*cbind(-x[,2],x[,2]))) # calculate 95% CI
    colnames(mat) <- c("est","se","lwr","upr")
    mat = as.data.frame(mat)
    mat$year = tmp_proj$years_full
    mat$model = mod_codes[j]
    mat$type = 'SSBmsy'    
    save_ts[[j]] = bind_rows(save_ts[[j]], mat)
    # Catch projections
    x = tmp_proj$rep$pred_catch
    mat = as.matrix(rowSums(x))
    colnames(mat) <- c("est")
    mat = as.data.frame(mat)
    mat$year = tmp_proj$years_full
    mat$model = mod_codes[j]
    mat$type = 'Tot_catch' 
    save_ts[[j]] = bind_rows(save_ts[[j]], mat)
    # MSY projections
    x = sd_rep[rownames(sd_rep) == "log_MSY",] # SSBmsy estimates with SE
    mat = exp(cbind(x, x[,1] + qnorm(0.975)*cbind(-x[,2],x[,2]))) # calculate 95% CI
    colnames(mat) <- c("est","se","lwr","upr")
    mat = as.data.frame(mat)
    mat$year = tmp_proj$years_full
    mat$model = mod_codes[j]
    mat$type = 'MSY'  
    save_ts[[j]] = bind_rows(save_ts[[j]], mat)

    # Remove object to save some memory 
    rm(tmp_fit, tmp_proj)
    cat("Model", j, "done", "\n")
  #}
  
}

plot_data = bind_rows(save_ts)
saveRDS(plot_data, file = 'models/proj_results.rds')

# -------------------------------------------------------------------------
# Plot projected quantities:
dir.create('figures/proj', showWarnings = FALSE)
plot_data = plot_data %>% filter(year > 2020) %>% mutate(model = factor(model, levels = mod_codes, labels = model_labels))

p1 = ggplot(data = plot_data %>% filter(type %in% c('SSB', 'SSBmsy')), aes(x = year, y = est/1000)) +
  geom_line(aes(color = type)) +
  geom_ribbon(aes(ymin = lwr/1000, ymax = upr/1000, fill = type), alpha = 0.2) +
  xlab(NULL) + ylab("Spawning biomass ('000 tonnes)") +
  scale_x_continuous(breaks = seq(from = 2020, to = 2029, by = 2)) +
  scale_color_hue() + scale_fill_hue() +
  geom_vline(xintercept = 2025, linetype = 'dashed') +
  theme_classic() +
  theme(legend.position = 'bottom') +
  labs(color = 'Type', fill = 'Type') +
  facet_wrap(~ model)
ggsave('figures/proj/SSB.png', plot = p1, width = 170, height = 130, units = 'mm', dpi = 400)

p2 = ggplot(data = plot_data %>% filter(type %in% c('Tot_catch', 'MSY')), aes(x = year, y = est/1000)) +
  geom_line(aes(color = type)) +
  geom_ribbon(aes(ymin = lwr/1000, ymax = upr/1000, fill = type), alpha = 0.2) +
  xlab(NULL) + ylab("Total catch ('000 tonnes)") +
  scale_x_continuous(breaks = seq(from = 2020, to = 2029, by = 2)) +
  scale_color_hue() + scale_fill_hue() +
  geom_vline(xintercept = 2025, linetype = 'dashed') +
  theme_classic() +
  theme(legend.position = 'bottom') +
  labs(color = 'Type', fill = 'Type') +
  facet_wrap(~ model)
ggsave('figures/proj/catch.png', plot = p2, width = 170, height = 130, units = 'mm', dpi = 400)
