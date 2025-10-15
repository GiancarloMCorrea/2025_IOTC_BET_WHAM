args = commandArgs(trailingOnly=TRUE)
mod_i = as.integer(args[1])

main_dir = here::here() 
source(here::here('code', 'auxiliary_functions.R'))
run_df = readRDS(here::here("data", "run_df.rds"))
h_init = 0.8
source(here::here('code', 'models', '1_prepare_wham_data.R'))
source(here::here('code', 'models', paste0('2_add_indices_pointers_', run_df$index_type[mod_i],'.R')))
source(here::here('code', 'models', '3_set_parameters.R'))

# Run model:
tmp_fit = wham::fit_wham(input = input_wham, do.retro = FALSE, do.osa = FALSE, 
                         do.sdrep = TRUE, do.fit = TRUE, 
                         n.newton = 3, do.proj = FALSE, MakeADFun.silent = TRUE)
saveRDS(tmp_fit, file = file.path(main_dir, "models", paste0("fit", run_df$i[mod_i],".RDS")))