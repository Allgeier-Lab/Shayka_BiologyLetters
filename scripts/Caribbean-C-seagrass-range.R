#### Load libs and default params ####

library(foreign)
library(Rcpp)
library(stringr)
library(truncnorm)

# # source sample function (will be sourced on HPC)
# sourceCpp("Functions/rcpp_sample_carbon.cpp")

# print progress report
verbose <- TRUE

# set amount of random numbers for distributions
n_sample <- 10000000 # 50 000 000

##### Set default distribution parameters ####

# set mean and sd values
agb_mean <- 47.84 #mean aboveground (ag) biomass (gDW m-2)
agb_sd <- 28.98 #standard deviation of mean ag biomass

bgb_mean <- 191.47 #mean belowground (bg) biomass (gDW m-2)
bgb_sd <- 135.67 #standard deviation of mean bg biomass

agc_mean <- 0.369 #mean % Carbon of ag biomass as a decimal (gC gDW-1)
agc_sd <- 0.025 #standard deviation of mean % C of ag biomass

bgc_mean <- 0.30890 #mean % Carbon of bg biomass as a decimal (gC gDW-1)
bgc_sd <- 0.01059 #standard deviation of mean % C of bg biomass

# combine to list for easier looping
parameters_list <- list(ag = c(agb_mean = agb_mean, agb_sd = agb_sd,
                               agc_mean = agc_mean, agc_sd = agc_sd),
                        bg = c(bgb_mean = bgb_mean, bgb_sd = bgb_sd,
                               bgc_mean = bgc_mean, bgc_sd = bgc_sd))

#### Prepare submission data.frame ####

# get names of all tif database
countries_attr_tables <- list.files(path = "data/caribbean_maps", pattern = "*.tif.vat.dbf$", full.names = TRUE, recursive = TRUE) |> 
  stringr::str_sort()

# create table with number of sparse and dense cells
n_seagrass <- purrr::map_dfr(countries_attr_tables, function(tab_i) {
  
  tab_temp <- foreign::read.dbf(tab_i) #rat=raster attribute table #import rat from database
  
  n_dense <- tab_temp$Count[tab_temp$Value == "13"] # number of rows in the dense_data data frame; 13 in rat = dense seagrass
  
  n_sparse <- tab_temp$Count[tab_temp$Value == "12"] # number of rows in the sparse_data data frame; 12 in rat = sparse seagrass
  
  data.frame(n_sparse = ifelse(length(n_sparse) > 0, n_sparse, 0), 
             n_dense = ifelse(length(n_dense) > 0, n_dense, 0), 
             country = stringr::str_split(tab_i, pattern = "/", simplify = TRUE)[, 3])}) |> 
  dplyr::slice(rep(x = 1:dplyr::n(), each = 10))

#### Init function to run on HPC ####
foo <- function(n_sparse, n_dense, country) {
  
  # souce Rcpp
  Rcpp::sourceCpp("/home/mhessel/rcpp_sample_carbon.cpp")
  
  # loop AG and BG biomass
  purrr::map_dfr(parameters_list, function(part_i) {
  
    # create numbers to sample from
    dist_biom <- truncnorm::rtruncnorm(n = n_sample, mean = part_i[[1]], sd = part_i[[2]], 
                                       a = 0.0, b = Inf)
    
    dist_c <- truncnorm::rtruncnorm(n = n_sample, mean = part_i[[3]], sd = part_i[[4]],
                                    a = 0.0, b = 1.0)
    
    # create vector with TRUE/FALSE based on quantiles for biomass
    q_sprs <- dist_biom < quantile(x = dist_biom, probs = 0.5)
    q_dns <- dist_biom > quantile(x = dist_biom, probs = 0.5)
    
    # create vector with TRUE/FALSE based on quantiles for carbon
    q_c_low <- dist_c < quantile(x = dist_c, probs = 0.025)
    q_c_high <- dist_c > quantile(x = dist_c, probs = 0.975)
    
    # calculate cumulative biomass x carbon numbers
    sparse_low <- rcpp_sample_carbon(biomass = dist_biom[q_sprs], carbon = dist_c[q_c_low], 
                                     n = n_sparse, verbose = verbose)
    
    sparse_mean <- rcpp_sample_carbon(biomass = dist_biom[q_sprs], carbon = dist_c, 
                                      n = n_sparse, verbose = verbose)
    
    sparse_high <- rcpp_sample_carbon(biomass = dist_biom[q_sprs], carbon = dist_c[q_c_high], 
                                      n = n_sparse, verbose = verbose)
    
    # calculate cumulative biomass x carbon numbers
    dense_low <- rcpp_sample_carbon(biomass = dist_biom[q_dns], carbon = dist_c[q_c_low], 
                                    n = n_dense, verbose = verbose)
    
    dense_mean <- rcpp_sample_carbon(biomass = dist_biom[q_dns], carbon = dist_c, 
                                     n = n_dense, verbose = verbose)
    
    dense_high <- rcpp_sample_carbon(biomass = dist_biom[q_dns], carbon = dist_c[q_c_high], 
                                     n = n_dense, verbose = verbose)
    
    tibble::tibble(type = rep(x = c("sparse", "dense"), each = 3), 
                   range = rep(x = c("low", "mean", "high"), times = 2), 
                   value = c(sparse_low, sparse_mean, sparse_high, 
                             dense_low, dense_mean, dense_high), 
                   country = country)
    
  }, .id = "part")
}

#### Submit to cluster ####

globals <- c("parameters_list", "n_sample", "verbose") # filter_meta 

rscript_path <- "/sw/pkgs/arc/stacks/gcc/10.3.0/R/4.2.0/lib64/R/bin/Rscript"

sbatch_carbon <- rslurm::slurm_apply(f = foo, params = n_seagrass, 
                                     global_objects = globals, jobname = "segrass_c",
                                     nodes = nrow(n_seagrass), cpus_per_node = 1, 
                                     slurm_options = list("account" = "jeallg0", 
                                                          "partition" = "standard",
                                                          "time" = "02:00:00",
                                                          "mem-per-cpu" = "7G"),
                                     pkgs = c("Rcpp", "truncnorm", "purrr"),
                                     rscript_path = rscript_path, submit = FALSE)

#### Collect results #### 

suppoRt::rslurm_missing(x = sbatch_carbon)

carbon_df <- rslurm::get_slurm_out(sbatch_carbon, outtype = "table")

suppoRt::save_rds(object = carbon_df, path = "Results/", 
                  overwrite = FALSE, filename = "seagrass-carbon.rds")

write.csv(x = carbon_df, file = "Results/seagrass-carbon.csv", row.names = FALSE)

rslurm::cleanup_files(sbatch_carbon)
