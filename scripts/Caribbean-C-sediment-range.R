#### Load libs and default params ####

library(foreign)
library(Rcpp)
library(stringr)
library(truncnorm)

# # source sample function (will be sourced on HPC)
# sourceCpp("Functions/rcpp_sample_sediment.cpp")

# print progress report
verbose <- TRUE

# set amount of random numbers for distributions
n_sample <- 10000000 # 50 000 000

##### Set default distribution parameters ####

# set mean and sd values
sedc_mean <- 15090 #mean Carbon in top 1m of sediment (gC m-2)
sedc_sd <- 4840 #standard deviation of mean C in sediment

#### Prepare submission data.frame ####

# get names of all tif database
countries_attr_tables <- list.files(path = "data/caribbean_maps/", pattern = "*.tif.vat.dbf$", full.names = TRUE, recursive = TRUE) |> 
  stringr::str_sort()

# create table with number of sparse and dense cells
n_sediment <- purrr::map_dfr(countries_attr_tables, function(tab_i) {
  
  tab_temp <- foreign::read.dbf(tab_i) #rat=raster attribute table #import rat from database
  
  n_sedi <- ifelse(test = length(tab_temp$Count[tab_temp$Value == "12"]) == 0, 
         yes = 0, no = tab_temp$Count[tab_temp$Value == "12"]) + 
    ifelse(test = length(tab_temp$Count[tab_temp$Value == "13"]) == 0, 
         yes = 0, no = tab_temp$Count[tab_temp$Value == "13"])
  
  data.frame(n_sedi = n_sedi, country = stringr::str_split(tab_i, pattern = "/", 
                                                           simplify = TRUE)[, 3])}) |> 
  dplyr::slice(rep(x = 1:dplyr::n(), each = 10))

#### Init function to run on HPC ####
foo <- function(n_sedi, country) {
  
  # souce Rcpp
  Rcpp::sourceCpp("/home/mhessel/rcpp_sample_sediment.cpp")
  
  # create numbers to sample from
  dist_sedim <- truncnorm::rtruncnorm(n = n_sample, mean = sedc_mean, sd = sedc_sd, 
                                      a = 0.0, b = Inf)

  # create vector with TRUE/FALSE based on quantiles for carbon
  q_sedim_low <- dist_sedim < quantile(x = dist_sedim, probs = 0.025)
  q_sedim_high <- dist_sedim > quantile(x = dist_sedim, probs = 0.975)
  
  # calculate cumulative biomass x carbon numbers
  sedim_low <- rcpp_sample_sediment(sediment = dist_sedim[q_sedim_low], 
                                    n = n_sedi, verbose = verbose)
  
  sedim_mean <- rcpp_sample_sediment(sediment = dist_sedim, n = n_sedi, verbose = verbose)
  
  sedim_high <- rcpp_sample_sediment(sediment = dist_sedim[q_sedim_high], 
                                     n = n_sedi, verbose = verbose)
  
  tibble::tibble(type = rep(x = "sediment", each = 3), 
                 range = c("low", "mean", "high"),
                 value = c(sedim_low, sedim_mean, sedim_high), 
                 country = country)
    
}

#### Submit to cluster ####

globals <- c("n_sample", "sedc_mean", "sedc_sd", "verbose") # filter_meta 

rscript_path <- "/sw/pkgs/arc/stacks/gcc/10.3.0/R/4.2.0/lib64/R/bin/Rscript"

sbatch_sediment <- rslurm::slurm_apply(f = foo, params = n_sediment, 
                                       global_objects = globals, jobname = "sediment_c",
                                       nodes = nrow(n_sediment), cpus_per_node = 1, 
                                       slurm_options = list("account" = "jeallg0", 
                                                            "partition" = "standard",
                                                            "time" = "02:00:00",
                                                            "mem-per-cpu" = "7G"),
                                       pkgs = c("Rcpp", "truncnorm", "purrr"),
                                       rscript_path = rscript_path, submit = FALSE)

#### Collect results #### 

suppoRt::rslurm_missing(x = sbatch_sediment)

sediment_df <- rslurm::get_slurm_out(sbatch_sediment, outtype = "table")

suppoRt::save_rds(object = sediment_df, path = "Results/", 
                  overwrite = FALSE, filename = "sediment-carbon.rds")

write.csv(x = sediment_df, file = "Results/sediment-carbon.csv", row.names = FALSE)

rslurm::cleanup_files(sbatch_sediment)
