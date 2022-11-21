###########
#This code takes .tif files and .tif.vat.dbf files and calculates nitrogen storage in that area of seagrass
#This is for all of the Caribbean
#Created by Bridget Shayka
##########

##Load libraries -------------
library(foreign)
library(tidyverse)
library(reshape2)
library(Rcpp) #need for C++ loop function


##Load functions ------------
sourceCpp("Functions/rcpp_calc_carbon.cpp")
sourceCpp("Functions/rcpp_calc_sed.cpp")
#C++ functions (2 of them)


##Load data -----------------
#This loads all the tifs for all of the Caribbean and sorts them alphabetically,
#and loads the raster attribute tables for each map and sorts them alphabetically
#Anguilla, Antigua & Barbuda, Barbados, Bahamas, British Virgin Islands, Cuba, Cayman Islands, Dominica, Dominican Republic, Grenada, Guadeloupe, Haiti, Jamaica, Martinique, Monterrat, Puerto Rico, Saba, 
#Saint Barthelemy, Saint Lucia, Saint Martin, Sint Eustatius, Sint Maarten, Saint Kitts and Nevis, Saint Vincent and the Grenadines, Turks and Caicos Islands, Trinidad and Tobago, US Virgin Islands

#full.names = TRUE returns the file path, if FALSE just returns names of files
names_tif <- list.files("data/caribbean_maps/", pattern = "*.tif$", full.names = TRUE) %>% 
  stringr::str_sort()

names_rat <- list.files("data/caribbean_maps/", pattern = "*.tif.vat.dbf$", full.names = TRUE) %>% 
  stringr::str_sort()


##Data analysis -------------

#Mean (+-SD) AG biomass 47.84 +- 28.98 gDW m-2 from Fourqurean et al. 2012 NatGeosci data for only Caribbean Thalassia communities
#Mean (+-SD) %N for AG biomass 1.466 +- 0.124 % of DW from Layman et al. 2016 EcolEng Supp Appendix A for Thalassia testudinum in The Bahamas

#Mean (+-SD) BG biomass 191.47 +- 135.67 gDW m-2 from Fourqurean et al. 2012 NatGeosci data for only Caribbean Thalassia communities
#Mean (+-SD) %N for BG biomass 0.8285 +- 0.0804 % of DW from Layman et al. 2016 EcolEng Supp Appendix A for Thalassia testudinum in The Bahamas
#Root: 0.902 +- 0.129, Rhizome: 0.755 +- 0.096
#(0.902+0.755)/2 #= 0.8285
#.5*sqrt(((0.129)^2)+((0.096)^2)) #= 0.08040056

#Mean (+-SD) sediment N 1240 +- 440 gN m-2 in top 1m of sediment from Fourqurean et al. 2012 MarFWRes for FL Bay and Shark Bay 
#These N numbers are for Florida Bay and Shark Bay, which are very different, but there was not a significant difference in N between locations (p>0.05), so I am using the overall average and sd b/c that's all they give
#12.4 Mg/ha * 100 = 1240 g/m2 #10000 m2 = 1 ha; 1000000 g = 1 Mg
#4.4 Mg/ha * 100 = 440 g/m2



agb_mean <- 47.84 #mean aboveground (ag) biomass (gDW m-2)
agb_sd <- 28.98 #standard deviation of mean ag biomass
agn_mean <- 0.01466 #mean % Nitrogen of ag biomass as a decimal (gN gDW-1)
agn_sd <- 0.00124 #standard deviation of mean % N of ag biomass
bgb_mean <- 191.47 #mean belowground (bg) biomass (gDW m-2)
bgb_sd <- 135.67 #standard deviation of mean bg biomass
bgn_mean <- 0.008285 #mean % Nitrogen of bg biomass as a decimal (gN gDW-1)
bgn_sd <- 0.000804 #standard deviation of mean % N of bg biomass
sedn_mean <- 1240 #mean Nitrogen in top 1m of sediment (gN m-2)
sedn_sd <- 440 #standard deviation of mean N in sediment
units <- 1000000000000 #value to divide by to get reasonable units of Nitrogen (1000000 gives Mg (MgN = 1000 kg), 1000000000000 gives Tg)

cell_area <- 16 #area of each cell in m2, which is the same for every cell, it's the resolution of the raster, which is product of 4mx4m

nms <- c("ag_n_total", "bg_n_total", "sed_n_total", "area_total") #will be column names below
its <- 10 #iterations

result_list <- vector(mode = "list", length = length(names_tif))


#For 10 its of this loop (just nitrogen), Bahamas takes 10 hrs, Cuba takes 5.8 hrs, everything else combined takes ~1 hr (DR takes 7.7 min, Haiti takes 10.2 min, Jamaica takes 5.2 min)
for (i in 1:length(names_tif)) {
  
  rat_temp <- foreign::read.dbf(names_rat[i]) #rat=raster attribute table #import rat from database
  
  len_d <- rat_temp$Count[rat_temp$Value=="13"] # number of rows in the dense_data data frame; 13 in rat = dense seagrass
  len_s <- rat_temp$Count[rat_temp$Value=="12"] # number of rows in the sparse_data data frame; 12 in rat = sparse seagrass
  #the next 2 functions check for scenarios in which one of the seagrass categories doesn't exist, and replaces len_d or len_s with 0
  #if you don't run them, you would get numeric(0) for missing values, which returns an error in functions
  if (length(len_d) == 0) {
    len_d = 0
  } else {
    len_d = len_d
  }
  if (length(len_s) == 0) {
    len_s = 0
  } else {
    len_s = len_s
  }
  
  # create nmat
  n_mat <- matrix(NA_real_, nrow = its, ncol = length(nms), dimnames = list(NULL, nms)) #nitrogen matrix
  
  for (j in 1:its) {
    
    message("\rProgress its: ", j, " / ", its, "; Progress tif: ", i, " / ", length(names_tif), "\t\t", appendLF = FALSE)
    
    #dense
    
    #ag_n is: random number from top half of abg distribution * random number from agn distribution from every cell all added together
    ag_n <- rcpp_calc_carbon(n = len_d, trunc_min = agb_mean, trunc_max = Inf, trunc_mean = agb_mean, trunc_sd = agb_sd, norm_mean = agn_mean, norm_sd = agn_sd)
                #only pulls from top half of ag biomass distribution b/c dense #multiplies by number from distribution of percent Nitrogen in ag biomass #end units of grams Nitrogen per m2 (g m-2 * gN g-1 = gN m-2)
    bg_n <- rcpp_calc_carbon(n = len_d, trunc_min = bgb_mean, trunc_max = Inf, trunc_mean = bgb_mean, trunc_sd = bgb_sd, norm_mean = bgn_mean, norm_sd = bgn_sd)
                #only pulls from top half of bg biomass distribution b/c dense #multiplies by number from distribution of percent Nitrogen in bg biomass #end units of grams Nitrogen per m2 (g m-2 * gN g-1 = gN m-2)
    sed_n <- rcpp_calc_sed(n = len_d, norm_mean = sedn_mean, norm_sd = sedn_sd) #distribution of Nitrogen in sediment (gN m-2)
                #~13 minutes to run sed_n for len_s from The Bahamas (2497100661, the largest number); ~16min to run bg_n for len_s from The Bahamas
    
    #calc dense total 
    d_total_ag_n <- (ag_n * cell_area) / units #takes total ag_n number from above, multiplies by cell area to get total ag nitrogen in g Nitrogen; units brings it to Tg (or Mg or whatever is in the units variable)
    d_total_bg_n <- (bg_n * cell_area) / units
    d_total_sed_n <- (sed_n * cell_area) / units
    d_total_area <- len_d * cell_area
    
    rm(ag_n, bg_n, sed_n) # this forgets the vectors to save memory
    
    #sparse
    
    ag_n <- rcpp_calc_carbon(n = len_s, trunc_min = 0, trunc_max = agb_mean, trunc_mean = agb_mean, trunc_sd = agb_sd, norm_mean = agn_mean, norm_sd = agn_sd)
    #only pulls from bottom half of ag biomass distribution b/c sparse #multiplies by number from distribution of percent Nitrogen in ag biomass #end units of grams Nitrogen per m2 (g m-2 * gN g-1 = gN m-2)
    bg_n <- rcpp_calc_carbon(n = len_s, trunc_min = 0, trunc_max = bgb_mean, trunc_mean = bgb_mean, trunc_sd = bgb_sd, norm_mean = bgn_mean, norm_sd = bgn_sd)
    #only pulls from top half of bg biomass distribution b/c dense #multiplies by number from distribution of percent Nitrogen in bg biomass #end units of grams Nitrogen per m2 (g m-2 * gN g-1 = gN m-2)
    sed_n <- rcpp_calc_sed(n = len_s, norm_mean = sedn_mean, norm_sd = sedn_sd) #distribution of Nitrogen in sediment (gN m-2)
    
    #calc sparse total
    s_total_ag_n <- (ag_n * cell_area) / units
    s_total_bg_n <- (bg_n * cell_area) / units
    s_total_sed_n <- (sed_n * cell_area) / units
    s_total_area <- len_s * cell_area
    
    rm(ag_n, bg_n, sed_n)
    
    
    #sum dense and sparse
    n_mat[j, ] <- c(ag_n_total = d_total_ag_n + s_total_ag_n, 
                    bg_n_total = d_total_bg_n + s_total_bg_n,
                    sed_n_total = d_total_sed_n + s_total_sed_n,
                    area_total = d_total_area + s_total_area)
    
  } #end j loop
  
  result_list[[i]] <- n_mat
  
  
} #end i loop

names_only_tif <- list.files("data/caribbean_maps/", pattern = "*.tif$", full.names = FALSE) %>% 
  stringr::str_sort()
names(result_list) <- names_only_tif

saveRDS(result_list, file="outputs/nitrogen_list_carib.rds") #DO THIS!!!!!!!!!!

#result_df <- dplyr::bind_rows(result_list, .id = "id")

result_list <- readRDS("outputs/nitrogen_list_carib.rds")




