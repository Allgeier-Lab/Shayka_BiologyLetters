###########
#This code takes .tif files and .tif.vat.dbf files and calculates phosphorus storage in that area of seagrass
#And produces graphs of the data
#This is for all of the Caribbean
#Created by Bridget Shayka on 12-20-21
#Last modified by Bridget Shayka on 12-20-21
##########

##Load libraries -------------
library(foreign)
library(tidyverse)
library(reshape2)
library(Rcpp) #need for C++ loop function


##Load functions ------------
sourceCpp("scripts/rcpp_calc_carbon.cpp")
sourceCpp("scripts/rcpp_calc_sed.cpp")
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
#Mean (+-SD) %P for AG biomass 0.054 +- 0.008 % of DW from Layman et al. 2016 EcolEng Supp Appendix A for Thalassia testudinum in The Bahamas

#Mean (+-SD) BG biomass 191.47 +- 135.67 gDW m-2 from Fourqurean et al. 2012 NatGeosci data for only Caribbean Thalassia communities
#Mean (+-SD) %P for BG biomass 0.0125 +- 0.003354 % of DW from Layman et al. 2016 EcolEng Supp Appendix A for Thalassia testudinum in The Bahamas
#Root: 0.015 +- 0.006, Rhizome: 0.010 +- 0.003
#(0.015+0.010)/2 #= 0.0125
#.5*sqrt(((0.006)^2)+((0.003)^2)) #= 0.003354102

#Mean (+-SD) sediment P 82 +- 36 gP m-2 in top 1m of sediment from Fourqurean et al. 2012 MarFWRes for FL Bay and Shark Bay 
#These P numbers are for Florida Bay and Shark Bay, which are very different, but there was not a significant difference in P between locations (p>0.05), so I am using the overall average and sd b/c that's all they give
#0.82 Mg/ha * 100 = 82 g/m2 #10000 m2 = 1 ha; 1000000 g = 1 Mg
#0.36 Mg/ha * 100 = 36 g/m2



agb_mean <- 47.84 #mean aboveground (ag) biomass (gDW m-2)
agb_sd <- 28.98 #standard deviation of mean ag biomass
agp_mean <- 0.00054 #mean % Phosphorus of ag biomass as a decimal (gP gDW-1)
agp_sd <- 0.00008 #standard deviation of mean % P of ag biomass
bgb_mean <- 191.47 #mean belowground (bg) biomass (gDW m-2)
bgb_sd <- 135.67 #standard deviation of mean bg biomass
bgp_mean <- 0.000125 #mean % Phosphorus of bg biomass as a decimal (gP gDW-1)
bgp_sd <- 0.00003354 #standard deviation of mean % P of bg biomass
sedp_mean <- 82 #mean Phosphorus in top 1m of sediment (gP m-2)
sedp_sd <- 36 #standard deviation of mean P in sediment
units <- 1000000000000 #value to divide by to get reasonable units of Nitrogen (1000000 gives Mg (MgN = 1000 kg), 1000000000000 gives Tg, 1 gives g)

cell_area <- 16 #area of each cell in m2, which is the same for every cell, it's the resolution of the raster, which is product of 4mx4m

nms <- c("ag_p_total", "bg_p_total", "sed_p_total", "area_total") #will be column names below
its <- 10 #iterations

result_list <- vector(mode = "list", length = length(names_tif))


#For 10 its of this loop (just phosphorus), Bahamas takes 10 hrs, Cuba takes 5.8 hrs, everything else combined takes ~1 hr (DR takes 7.7 min, Haiti takes 10.2 min, Jamaica takes 5.2 min)
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
  
  # create pmat
  p_mat <- matrix(NA_real_, nrow = its, ncol = length(nms), dimnames = list(NULL, nms)) #phosphorus matrix
  
  for (j in 1:its) {
    
    message("\rProgress its: ", j, " / ", its, "; Progress tif: ", i, " / ", length(names_tif), "\t\t", appendLF = FALSE)
    
    #dense
    
    #ag_p is: random number from top half of abg distribution * random number from agp distribution from every cell all added together
    ag_p <- rcpp_calc_carbon(n = len_d, trunc_min = agb_mean, trunc_max = Inf, trunc_mean = agb_mean, trunc_sd = agb_sd, norm_mean = agp_mean, norm_sd = agp_sd)
                #only pulls from top half of ag biomass distribution b/c dense #multiplies by number from distribution of percent Phosphorus in ag biomass #end units of grams Phosphorus per m2 (g m-2 * gP g-1 = gP m-2)
    bg_p <- rcpp_calc_carbon(n = len_d, trunc_min = bgb_mean, trunc_max = Inf, trunc_mean = bgb_mean, trunc_sd = bgb_sd, norm_mean = bgp_mean, norm_sd = bgp_sd)
                #only pulls from top half of bg biomass distribution b/c dense #multiplies by number from distribution of percent Phosphorus in bg biomass #end units of grams Phosphorus per m2 (g m-2 * gP g-1 = gP m-2)
    sed_p <- rcpp_calc_sed(n = len_d, norm_mean = sedp_mean, norm_sd = sedp_sd) #distribution of Phosphorus in sediment (gP m-2)
                #~13 minutes to run sed_p for len_s from The Bahamas (2497100661, the largest number); ~16min to run bg_p for len_s from The Bahamas
    
    #calc dense total 
    d_total_ag_p <- (ag_p * cell_area) / units #takes total ag_p number from above, multiplies by cell area to get total ag phosphorus in g Phosphorus; units brings it to Tg (or Mg or whatever is in the units variable)
    d_total_bg_p <- (bg_p * cell_area) / units
    d_total_sed_p <- (sed_p * cell_area) / units
    d_total_area <- len_d * cell_area
    
    rm(ag_p, bg_p, sed_p) # this forgets the vectors to save memory
    
    #sparse
    
    ag_p <- rcpp_calc_carbon(n = len_s, trunc_min = 0, trunc_max = agb_mean, trunc_mean = agb_mean, trunc_sd = agb_sd, norm_mean = agp_mean, norm_sd = agp_sd)
    #only pulls from bottom half of ag biomass distribution b/c sparse #multiplies by number from distribution of percent Phosphorus in ag biomass #end units of grams Phosphorus per m2 (g m-2 * gP g-1 = gP m-2)
    bg_p <- rcpp_calc_carbon(n = len_s, trunc_min = 0, trunc_max = bgb_mean, trunc_mean = bgb_mean, trunc_sd = bgb_sd, norm_mean = bgp_mean, norm_sd = bgp_sd)
    #only pulls from top half of bg biomass distribution b/c dense #multiplies by number from distribution of percent Phosphorus in bg biomass #end units of grams Phosphorus per m2 (g m-2 * gP g-1 = gP m-2)
    sed_p <- rcpp_calc_sed(n = len_s, norm_mean = sedp_mean, norm_sd = sedp_sd) #distribution of Nitrogen in sediment (gN m-2)
    
    #calc sparse total
    s_total_ag_p <- (ag_p * cell_area) / units
    s_total_bg_p <- (bg_p * cell_area) / units
    s_total_sed_p <- (sed_p * cell_area) / units
    s_total_area <- len_s * cell_area
    
    rm(ag_p, bg_p, sed_p)
    
    
    #sum dense and sparse
    p_mat[j, ] <- c(ag_p_total = d_total_ag_p + s_total_ag_p, 
                    bg_p_total = d_total_bg_p + s_total_bg_p,
                    sed_p_total = d_total_sed_p + s_total_sed_p,
                    area_total = d_total_area + s_total_area)
    
  } #end j loop
  
  result_list[[i]] <- p_mat
  
  
} #end i loop

names_only_tif <- list.files("data/caribbean_maps/", pattern = "*.tif$", full.names = FALSE) %>% 
  stringr::str_sort()
names(result_list) <- names_only_tif

saveRDS(result_list, file="outputs/phosphorus_list_carib.rds") #DO THIS!!!!!!!!!!

#result_df <- dplyr::bind_rows(result_list, .id = "id")

result_list <- readRDS("outputs/phosphorus_list_carib.rds")


######## to get sums across matrices
#output is a matrix with "its" number of rows where each row is the sum of a run for the whole Bahamas
#then can graph/average that matrix

Caribbean_sums_mat <- Reduce('+', result_list) #first line is the sum of all the first lines of the matrices in the list, second line is...
Caribbean_totals <- colMeans(Caribbean_sums_mat)
Caribbean_sds <- apply(Caribbean_sums_mat, 2, sd) #2 means apply by columns (1 would be by rows), sd is standard deviation function

Caribbean_phosphorus <- sum(Caribbean_totals[c(1:3)])
Caribbean_phosphorus_sd <- sapply(Caribbean_sds[c(1:3)], function(x) x^2 ) %>%
  sum() %>%
  sqrt()

##Graphs ---------------- 


long <- reshape2::melt(Caribbean_sums_mat) #changing the format of the matrix allows you to graph it
agbgsed <- long %>%
  filter(Var2 != "area_total") %>%
  group_by(Var2) %>%
  summarise(avg = mean(value),
            stdev = sd(value)) %>%
  ggplot() + 
  geom_col(aes(x=Var2, y=avg)) +
  geom_errorbar(aes(ymin=avg-stdev, ymax=avg+stdev, x=Var2), width=.2) +
  theme_classic() +
  labs(x= "Pool",
       y= "Phosphorus (Tg)",
       title= "Phosphorus Pools in Seagrass Beds of the Caribbean") +
  scale_x_discrete(labels = c("Aboveground", "Belowground", "Sediment"))

ggsave(filename = "Caribbean_phosphorus.pdf", path="outputs", plot=agbgsed, device = "pdf", width = 7, height = 6, units="in", dpi=300)

agbg <- long %>%
  filter(Var2 != "area_total", Var2 != "sed_p_total") %>%
  group_by(Var2) %>%
  summarise(avg = mean(value),
            stdev = sd(value)) %>%
  ggplot() + 
  geom_col(aes(x=Var2, y=avg)) +
  geom_errorbar(aes(ymin=avg-stdev, ymax=avg+stdev, x=Var2), width=.2) +
  theme_classic() +
  labs(x= "Pool",
       y= "Phosphorus (Tg)",
       title= "Phosphorus Pools in Seagrass of the Caribbean") +
  scale_x_discrete(labels = c("Aboveground", "Belowground"))

ggsave(filename = "Caribbean_seagrass_phosphorus.pdf", path="outputs", plot=agbg, device = "pdf", width = 7, height = 6, units="in", dpi=300)




