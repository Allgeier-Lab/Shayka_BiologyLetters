###########
#This code takes .tif files and .tif.vat.dbf files and calculates carbon storage in that area of seagrass - uses values from the literature
#And produces graphs of the data
#This is for all of the Caribbean
#Created by Bridget Shayka on 12-15-21
#Last modified by Bridget Shayka on 3-4-22
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
#Mean (+-SD) %C for AG biomass 36.9 +- 2.5 % of DW from Fourqurean and Zieman 2002 Biogeochem Table 1 for Thalassia testudinum in Florida Bay
#Mean (+-SD) BG biomass 191.47 +- 135.67 gDW m-2 from Fourqurean et al. 2012 NatGeosci data for only Caribbean Thalassia communities
#Mean (+-SD) %C for BG biomass 30.890 +- 1.059 % of DW from Layman et al. 2016 EcolEng Supp Appendix A for Thalassia testudinum in The Bahamas
#Root: 30.085 +- 1.819, Rhizome: 31.695 +- 1.086
#(30.085+31.695)/2 #= 30.890
#.5*sqrt(((1.819)^2)+((1.086)^2)) #= 1.059
#Mean (+-SD) sediment C 15090 +- 4840 gC m-2 in top 1m of sediment from Howard et al. 2018 Table 3 for Tropical Western Atlantic (data from Fourqurean et al. 2012)
#150.9 Mg/ha * 100 = 15090 g/m2 #10000 m2 = 1 ha; 1000000 g = 1 Mg
#48.4 Mg/ha * 100 = 4840 g/m2



agb_mean <- 47.84 #mean aboveground (ag) biomass (gDW m-2)
agb_sd <- 28.98 #standard deviation of mean ag biomass
agc_mean <- 0.369 #mean % Carbon of ag biomass as a decimal (gC gDW-1)
agc_sd <- 0.025 #standard deviation of mean % C of ag biomass
bgb_mean <- 191.47 #mean belowground (bg) biomass (gDW m-2)
bgb_sd <- 135.67 #standard deviation of mean bg biomass
bgc_mean <- 0.30890 #mean % Carbon of bg biomass as a decimal (gC gDW-1)
bgc_sd <- 0.01059 #standard deviation of mean % C of bg biomass
sedc_mean <- 15090 #mean Carbon in top 1m of sediment (gC m-2)
sedc_sd <- 4840 #standard deviation of mean C in sediment
units <- 1000000000000 #value to divide by to get reasonable units of Carbon (1000000 gives Mg (MgC = 1000 kg), 1000000000000 gives Tg)

cell_area <- 16 #area of each cell in m2, which is the same for every cell, it's the resolution of the raster, which is product of 4mx4m

nms <- c("ag_c_total", "bg_c_total", "sed_c_total", "area_total") #will be column names below
its <- 10 #iterations

result_list <- vector(mode = "list", length = length(names_tif))


#For 1 its of this loop (just carbon), it takes 1hr 40min
#For 10 its of this loop (just carbon), Bahamas takes 10 hrs, Cuba takes 5.8 hrs, everything else combined takes ~1 hr (DR takes 7.7 min, Haiti takes 10.2 min, Jamaica takes 5.2 min)
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
  
  # create cmat
  c_mat <- matrix(NA_real_, nrow = its, ncol = length(nms), dimnames = list(NULL, nms)) #carbon matrix
  
  for (j in 1:its) {
    
    message("\rProgress its: ", j, " / ", its, "; Progress tif: ", i, " / ", length(names_tif), "\t\t", appendLF = FALSE)
    
    #dense
    
    #ag_c is: random number from top half of abg distribution * random number from agc distribution from every cell all added together
    ag_c <- rcpp_calc_carbon(n = len_d, trunc_min = agb_mean, trunc_max = Inf, trunc_mean = agb_mean, trunc_sd = agb_sd, norm_mean = agc_mean, norm_sd = agc_sd)
                #only pulls from top half of ag biomass distribution b/c dense #multiplies by number from distribution of percent Carbon in ag biomass #end units of grams Carbon per m2 (g m-2 * gC g-1 = gC m-2)
    bg_c <- rcpp_calc_carbon(n = len_d, trunc_min = bgb_mean, trunc_max = Inf, trunc_mean = bgb_mean, trunc_sd = bgb_sd, norm_mean = bgc_mean, norm_sd = bgc_sd)
                #only pulls from top half of bg biomass distribution b/c dense #multiplies by number from distribution of percent Carbon in bg biomass #end units of grams Carbon per m2 (g m-2 * gC g-1 = gC m-2)
    sed_c <- rcpp_calc_sed(n = len_d, norm_mean = sedc_mean, norm_sd = sedc_sd) #distribution of Carbon in sediment (gC m-2)
                #~13 minutes to run sed_c for len_s from The Bahamas (2497100661, the largest number); ~16min to run bg_c for len_s from The Bahamas
    
    #calc dense total 
    d_total_ag_c <- (ag_c * cell_area) / units #takes total ag_c number from above, multiplies by cell area to get total ag carbon in g Carbon; units brings it to Tg (or Mg or whatever is in the units variable)
    d_total_bg_c <- (bg_c * cell_area) / units
    d_total_sed_c <- (sed_c * cell_area) / units
    d_total_area <- len_d * cell_area
    
    rm(ag_c, bg_c, sed_c) # this forgets the vectors to save memory
    
    #sparse
    
    ag_c <- rcpp_calc_carbon(n = len_s, trunc_min = 0, trunc_max = agb_mean, trunc_mean = agb_mean, trunc_sd = agb_sd, norm_mean = agc_mean, norm_sd = agc_sd)
    #only pulls from bottom half of ag biomass distribution b/c sparse #multiplies by number from distribution of percent Carbon in ag biomass #end units of grams Carbon per m2 (g m-2 * gC g-1 = gC m-2)
    bg_c <- rcpp_calc_carbon(n = len_s, trunc_min = 0, trunc_max = bgb_mean, trunc_mean = bgb_mean, trunc_sd = bgb_sd, norm_mean = bgc_mean, norm_sd = bgc_sd)
    #only pulls from top half of bg biomass distribution b/c dense #multiplies by number from distribution of percent Carbon in bg biomass #end units of grams Carbon per m2 (g m-2 * gC g-1 = gC m-2)
    sed_c <- rcpp_calc_sed(n = len_s, norm_mean = sedc_mean, norm_sd = sedc_sd) #distribution of Carbon in sediment (gC m-2)
    
    #calc sparse total
    s_total_ag_c <- (ag_c * cell_area) / units
    s_total_bg_c <- (bg_c * cell_area) / units
    s_total_sed_c <- (sed_c * cell_area) / units
    s_total_area <- len_s * cell_area
    
    rm(ag_c, bg_c, sed_c)
    
    
    #sum dense and sparse
    c_mat[j, ] <- c(ag_c_total = d_total_ag_c + s_total_ag_c, 
                    bg_c_total = d_total_bg_c + s_total_bg_c,
                    sed_c_total = d_total_sed_c + s_total_sed_c,
                    area_total = d_total_area + s_total_area)
    
  } #end j loop
  
  result_list[[i]] <- c_mat
  
  
} #end i loop

names_only_tif <- list.files("data/caribbean_maps/", pattern = "*.tif$", full.names = FALSE) %>% 
  stringr::str_sort()
names(result_list) <- names_only_tif

saveRDS(result_list, file="outputs/carbon_list_carib.rds")

#result_df <- dplyr::bind_rows(result_list, .id = "id")

result_list <- readRDS("outputs/carbon_list_carib.rds")


######## to get sums across matrices
#output is a matrix with "its" number of rows where each row is the sum of a run for the whole Bahamas
#then can graph/average that matrix

Caribbean_sums_mat <- Reduce('+', result_list) #first line is the sum of all the first lines of the matrices in the list, second line is...
Caribbean_totals <- colMeans(Caribbean_sums_mat)
Caribbean_sds <- apply(Caribbean_sums_mat, 2, sd) #2 means apply by columns (1 would be by rows), sd is standard deviation function

Caribbean_carbon <- sum(Caribbean_totals[c(1:3)])
Caribbean_carbon_sd <- sapply(Caribbean_sds[c(1:3)], function(x) x^2 ) %>%
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
       y= "Carbon (Tg)",
       title= "Carbon Pools in Seagrass Beds of the Caribbean") +
  scale_x_discrete(labels = c("Aboveground", "Belowground", "Sediment"))

ggsave(filename = "Caribbean_carbon.pdf", path="outputs", plot=agbgsed, device = "pdf", width = 7, height = 6, units="in", dpi=300)

agbg <- long %>%
  filter(Var2 != "area_total", Var2 != "sed_c_total") %>%
  group_by(Var2) %>%
  summarise(avg = mean(value),
            stdev = sd(value)) %>%
  ggplot() + 
  geom_col(aes(x=Var2, y=avg)) +
  geom_errorbar(aes(ymin=avg-stdev, ymax=avg+stdev, x=Var2), width=.2) +
  theme_classic() +
  labs(x= "Pool",
       y= "Carbon (Tg)",
       title= "Carbon Pools in Seagrass of the Caribbean") +
  scale_x_discrete(labels = c("Aboveground", "Belowground"))

ggsave(filename = "Caribbean_seagrass_carbon.pdf", path="outputs", plot=agbg, device = "pdf", width = 7, height = 6, units="in", dpi=300)


#dotplots
agbgsed_pts <- long %>%
  filter(Var2 != "area_total") %>%
  ggplot(aes(x=Var2, y=value)) + 
  geom_jitter(width = 0.1) +
  theme_classic() +
  labs(x= "Pool",
       y= "Carbon (Tg)",
       title= "Carbon Pools in Seagrass Beds of the Caribbean") +
  scale_x_discrete(labels = c("Aboveground", "Belowground", "Sediment"))

ggsave(filename = "Caribbean_carbon_point.pdf", path="outputs", plot=agbgsed_pts, device = "pdf", width = 7, height = 6, units="in", dpi=300)


agbg_pts <- long %>%
  filter(Var2 != "area_total", Var2 != "sed_c_total") %>%
  ggplot(aes(x=Var2, y=value)) + 
  geom_jitter(width = 0.1) +
  theme_classic() +
  labs(x= "Pool",
       y= "Carbon (Tg)",
       title= "Carbon Pools in Seagrass of the Caribbean") +
  scale_x_discrete(labels = c("Aboveground", "Belowground"))

ggsave(filename = "Caribbean_seagrass_carbon_point.pdf", path="outputs", plot=agbg_pts, device = "pdf", width = 7, height = 6, units="in", dpi=300)

#boxplots
agbgsed_box <- long %>%
  filter(Var2 != "area_total") %>%
  ggplot(aes(x=Var2, y=value)) + 
  geom_boxplot() +
  theme_classic() +
  labs(x= "Pool",
       y= "Carbon (Tg)",
       title= "Carbon Pools in Seagrass Beds of the Caribbean") +
  scale_x_discrete(labels = c("Aboveground", "Belowground", "Sediment"))

ggsave(filename = "Caribbean_carbon_box.pdf", path="outputs", plot=agbgsed_box, device = "pdf", width = 7, height = 6, units="in", dpi=300)

agbg_box <- long %>%
  filter(Var2 != "area_total", Var2 != "sed_c_total") %>%
  ggplot(aes(x=Var2, y=value)) + 
  geom_boxplot() +
  theme_classic() +
  labs(x= "Pool",
       y= "Carbon (Tg)",
       title= "Carbon Pools in Seagrass of the Caribbean") +
  scale_x_discrete(labels = c("Aboveground", "Belowground"))

ggsave(filename = "Caribbean_seagrass_carbon_box.pdf", path="outputs", plot=agbg_box, device = "pdf", width = 7, height = 6, units="in", dpi=300)




#### Plot dist #### 


sourceCpp("scripts/rcpp_calc_carbon2.cpp")
sourceCpp("scripts/rcpp_calc_sed2.cpp")

# number of cells to use
cells <- 1000000

# pull one random number for each cell and save in vector
sed_cells_dist <- purrr::map_dbl(1:cells, function(i){  
  rcpp_calc_sed(n = 1, norm_mean = sedc_mean, norm_sd = sedc_sd, verbose = FALSE)
})

# plot density
seddist <- ggplot() + 
  geom_density(aes(x = sed_cells_dist)) + 
  labs(x = "Sediment Carbon value", y = "Density")




agd_cells_dist <- purrr::map_dbl(1:cells, function(i){  
  rcpp_calc_carbon(n = 1, trunc_min = agb_mean, trunc_max = Inf, trunc_mean = agb_mean, trunc_sd = agb_sd, norm_mean = agc_mean, norm_sd = agc_sd, verbose = FALSE)
})

agddist <- ggplot() + 
  geom_density(aes(x = agd_cells_dist)) + 
  labs(x = "AG dense Carbon value (AG dense biomass * AG %C)", y = "Density")


ags_cells_dist <- purrr::map_dbl(1:cells, function(i){  
  rcpp_calc_carbon(n = 1, trunc_min = 0, trunc_max = agb_mean, trunc_mean = agb_mean, trunc_sd = agb_sd, norm_mean = agc_mean, norm_sd = agc_sd, verbose = FALSE)
})

agsdist <- ggplot() + 
  geom_density(aes(x = ags_cells_dist)) + 
  labs(x = "AG sparse Carbon value (AG sparse biomass * AG %C)", y = "Density")



bgd_cells_dist <- purrr::map_dbl(1:cells, function(i){  
  rcpp_calc_carbon(n = 1, trunc_min = bgb_mean, trunc_max = Inf, trunc_mean = bgb_mean, trunc_sd = bgb_sd, norm_mean = bgc_mean, norm_sd = bgc_sd, verbose = FALSE)
})

bgddist <- ggplot() + 
  geom_density(aes(x = bgd_cells_dist)) + 
  labs(x = "BG dense Carbon value (BG dense biomass * BG %C)", y = "Density")



bgs_cells_dist <- purrr::map_dbl(1:cells, function(i){  
  rcpp_calc_carbon(n = 1, trunc_min = 0, trunc_max = bgb_mean, trunc_mean = bgb_mean, trunc_sd = bgb_sd, norm_mean = bgc_mean, norm_sd = bgc_sd, verbose = FALSE)
})

bgsdist <- ggplot() + 
  geom_density(aes(x = bgs_cells_dist)) + 
  labs(x = "BG sparse Carbon value (BG sparse biomass * BG %C)", y = "Density")


bgc_cells_dist <- purrr::map_dbl(1:cells, function(i){  
  rcpp_calc_sed(n = 1, norm_mean = bgc_mean, norm_sd = bgc_sd, verbose = FALSE)
})

bgcdist <- ggplot() + 
  geom_density(aes(x = bgc_cells_dist)) + 
  labs(x = "BG %Carbon value", y = "Density")

agc_cells_dist <- purrr::map_dbl(1:cells, function(i){  
  rcpp_calc_sed(n = 1, norm_mean = agc_mean, norm_sd = agc_sd, verbose = FALSE)
})

agcdist <- ggplot() + 
  geom_density(aes(x = agc_cells_dist)) + 
  labs(x = "AG %Carbon value", y = "Density")

ggpubr::ggarrange(agsdist, agddist, bgsdist, bgddist, agcdist, bgcdist, seddist, ncol=2, nrow = 4)

