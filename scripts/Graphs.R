###########
#This code takes C, N, and P values calculated in Caribbean_C_storage (and N and P) and calculates dollar ($) values and graphs for manuscript
#Created by Bridget Shayka on 4-4-22
#Last modified by Bridget Shayka on 9-7-22
##########

##Load libraries -------------
library(tidyverse)
library(reshape2)
library(plotrix)
library(ggforce)
library(ggbreak)
#If you use ggbreak in published research, please cite the following paper:
#S Xu, M Chen, T Feng, L Zhan, L Zhou, G Yu. Use ggbreak to effectively utilize plotting space to deal with large datasets and outliers. Frontiers in Genetics. 2021, 12:774846. doi:10.3389/fgene.2021.774846 


##Load functions ------------



##Load data -----------------
Cresult_list <- readRDS("outputs/carbon_list_carib.rds")
Nresult_list <- readRDS("outputs/nitrogen_list_carib.rds")
Presult_list <- readRDS("outputs/phosphorus_list_carib.rds")

countries <- c("Anguilla", "Antigua & Barbuda", "Barbados", "Bahamas", "British Virgin Islands", "Cuba", "Cayman Islands", "Dominica", 
               "Dominican Republic", "Grenada", "Guadeloupe", "Haiti", "Jamaica", "Martinique", "Montserrat", "Puerto Rico", "Saba", 
               "St. Barthelemy", "St. Lucia", "St. Martin", "St. Eustatius", "St. Maarten", "St. Kitts & Nevis", 
               "St. Vincent & the Grenadines", "Turks & Caicos", "Trinidad & Tobago", "US Virgin Islands")


##Data analysis -------------
##C, N, and P totals for the Caribbean and #C, N, and P breakdown by AG, BG, and Sediment for Caribbean
#C
Caribbean_Csums_mat <- Reduce('+', Cresult_list) #first line is the sum of all the first lines of the matrices in the list, second line is...
Caribbean_Ctotals <- colMeans(Caribbean_Csums_mat)
Caribbean_Csds <- apply(Caribbean_Csums_mat, 2, sd) #2 means apply by columns (1 would be by rows), sd is standard deviation function

Caribbean_carbon <- sum(Caribbean_Ctotals[c(1:3)])
Caribbean_carbon_sd <- sapply(Caribbean_Csds[c(1:3)], function(x) x^2 ) %>%
  sum() %>%
  sqrt()

#N
Caribbean_Nsums_mat <- Reduce('+', Nresult_list) #first line is the sum of all the first lines of the matrices in the list, second line is...
Caribbean_Ntotals <- colMeans(Caribbean_Nsums_mat)
Caribbean_Nsds <- apply(Caribbean_Nsums_mat, 2, sd) #2 means apply by columns (1 would be by rows), sd is standard deviation function

Caribbean_nitrogen <- sum(Caribbean_Ntotals[c(1:3)])
Caribbean_nitrogen_sd <- sapply(Caribbean_Nsds[c(1:3)], function(x) x^2 ) %>%
  sum() %>%
  sqrt()

#P
Caribbean_Psums_mat <- Reduce('+', Presult_list) #first line is the sum of all the first lines of the matrices in the list, second line is...
Caribbean_Ptotals <- colMeans(Caribbean_Psums_mat)
Caribbean_Psds <- apply(Caribbean_Psums_mat, 2, sd) #2 means apply by columns (1 would be by rows), sd is standard deviation function

Caribbean_phosphorus <- sum(Caribbean_Ptotals[c(1:3)])
Caribbean_phosphorus_sd <- sapply(Caribbean_Psds[c(1:3)], function(x) x^2 ) %>%
  sum() %>%
  sqrt()

#C,N,P AG,BG,Sed
totals <- rbind(Caribbean_Ctotals, Caribbean_Ntotals, Caribbean_Ptotals)
rownames(totals) <- c("Carbon", "Nitrogen", "Phosphorus")
colnames(totals) <- c("Aboveground", "Belowground", "Sediment", "Area")
totals_df <- data.frame(totals)

sds <- rbind(Caribbean_Csds, Caribbean_Nsds, Caribbean_Psds)
rownames(sds) <- c("Carbon", "Nitrogen", "Phosphorus")
colnames(sds) <- c("ag_sd", "bg_sd", "sed_sd", "area_sd")
sds_df <- data.frame(sds)

cnp <- c(Caribbean_carbon, Caribbean_nitrogen, Caribbean_phosphorus)
cnp_sd <- c(Caribbean_carbon_sd, Caribbean_nitrogen_sd, Caribbean_phosphorus_sd)

cnp_df <- cbind(totals_df, sds_df) #this combines the two dfs by columns and doesn't overlap any of them like the join functions do
cnp_df$Total <- cnp
cnp_df$total_sd <- cnp_sd

cnp_df <- cnp_df %>%
  mutate(ag_per = (Aboveground/Total) * 100,
         bg_per = (Belowground/Total) * 100,
         sed_per = (Sediment/Total) * 100, #these give the % of the total C, N, or P that each component (AG, BG, and Sed) represents
         nutrient = c("Carbon", "Nitrogen", "Phosphorus")) 



##Value of C and seagrass by country
Cmeans_list <- lapply(Cresult_list, function(x) colMeans(x))
Cmeans_matrix <- matrix(unlist(Cmeans_list), ncol=4, byrow=T)
Cmeans_df <- data.frame(Cmeans_matrix)
Cbycountry <- cbind(countries, Cmeans_df) %>%
  rename(ag_c = X1,
         bg_c = X2,
         sed_c = X3,
         area = X4) %>%
  mutate(total_c = (ag_c + bg_c + sed_c), 
         c_value = (total_c * (18*(44/12)*1000000))/1000000000, #all carbon columns are in Tg, and carbon value is in $/tCO2e (1000000 tonnes = 1 Tg) #CO2 has an atomic mass of 44 #end value is in $ 
                                  #https://carbonpricingdashboard.worldbank.org/map_data #California cap and trade $18 for 2021 
         seagrass_value = ((area/10000) * 28916)/1000000000) #28916 $/ha/yr in 2011 from Costanza et al. 2014 #area is in m2 #so end value is in $/yr
                          #dividing by 1,000,000,000 gives the final number in billions of dollars!!

sum(Cbycountry$seagrass_value)
sum(Cbycountry$c_value)
sum(Cbycountry$area)

byCountry <- format(Cbycountry, scientific = F, digits = 3) #if you don't want the numbers in scientific notation

##C stock in Caribbean compared to...
#all of these are in Pg
CaribCPg <- Caribbean_carbon/1000
CaribCsdPg <- Caribbean_carbon_sd/1000
GlobalSG <- (4.2+8.4)/2
GlobalSGrange <- GlobalSG-4.2
MedSG <- ((1235.39+1733.29)/2)/1000
MedSGrange <- MedSG-(1235.39/1000) #this makes the values in Pg
Amazon <- 123
Amazonuncert <- 31
Tempforest <- 119
Tempforestsd <- 6
Animals <- 2
Animalsupuncert <- 2*5 
Animalslowuncert <- 2/5
Fish <- 0.7
Fishupuncert <- 0.7*8
Fishlowuncert <- 0.7/8


values <- c(CaribCPg, GlobalSG, MedSG, Amazon, Tempforest, Fish)
upuncerts <- c(CaribCsdPg, GlobalSGrange, MedSGrange, Amazonuncert, Tempforestsd, Fishupuncert)
lowuncerts <- c(CaribCsdPg, GlobalSGrange, MedSGrange, Amazonuncert, Tempforestsd, Fishlowuncert)
names <- c("Caribbean seagrass", "Global seagrass", "Mediterranean seagrass", "Amazon woody biomass", "Temperate forests", "Global fish")


comparisons_df <- data.frame(names, values, upuncerts, lowuncerts) %>%
  mutate(frac = round((1.336258/values), 2),
         x = "x") %>%
  unite('times', frac:x, remove=FALSE, sep= "") %>%
  mutate(relative = round((1.336258/values)*100, 2),
         per = "%") %>%
  unite('merged', relative:per, remove=FALSE, sep= "")

##Graphs ---------------- 
#C, N, and P totals for the Caribbean
cnp_graph <- cnp_df %>%
  ggplot(aes(x=nutrient, y=Total)) +
  geom_col() + 
  geom_errorbar(aes(ymin=Total-total_sd, ymax=Total+total_sd), width=.2) +
  theme_classic() +
  labs(x= "Nutrient",
       y= "Amount in Caribbean (Tg)") +
  geom_text(aes(label = c("1336.3 (0.4%)", "109.5 (0.2%)", "7.2 (0.05%)"), y = Total + 100))

ggsave(filename = "Caribbean_cnp.pdf", path="outputs", plot=cnp_graph, device = "pdf", width = 7, height = 6, units="in", dpi=300)

mb <- unique(as.numeric(1:10 %o% 10 ^ (0:3))) #this object creates a set of minor break values that are reasonable for log scales up to 10,000
cnp_graph2 <- cnp_df %>%
  ggplot(aes(x=nutrient, y=Total)) +
  geom_col() +
  geom_errorbar(aes(ymin=Total-total_sd, ymax=Total+total_sd), width=.2) +
  scale_y_continuous(trans='log10',
                     limits = c(NA,2000),
                     breaks = scales::breaks_log(),
                     minor_breaks = mb) + #this pulls minor breaks from the object above
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "darkgray"),
        panel.grid.minor.y = element_line(),
        aspect.ratio = 2/1) +  #makes the x axis narrower but keeps the height the same
#        plot.margin = margin(0, 0, 0, 0)) +
  labs(x= "Nutrient",
       y= expression('Amount in Caribbean (Tg) [Ticks placed on '*log[10]*' scale]')) + 
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=12, color = "black")) +
  geom_text(aes(label = c("1336.3\n(0.4%)", "109.5\n(0.2%)", "7.2\n(0.05%)"), y = Total - (Total*0.3)), color="white") #\n makes line breaks

ggsave(filename = "Caribbean_cnp_log10.pdf", path="outputs", plot=cnp_graph2, device = "pdf", width = 7, height = 6, units="in", dpi=300)


#C, N, and P breakdown by AG, BG, and Sediment for Caribbean
cnp_long <- cnp_df %>%
  select(ag_per, bg_per, sed_per, nutrient) %>%
  reshape2::melt()

cnp_per_graph <- cnp_long %>%
  ggplot() +
  geom_col(aes(x=variable, y=value, fill=nutrient), position = "dodge") +
  theme_classic() +
  labs(x= "Pool",
       y= "Percent of Total",
       title= "Carbon, Nitrogen, and Phosphorus Pools in Seagrass Beds of the Caribbean",
       fill = "Nutrient") +
  scale_x_discrete(labels = c("Aboveground", "Belowground", "Sediment"))

ggsave(filename = "Caribbean_cnp_pools.pdf", path="outputs", plot=cnp_per_graph, device = "pdf", width = 7, height = 6, units="in", dpi=300)



#Value of C and seagrass by country
values_long <- Cbycountry %>%
  select(countries, c_value, seagrass_value) %>%
  reshape2::melt()

value_graph <- values_long %>%
  ggplot() +
  geom_col(aes(x=countries, y=value, fill=variable), position = "dodge") +
  theme_classic() +
  labs(x= "Country",
       y= "Value (Billions of USD)",
       title= "Value of Carbon and Seagrass Beds in the Caribbean",
       fill = "Resource") +
  scale_fill_discrete(labels = c("Carbon", "Seagrass")) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #this rotates the x axis labels and makes them centered and readable


values_long2 <- Cbycountry %>%
  mutate(c3_value = c_value *2) %>%
  select(countries, seagrass_value, c3_value) %>%
  reshape2::melt()

value_graph2 <- values_long2 %>%
  ggplot() +
  geom_col(aes(x=forcats::fct_rev(forcats::fct_reorder(countries, value)), y=value, fill=variable), position = "dodge", show.legend = FALSE) +
  ggforce::facet_zoom(ylim = c(0, 6), zoom.size = 1.5) +
  scale_y_continuous( "Seagrass Value (Billions of USD)", 
                      sec.axis = sec_axis(~ . * 1/2, name = "Carbon Value (Billions of USD)")) +
  theme_classic() +
  labs(x= "Country",
       title= "Value of Carbon and Seagrass Beds in the Caribbean") +
  scale_fill_discrete(labels = c("Carbon", "Seagrass")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_text(color = "red", size=13),
        axis.text.y = element_text(color = "red"),
        axis.title.y.right = element_text(color = "turquoise4", size=13),
        axis.text.y.right = element_text(color = "turquoise4"),
        panel.border = element_rect(colour = "black", fill=NA))

ggsave(filename = "Caribbean_values2.pdf", path="outputs", plot=value_graph2, device = "pdf", width = 12, height = 6, units="in", dpi=300)

value_graph3 <- values_long2 %>%
  mutate(bin = value > 50) %>%
  ggplot(aes(x=forcats::fct_rev(forcats::fct_reorder(countries, value)), y=value, fill=variable)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  scale_fill_manual(values = c(c3_value = "#D48635", seagrass_value = "#7D75A8"), 
                       labels = c(c3_value = "Carbon", seagrass_value = "Seagrass")) +
  coord_flip() +
  scale_y_continuous( "Seagrass Value (Billions of USD per year)", 
                      sec.axis = sec_axis(~ . * 1/2, name = "Carbon Value (Billions of USD)")) +
  theme_classic() +
  ggbreak::scale_y_break(c(6, 60), scales = 1.5) + #this adds a break in the y axis and zooms in to the lower portion of the graph
  labs(x= "Country") +
  theme(axis.title.x = element_text(color = "#7D75A8", size=15),
        axis.text.x = element_text(color = "#7D75A8", size=15),
        axis.title.x.top = element_text(color = "#D48635", size=15),
        axis.text.x.top = element_text(color = "#D48635", size=15),
        axis.text.y = element_text(size=13, colour = "black"),
        axis.title.y = element_text(size=15, colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))

ggsave(filename = "Caribbean_values.pdf", path="outputs", plot=value_graph3, device = "pdf", width = 8, height = 6, units="in", dpi=300)

value_graph4 <- values_long2 %>%
  mutate(bin = value > 50) %>%
  ggplot() +
  geom_point(aes(x=forcats::fct_rev(forcats::fct_reorder(countries, value)), y=value, color=variable), position = "jitter", show.legend = FALSE) +
  facet_grid(. ~ bin, scale='free_x') +
  coord_flip() +
  scale_y_continuous( "Seagrass Value (Billions of USD)", 
                      sec.axis = sec_axis(~ . * 1/2, name = "Carbon Value (Billions of USD)")) +
  theme_classic() +
  theme(strip.text.x = element_blank()) +
  labs(x= "Country",
       title= "Value of Carbon and Seagrass Beds in the Caribbean") +
  scale_fill_discrete(labels = c("Carbon", "Seagrass")) +
  theme(axis.title.x = element_text(color = "red", size=12),
        axis.text.x = element_text(color = "red"),
        axis.title.x.top = element_text(color = "turquoise4", size=12),
        axis.text.x.top = element_text(color = "turquoise4"),
        panel.border = element_rect(colour = "black", fill=NA))



#C stock in Caribbean compared to...

comparisons_graph <- comparisons_df %>%
  ggplot() +
  geom_col(aes(x=forcats::fct_rev(forcats::fct_reorder(names, values)), y=values)) +
  theme_classic() +
  geom_errorbar(aes(ymin=values-uncerts, ymax=values+uncerts, x=names), width=.2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x= "Carbon Pool",
       title= "Comparison of Carbon Pools on Earth",
       y= "Carbon (Pg)")

comparisons_graph2 <- comparisons_df %>%
  ggplot() +
  geom_col(aes(x=forcats::fct_rev(forcats::fct_reorder(names, values)), y=values)) +
  ggforce::facet_zoom(ylim = c(0, 10), zoom.size = 1.5) +
  theme_classic() +
  geom_errorbar(aes(ymin=values-uncerts, ymax=values+uncerts, x=names), width=.2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x= "Carbon Pool",
       title= "Comparison of Carbon Pools on Earth",
       y= "Carbon (Pg)")

comparisons_graph3 <- comparisons_df %>%
  mutate(bin = values < 50) %>%
  ggplot() +
  geom_point(aes(x=forcats::fct_rev(forcats::fct_reorder(names, values)), y=values), size=c(1.5,1.5,0,1.5,1.5,1.5)) + #the zero basically gets rid of the mean point for Med seagrass
  facet_grid(bin ~ ., scale='free_y') +
  geom_errorbar(aes(ymin=values-lowuncerts, ymax=values+upuncerts, x=names), width=.2) +
  theme_classic() +
  theme(strip.text.y = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x= "Carbon Pool",
       title= "Comparison of Carbon Pools on Earth",
       y= "Carbon (Pg)") +
  geom_hline(yintercept = 12.5, linetype="dashed", color = "gray", size=.5)

comparisons_graph4 <- comparisons_df %>%
  mutate(bin = values > 50) %>%
  ggplot() +
  geom_point(aes(x=forcats::fct_rev(forcats::fct_reorder(names, values)), y=values), size=c(1.5,0,0,1.5,1.5,1.5)) + #the zero basically gets rid of the mean point for Med seagrass
  facet_grid(. ~ bin, scale='free_x') +
  coord_flip() +
  geom_errorbar(aes(ymin=values-lowuncerts, ymax=values+upuncerts, x=names), width=.2, size=0.8) +
  theme_classic() +
  theme(strip.text.x = element_blank()) +
  labs(x= "Carbon Pool",
       title= "Comparison of Carbon Pools on Earth",
       y= "Carbon (Pg)") +
  geom_hline(yintercept = 12.5, linetype="dashed", color = "gray", size=.5)

ggsave(filename = "GlobalComparisons.pdf", path="outputs", plot=comparisons_graph4, device = "pdf", width = 8, height = 7, units="in", dpi=300)

comparisons_graph5 <- comparisons_df %>%
  mutate(bin = values > 50) %>%
  ggplot() +
  geom_point(aes(x=forcats::fct_rev(forcats::fct_reorder(names, values)), y=values), size=c(5,0,0,5,5,5), color="forestgreen") + #the zero basically gets rid of the mean point for Med seagrass
  coord_flip() +
  geom_errorbar(aes(ymin=values-lowuncerts, ymax=values+upuncerts, x=names), width=.13, size=0.8) +
  theme_classic() +
  ggbreak::scale_y_break(c(11, 80), scales = 1.2) + #this adds a break in the y axis and zooms in to the lower portion of the graph
  labs(x= "Carbon Pool",
       y= "Carbon (Pg)") +
  theme(axis.text = element_text(size=17, color = "black"),
        axis.title = element_text(size=17, color = "black"),
        panel.background = element_rect(colour = "black", fill=NA)) +
#        plot.margin = margin(10, 10, 10, 10)) + #this makes it so the labels aren't cut off on the side
#  geom_hline(yintercept = 11.5, linetype="dashed", color = "gray", size=.5) +
  geom_text(x=names, y=values, label=comparisons_df$times,
            vjust = 2.5, hjust = .25, size=6)


ggsave(filename = "GlobalComparisons2.pdf", path="outputs", plot=comparisons_graph5, device = "pdf", width = 9, height = 7, units="in", dpi=300)

# comparisons_graph6 <- comparisons_df %>%
#   mutate(bin = values > 50) %>%
#   ggplot() +
#   geom_point(aes(x=forcats::fct_rev(forcats::fct_reorder(names, values)), y=values), size=c(5,0,0,5,5,5), color="forestgreen") + #the zero basically gets rid of the mean point for Med seagrass
#   geom_errorbar(aes(ymin=values-lowuncerts, ymax=values+upuncerts, x=names), width=.17, size=0.8) +
#   theme_classic() +
#   ggbreak::scale_y_break(c(11, 80), scales = 1.2) + #this adds a break in the y axis and zooms in to the lower portion of the graph
#   labs(x= "Carbon Pool",
#        y= "Carbon (Pg)") +
#   theme(axis.text = element_text(size=14),
#         axis.title = element_text(size=14)) +
#   #        plot.margin = margin(10, 10, 10, 10)) + #this makes it so the labels aren't cut off on the side
#   geom_hline(yintercept = 11.5, linetype="dashed", color = "gray", size=.5) +
#   geom_text(x=names, y=values, label=comparisons_df$merged,
#             vjust = 0, hjust = 0.05)
# 
# ggsave(filename = "GlobalComparisons3.pdf", path="outputs", plot=comparisons_graph6, device = "pdf", width = 9, height = 7, units="in", dpi=300)


