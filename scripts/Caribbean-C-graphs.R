###########
#This code takes C values calculated in Caribbean-C-seagrass-range.R and Caribbean-C-sediment-range.R and calculates dollar ($) values and graphs for manuscript
#Created by Bridget Shayka
##########

##Load libraries -------------
library(tidyverse)
library(reshape2)
library(plotrix)
library(ggforce)
library(ggbreak) #S Xu, M Chen, T Feng, L Zhan, L Zhou, G Yu. Use ggbreak to effectively utilize plotting space to deal with large datasets and outliers. Frontiers in Genetics. 2021, 12:774846. doi:10.3389/fgene.2021.774846 


##Load data -----------------
Cseagrass <- read_csv("Results/seagrass-carbon.csv")
Csediment <- read_csv("Results/sediment-carbon.csv")

#get names of all tif database
countries_attr_tables <- list.files(path = "data/caribbean_maps", pattern = "*.tif.vat.dbf$", full.names = TRUE, recursive = TRUE) |> 
  stringr::str_sort()

#create table with number of sparse and dense cells
areas <- purrr::map_dfr(countries_attr_tables, function(tab_i) {
  tab_temp <- foreign::read.dbf(tab_i) #rat=raster attribute table #import rat from database
  n_cells <- ifelse(test = length(tab_temp$Count[tab_temp$Value == "12"]) == 0, 
                   yes = 0, no = tab_temp$Count[tab_temp$Value == "12"]) + 
    ifelse(test = length(tab_temp$Count[tab_temp$Value == "13"]) == 0, 
           yes = 0, no = tab_temp$Count[tab_temp$Value == "13"])
  data.frame(n_cells = n_cells, country = stringr::str_split(tab_i, pattern = "/", 
                                                           simplify = TRUE)[, 3])}) %>%
  mutate(area = n_cells * 16) #each grid cell is 4m x 4m = 16 m2 area

countries <- c("Anguilla", "Antigua & Barbuda", "Barbados", "Bahamas", "British Virgin Islands", "Cuba", "Cayman Islands", "Dominica", 
               "Dominican Republic", "Grenada", "Guadeloupe", "Haiti", "Jamaica", "Martinique", "Montserrat", "Puerto Rico", "Saba", 
               "St. Barthelemy", "St. Lucia", "St. Martin", "St. Eustatius", "St. Maarten", "St. Kitts & Nevis", 
               "St. Vincent & the Grenadines", "Turks & Caicos", "Trinidad & Tobago", "US Virgin Islands")


##Data analysis -------------
##C totals for the Caribbean and C breakdown by AG, BG, and Sediment for Caribbean
Carib_Cseagrass <- Cseagrass %>%
  group_by(part, type, range, country) %>%
  summarise(means = mean(value)) %>%
  group_by(part, range, country) %>%
  summarise(typesums = sum(means)) %>%
  group_by(part,range) %>%
  summarise(sums = sum(typesums)) %>%  #units of gC m-2
  mutate(TgC = (sums*16)/1000000000000) #units of TgC

Carib_Csediment <- Csediment %>%
  group_by(type, range, country) %>%
  summarise(means = mean(value)) %>%
  group_by(type,range) %>%
  summarise(sums = sum(means)) %>% #units of gC m-2
  mutate(TgC = (sums*16)/1000000000000) %>% #units of TgC
  mutate(part = type)

#C AG,BG,Sed
Carib_Ctotals <- rbind(Carib_Cseagrass, Carib_Csediment) %>%
  select(part, range, TgC) %>%
  pivot_wider(names_from = range, values_from = TgC)

Caribbean_C <- sum(Carib_Ctotals$mean)
Caribbean_C_low <- sum(Carib_Ctotals$low)
Caribbean_C_high <- sum(Carib_Ctotals$high)

##Value of C and seagrass by country
Cseagrassbycountry <- Cseagrass %>%
  group_by(part, type, range, country) %>%
  summarise(means = mean(value)) %>%
  group_by(part, range, country) %>%
  summarise(typesums = sum(means)) %>%
  filter(range == "mean") %>%
  group_by(country) %>%
  summarise(sums = sum(typesums)) %>%  #units of gC m-2
  mutate(grassTgC = (sums*16)/1000000000000) %>% #units of TgC
  select(country, grassTgC)
  
Csedimentbycountry <- Csediment %>%
  group_by(type, range, country) %>%
  summarise(means = mean(value)) %>%
  filter(range == "mean") %>% #units of gC m-2
  mutate(sedTgC = (means*16)/1000000000000) %>% #units of TgC
  ungroup() %>%
  select(country, sedTgC)
  
Ctotalbycountry <- left_join(Cseagrassbycountry, Csedimentbycountry, by = "country") %>%
  left_join(areas, by = "country") %>%
  cbind(countries) %>%
  mutate(TgC = grassTgC + sedTgC,
         c_value = (TgC * (18*(44/12)*1000000))/1000000000, #carbon is in Tg, and carbon value is in $/tCO2e (1000000 tonnes = 1 Tg) #CO2 has an atomic mass of 44 #end value is in $ 
                                  #https://carbonpricingdashboard.worldbank.org/map_data #California cap and trade $18 for 2021 
         seagrass_value = ((area/10000) * 28916)/1000000000) #28916 $/ha/yr in 2011 from Costanza et al. 2014 #area is in m2 #so end value is in $/yr
                          #dividing by 1,000,000,000 gives the final number in billions of dollars!!

sum(Ctotalbycountry$seagrass_value)
sum(Ctotalbycountry$c_value)
sum(Ctotalbycountry$area)

byCountry <- format(Ctotalbycountry, scientific = F, digits = 3) #if you don't want the numbers in scientific notation


##C stock in Caribbean compared to...
#all of these are in Petagrams (Pg)
CaribCPg <- Caribbean_C/1000
CaribCPgup <- (Caribbean_C_high - Caribbean_C)/1000
CaribCPglow <- (Caribbean_C - Caribbean_C_low)/1000
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
upuncerts <- c(CaribCPgup, GlobalSGrange, MedSGrange, Amazonuncert, Tempforestsd, Fishupuncert)
lowuncerts <- c(CaribCPglow, GlobalSGrange, MedSGrange, Amazonuncert, Tempforestsd, Fishlowuncert)
names <- c("Caribbean seagrass", "Global seagrass", "Mediterranean seagrass", "Amazon woody biomass", "Temperate forests", "Global fish")


comparisons_df <- data.frame(names, values, upuncerts, lowuncerts) %>%
  mutate(frac = round((CaribCPg/values), 2),
         x = "x") %>%
  unite('times', frac:x, remove=FALSE, sep= "") %>%
  mutate(relative = round((CaribCPg/values)*100, 2),
         per = "%") %>%
  unite('merged', relative:per, remove=FALSE, sep= "")



##Graphs ---------------- 

#Value of C and seagrass by country
values_long2 <- Ctotalbycountry %>%
  mutate(c3_value = c_value *2) %>%
  select(countries, seagrass_value, c3_value) %>%
  reshape2::melt()

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


#C stock in Caribbean compared to...
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
  geom_text(x=names, y=values, label=comparisons_df$times,
            vjust = 2.5, hjust = .25, size=6)

ggsave(filename = "GlobalComparisons2.pdf", path="outputs", plot=comparisons_graph5, device = "pdf", width = 9, height = 7, units="in", dpi=300)


#C breakdown by AG, BG, and Sediment for Caribbean
mb <- unique(as.numeric(1:10 %o% 10 ^ (0:3))) #this object creates a set of minor break values that are reasonable for log scales up to 10,000

agbgsed_graph <- Carib_Ctotals %>%
  cbind(pool = c("Aboveground", "Belowground", "Sediment")) %>%
  ggplot() +
  geom_col(aes(x=pool, y=mean)) +
  geom_errorbar(aes(ymin=low, ymax=high, x=pool), width=.13, size=0.6) +
  scale_y_continuous(trans='log10',
                      limits = c(NA,3000),
                      breaks = scales::breaks_log(),
                      minor_breaks = mb) + #this pulls minor breaks from the object above
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "darkgray"),
         panel.grid.minor.y = element_line(),
         aspect.ratio = 1.65/1) +  #makes the x axis narrower but keeps the height the same
  labs(x= "Carbon Pool",
       y= expression('Carbon in Caribbean Seagrass Beds (Tg) [Ticks placed on '*log[10]*' scale]')) +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=12, color = "black"))

ggsave(filename = "Caribbean_carbon_pools.pdf", path="outputs", plot=agbgsed_graph, device = "pdf", width = 8, height = 6, units="in", dpi=300)


# #C, N, and P totals for the Caribbean
# 
# mb <- unique(as.numeric(1:10 %o% 10 ^ (0:3))) #this object creates a set of minor break values that are reasonable for log scales up to 10,000
# cnp_graph2 <- cnp_df %>%
#   ggplot(aes(x=nutrient, y=Total)) +
#   geom_col() +
#   geom_errorbar(aes(ymin=Total-total_sd, ymax=Total+total_sd), width=.2) +
#   scale_y_continuous(trans='log10',
#                      limits = c(NA,2000),
#                      breaks = scales::breaks_log(),
#                      minor_breaks = mb) + #this pulls minor breaks from the object above
#   theme_classic() +
#   theme(panel.grid.major.y = element_line(color = "darkgray"),
#         panel.grid.minor.y = element_line(),
#         aspect.ratio = 2/1) +  #makes the x axis narrower but keeps the height the same
#   labs(x= "Nutrient",
#        y= expression('Amount in Caribbean (Tg) [Ticks placed on '*log[10]*' scale]')) + 
#   theme(axis.text = element_text(size=12),
#         axis.title = element_text(size=12, color = "black")) +
#   geom_text(aes(label = c("1336.3\n(0.4%)", "109.5\n(0.2%)", "7.2\n(0.05%)"), y = Total - (Total*0.3)), color="white") #\n makes line breaks
# 
# ggsave(filename = "Caribbean_cnp_log10.pdf", path="outputs", plot=cnp_graph2, device = "pdf", width = 7, height = 6, units="in", dpi=300)


