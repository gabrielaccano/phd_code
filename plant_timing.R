#Plants this time!

library(vegan)
library(tidyverse)
library(rlang)
library(readxl)
library(ggridges)
library(writexl)
library(ggpubr)

setwd("C:/Users/tut43799/OneDrive - Temple University/Documents/GitHub/phd_code")
matrix_98_24<- read_xlsx("butterfly_data_16_24/gcc_complete_pollard_09_02_25.xlsx")

filtered_na_matrix<-matrix_98_24 |> 
  filter(nectar_species_cleaned != "NA", behavior==3) |> 
  group_by(nectar_species_cleaned)|>   
  filter(n()>=20) |> 
  ungroup()

#all nectar plants timing
ggplot(filtered_na_matrix, aes(x=julian, y= nectar_species_cleaned, fill= nectar_species_cleaned))+
  geom_density_ridges()
#table of earliest, latest, and mean date for each nectar species across community

nectar_summary <- filtered_na_matrix |> 
  group_by(nectar_species_cleaned) |> 
  summarise(
    earliest_julian = min(julian, na.rm = TRUE),
    latest_julian   = max(julian, na.rm = TRUE),
    mean_julian     = mean(julian, na.rm = TRUE),
    .groups = "drop"
  )

view(nectar_summary)
#same, but just for regal plants

rg_filtered_matrix<- filtered_na_matrix |> 
  filter(butterfly_species_cleaned=="ARID")

rg_nectar_summary <- rg_filtered_matrix |> 
  group_by(nectar_species_cleaned) |> 
  summarise(
    earliest_julian = min(julian, na.rm = TRUE),
    latest_julian   = max(julian, na.rm = TRUE),
    mean_julian     = mean(julian, na.rm = TRUE),
    .groups = "drop"
  )
view(rg_nectar_summary)


view(filtered_na_matrix |> 
       filter(nectar_species_cleaned=="CIDI",
              julian<170))

regal_plants<-filtered_na_matrix |> 
  filter(nectar_species_cleaned %in% c("APOCY", "ASSY", "ASTU", "CENTA", "MOFI2", "CIPU4", "CIDI"))

rg_regal_plants<-regal_plants |> 
  filter(butterfly_species_cleaned=="ARID")

all_focal_plants<- ggplot(regal_plants, aes(x=julian, y= nectar_species_cleaned, fill= nectar_species_cleaned))+
  geom_density_ridges()+
  labs(title= "Plant timing across community")+
  xlim(140, 300)

rg_focal_plants<- ggplot(rg_regal_plants, aes(x=julian, y= nectar_species_cleaned, fill= nectar_species_cleaned))+
  geom_density_ridges()+
  labs(title= "Plant timing of regals")+
  xlim(140, 300)

ggarrange(all_focal_plants, rg_focal_plants,
          ncol=1, nrow=2)

#post-2010 data

filter_2010<-filtered_na_matrix |> 
  filter(year>=2010)

nectar_summary_2010 <- filtered_na_matrix |> 
  group_by(nectar_species_cleaned) |> 
  summarise(
    earliest_julian = min(julian, na.rm = TRUE),
    latest_julian   = max(julian, na.rm = TRUE),
    mean_julian     = mean(julian, na.rm = TRUE),
    .groups = "drop"
  )

view(nectar_summary_2010)
