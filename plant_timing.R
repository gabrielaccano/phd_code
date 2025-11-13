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
  filter(nectar_species_cleaned != "NA", site!= c('Middle Creek', "BOYER")) |> 
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
  filter(butterfly_species_cleaned=="ARID") |> 
  filter(sex!= "NA")
  #filter(sex=="F")

all_focal_plants<- ggplot(regal_plants, aes(x=julian, y= nectar_species_cleaned, fill= nectar_species_cleaned))+
  geom_density_ridges()+
  labs(title= "Plant timing across community")+
  xlim(140, 300)

rg_focal_plants<- ggplot(rg_regal_plants, aes(x=julian, y= nectar_species_cleaned, fill= nectar_species_cleaned))+
  geom_density_ridges()+
  #facet_grid(~sex)+
  labs(title= "Plant timing of female regals")+
  xlim(140, 300)

ggarrange(all_focal_plants, rg_focal_plants,
          ncol=1, nrow=2)

#post-2010 data

filter_2010<-filtered_na_matrix |> 
  filter(year>=2010)

ggplot(filter_2010, aes(x=julian, y= nectar_species_cleaned, fill= nectar_species_cleaned))+
  geom_density_ridges()

nectar_summary_2010 <- filtered_na_matrix |> 
  group_by(nectar_species_cleaned) |> 
  summarise(
    earliest_julian = min(julian, na.rm = TRUE),
    latest_julian   = max(julian, na.rm = TRUE),
    mean_julian     = mean(julian, na.rm = TRUE),
    .groups = "drop"
  )


focal_nectar_summary_2010 <- regal_plants |> 
  group_by(nectar_species_cleaned) |> 
  summarise(
    earliest_julian = min(julian, na.rm = TRUE),
    latest_julian   = max(julian, na.rm = TRUE),
    mean_julian     = mean(julian, na.rm = TRUE),
    .groups = "drop"
  )

rg_nectar_summary_2010 <- rg_regal_plants |> 
  group_by(nectar_species_cleaned, sex) |> 
  summarise(
    earliest_julian = min(julian, na.rm = TRUE),
    latest_julian   = max(julian, na.rm = TRUE),
    mean_julian     = mean(julian, na.rm = TRUE),
    .groups = "drop"
  )

view(rg_nectar_summary_2010)
view(focal_nectar_summary_2010)

#monarchs

gsa_plants <-filter_2010 |> 
  filter(butterfly_species_cleaned=="ARCA")

gsa<-ggplot(gsa_plants, aes(x=julian, y= nectar_species_cleaned, fill= nectar_species_cleaned))+
  geom_density_ridges()+
  labs(title = "GSA")

nectar_summary_gsa <- gsa_plants |> 
  group_by(nectar_species_cleaned) |> 
  summarise(
    earliest_julian = min(julian, na.rm = TRUE),
    latest_julian   = max(julian, na.rm = TRUE),
    mean_julian     = mean(julian, na.rm = TRUE),
    .groups = "drop"
  )

ggarrange(all, gsa)

#cipu by year

cipu_matrix<- matrix_98_24 |> 
  filter(nectar_species_cleaned=='CIPU4')

ggplot(cipu_matrix, aes(x=julian, y= as.factor(year)))+
  geom_density_ridges()+
  labs(title= 'ASSY')

cipu_summary<- cipu_matrix |> 
  group_by(year) |> 
  summarise(
    earliest_julian = min(julian, na.rm = TRUE),
    latest_julian   = max(julian, na.rm = TRUE),
    mean_julian     = mean(julian, na.rm = TRUE),
    .groups = "drop"
  )

#assy by year

assy_matrix<- matrix_98_24 |> 
  filter(nectar_species_cleaned=='ASSY')

ggplot(assy_matrix, aes(x=julian, y= as.factor(year)))+
  geom_density_ridges()+
  labs(title= 'ASSY')

assy_summary<- assy_matrix |> 
  group_by(year) |> 
  summarise(
    earliest_julian = min(julian, na.rm = TRUE),
    latest_julian   = max(julian, na.rm = TRUE),
    mean_julian     = mean(julian, na.rm = TRUE),
    .groups = "drop"
  )

#astu
astu_matrix<- matrix_98_24 |> 
  filter(nectar_species_cleaned=='ASTU')

astu_summary<- astu_matrix |> 
  group_by(year) |> 
  summarise(
    earliest_julian = min(julian, na.rm = TRUE),
    latest_julian   = max(julian, na.rm = TRUE),
    mean_julian     = mean(julian, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(astu_matrix, aes(x=julian, y= as.factor(year)))+
  geom_density_ridges()+
  labs(title='ASTU')

#cidi
cidi_matrix<- matrix_98_24 |> 
  filter(nectar_species_cleaned=='CIDI')

ggplot(cidi_matrix, aes(x=julian, y= as.factor(year)))+
  geom_density_ridges()+
  labs(title= 'CIDI')

cidi_summary_ <- cidi_matrix |> 
  group_by(year) |> 
  summarise(
    earliest_julian = min(julian, na.rm = TRUE),
    latest_julian   = max(julian, na.rm = TRUE),
    mean_julian     = mean(julian, na.rm = TRUE),
    .groups = "drop"
  )

#mofi
mofi_matrix<- matrix_98_24 |> 
  filter(nectar_species_cleaned=='MOFI2')

mofi_summary<- mofi_matrix |> 
  group_by(year) |> 
  summarise(
    earliest_julian = min(julian, na.rm = TRUE),
    latest_julian   = max(julian, na.rm = TRUE),
    mean_julian     = mean(julian, na.rm = TRUE),
    median_julian = median(julian, na.rm= TRUE),
    .groups = "drop"
  )

ggplot(mofi_matrix, aes(x=julian, y= as.factor(year)))+
  geom_density_ridges()+
  labs(title="MOFI")

#point plots

#MOFI
mean_mofi<-ggplot(mofi_summary, aes(x=year, y=mean_julian))+
  geom_point()+ geom_smooth()+
  labs(title= " mean mofi")

early_mofi<-ggplot(mofi_summary, aes(x=year, y=earliest_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "earliest mofi")

late_mofi<-ggplot(mofi_summary, aes(x=year, y=latest_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "latest mofi")

ggarrange(mean_mofi, early_mofi, late_mofi)

#ASTU
mean_astu<-ggplot(astu_summary, aes(x=year, y=mean_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "mean astu")

early_astu<- ggplot(astu_summary, aes(x=year, y=earliest_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "earliest astu")

late_astu<- ggplot(astu_summary, aes(x=year, y=latest_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "latest astu")

ggarrange(mean_astu, early_astu, late_astu)

ggplot(astu_matrix, aes(x=julian, y=as.factor(year)))+
  geom_density_ridges()+
  labs(title= "astu timing")


#CIDI
mean_cidi<- ggplot(cidi_summary_, aes(x=year, y=mean_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "mean cidi")

early_cidi<-ggplot(cidi_summary_, aes(x=year, y=earliest_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "early cidi")

late_cidi<-ggplot(cidi_summary_, aes(x=year, y=latest_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "late cidi")

ggarrange(mean_cidi, early_cidi, late_cidi)


#ASSY
mean_assy<- ggplot(assy_summary, aes(x=year, y=mean_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "mean assy")

early_assy<- ggplot(assy_summary, aes(x=year, y=earliest_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "early assy")

late_assy<- ggplot(assy_summary, aes(x=year, y=latest_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "late assy")

ggarrange(mean_assy, early_assy, late_assy)

#CIPU
mean_cipu<- ggplot(cipu_summary, aes(x=year, y=mean_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "mean cipu")

early_cipu<- ggplot(cipu_summary, aes(x=year, y=earliest_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "early cipu")

late_cipu<- ggplot(cipu_summary, aes(x=year, y=latest_julian))+
  geom_point()+ geom_smooth()+
  labs(title= "late cipu")

ggarrange(mean_cipu, early_cipu, late_cipu)


#without gsa

no_gsa<- filter_2010 |> 
  filter(butterfly_species_cleaned!= "ARCA")

no_gsa_plot<-ggplot(no_gsa, aes(x=julian, y= nectar_species_cleaned, fill= nectar_species_cleaned))+
  geom_density_ridges()+
  labs(title= "NO GSA")


ggarrange(no_gsa_plot, gsa)

ggarrange(all, gsa)
