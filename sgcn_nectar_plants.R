#Butterfly favorite nectar plants for Lep Task Force Grant

library(tidyverse)
library(readxl)

matrix_98_24<-  read_excel("C:/Users/GCano/Documents/GitHub/phd_code/butterfly_data_16_25/complete pollard data CRT_October2025.xlsx")

focal_species <- c("ARID", "DAPL", "EUPH", "EUMA", "HELE", "LEEU", "SATI")

top10_nectar_per_butt <- matrix_98_24 |>
  filter(butterfly_species_cleaned %in% focal_species,
         !is.na(nectar_species_cleaned),
         nectar_species_cleaned != "NA") |>
  count(butterfly_species_cleaned, nectar_species_cleaned, name = "uses") |>
  group_by(butterfly_species_cleaned) |>
  arrange(desc(uses), .by_group = TRUE) |>
  slice_head(n = 10) |>
  ungroup()

top_10_overall<- matrix_98_24 |> 
  filter(butterfly_species_cleaned %in% focal_species) |> 
  count(nectar_species_cleaned, name ="count") |> 
  arrange (desc(count))
