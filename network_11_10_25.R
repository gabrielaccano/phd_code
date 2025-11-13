#Network Analysis
#Gabriela Cano
#Nov 10, 2025

#import packages
library(tidyverse)
library(readxl)
library(bipartite)

#read in data set
matrix<- read_excel("C:/Users/GCano/Documents/GitHub/phd_code/butterfly_data_16_24/gcc_complete_pollard_10_25_25.xlsx")

#Combine the problem data, combine these names to what Clayton and I agreed to (excel sheet describing combinations)
fix_matrix <- matrix |> 
  mutate(
    butterfly_species_cleaned = case_when(
      butterfly_species_cleaned == "ERIC" ~ "ERSP",
      butterfly_species_cleaned == "LICAM" ~ "LIART",
      butterfly_species_cleaned %in% c("POIN", "POCA") ~ "POSP",
      TRUE ~ butterfly_species_cleaned
    )
  )

#filter for years since all butterflies collected and observed nectar species
f_matrix<- fix_matrix |> 
  filter(!is.na(nectar_species_cleaned), year>=2007)

#filter out family distinctions (lycaneidae and pieridae)
g_sp_matrix<-f_matrix |>
  filter(!butterfly_species_cleaned%in% c("LYCA", "PIER", NA, "HEMA", "HESP")) |> 
  filter(!is.na(nectar_species_cleaned))
  
  
#Find butterfly species/groups with enough observations
highlight_butt<- g_sp_matrix |> 
  group_by(butterfly_species_cleaned, year) |> 
  summarise(obs_per_year = n(), .groups = "drop") |> 
  group_by(butterfly_species_cleaned) |> 
  summarize(years_pres= n_distinct(year),   #list the unique years that this species is present
            years= paste(sort(unique(year)), collapse= ", "),
            total_obs= sum(obs_per_year)) |> 
  arrange(desc(total_obs)) |> 
  filter(total_obs>10) #|> 
  filter(years_pres>=5) |> #only genus/species present for more than 5 years
  arrange(desc(total_obs))
#when limited to must be present for 5 years, 26 butterfly sp/genus 
#when not limited, 38 species included
  
#matrix with species to be used
use_butt<-g_sp_matrix |> 
 filter(butterfly_species_cleaned %in% highlight_butt$butterfly_species_cleaned)

#Number of unique nectar species for the focal butterfly species  
nunique_matrix<-g_sp_matrix |> 
    distinct(nectar_species_cleaned)

#making of the matrix (each unique butterfly and nectar plant occurrence counted for frequency)
net_butt <- table(use_butt$butterfly_species_cleaned, use_butt$nectar_species_cleaned) #cross product of columns 25 and 26 (butterfly_species_cleaned and nectar_species_cleaned)
net_df<- as.data.frame.matrix(net_butt)

#visualize the network
plotweb(net_df) #web
visweb(net_df) #heatmap

#network-level indices
net_indices<- networklevel(net_df, index=c("connectance", "nestedness", "H2"))
print(net_indices)

#connectance  nestedness          H2 
#0.2741597  10.3185293   0.2266022 

#connectance suggests about 27% of possible interactions occur
#very nested structure(specialists use a subset of generalist nectar plants)
#H2 suggested low specialization, most of the community is fairly generalized

#species-level indices
sp_indices<- specieslevel(net_butt, level= "both")
print(sp_indices)
