#Look at flowers regals are using
library(tidyverse)

#regals -------------------------

#Include missing values for 1998-2004
mutated_matrix<- updated_matrix |> 
  mutate(butterfly_species= ifelse(Year>= 1998 & Year<=2004, "ARID", butterfly_species))

#Assign weeks based on Julian Day
week_matrix<- mutated_matrix |> 
  mutate (Week=ceiling((Julian-122)/7))

#Only regals
regal_df<- week_matrix |> 
  filter(butterfly_species== "ARID")

#New df with only 6 most common nectar plants and the behavior is nectaring
regal_remove_na<- regal_df |> 
  filter(!is.na(Sex)) |> 
  filter(Behavior==3) #|> 
  #filter(nectar_species %in% c("ASTU", "MOFI2", "CIDI", "CIPU4", "ASSY","CENTA"))#|> 
  #filter(Year %in% c(2000, 2003, 2006, 2009, 2012, 2015))

#Count of behaviors by year
regal_behavior_count <- regal_remove_na %>%
  group_by(Week, nectar_species, Year, Sex) %>%
  summarise(count = n(), .groups = "drop") |> 
  filter(Sex != "MF", Sex!= "UNK")

#Graph with count
# ggplot(regal_behavior_count, aes(x = Week, y = count, color = nectar_species, group = nectar_species)) +
#   geom_point()+
#   geom_line()+ # Create a line for each nectar_species
#   facet_wrap(~Year)

library(ggridges)

ggplot(regal_behavior_count, aes(x=Week, y=factor(Year), fill=nectar_species))+
  geom_density_ridges (scale= 1, alpha=.5)+
  facet_wrap(~Sex)+
  scale_y_discrete(limits=rev)+
  theme_minimal()
       
#gsa and mona -----------

#filter for only GSA and Monarch
gsa_mona_df<- week_matrix |> 
  filter(butterfly_species== "ARCA" | butterfly_species== "DAPL")

#choose 6 most common nectaring plants
gsa_remove_na<- gsa_mona_df |> 
  filter(Behavior==3) |> 
  filter(nectar_species %in% c("ASTU", "MOFI2", "CIDI", "CIPU4", "ASSY","CENTA"))
  

# regal_behavior_count <- regal_remove_na %>%
#   group_by(Week, nectar_species, Year, Sex) %>%
#   summarise(count = n(), .groups = "drop") |> 
#   filter(Sex != "MF", Sex!= "UNK")

#Plot that bitch!
ggplot(gsa_remove_na, aes(x=Week, y= factor(Year), fill=nectar_species))+
  geom_density_ridges (scale= 1, alpha=.5)+
  facet_wrap(~butterfly_species)+
  scale_y_discrete(limits=rev)+
  theme_minimal()

