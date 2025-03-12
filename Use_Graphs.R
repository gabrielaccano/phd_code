#Look at flowers regals are using
library(tidyverse)
library(NatParksPalettes)
library (readxl)

setwd("C:/Users/GCano/Documents/GitHub/phd_code/")
original_matrix<- read_excel("Matrix_nt_2016-04-28 - Copy.xlsx", sheet = "Matrix")
updated_p_and_b<- read_excel("Matrix_nt_2016-04-28 - Copy.xlsx", sheet= "updated_p_and_b")
#fix dataframe naming -----------------

updated_matrix<-original_matrix
# Create a named vector for plant_name -> plant_code mapping
plant_code_map <- setNames(updated_p_and_b$plant_code, updated_p_and_b$plant_name)

# Replace nectar_species in original_matrix with corresponding plant_code
updated_matrix$nectar_species <- sapply(updated_matrix$nectar_species,
                                         function(x) plant_code_map[x])

# Create a named vector for plant_name -> plant_code mapping
butterfly_code_map <- setNames(updated_p_and_b$butterfly_code, updated_p_and_b$butterfly_name)

# Replace nectar_species in original_matrix with corresponding plant_code
updated_matrix$butterfly_species <- sapply(updated_matrix$butterfly_species,
                                            function(x) butterfly_code_map[x])
#pre-thistle replacement
write.csv(updated_matrix,"C:/Users/GCano/Documents/GitHub/phd_code/pre_thistle_matrix_gcc_03_07_25.csv", row.names = FALSE)

#Include missing values for 1998-2004 and fix thistle
mutated_matrix<- updated_matrix |> 
  mutate(butterfly_species= ifelse(Year>= 1998 & Year<=2004, "ARID", butterfly_species)) |> 
  mutate(nectar_species = case_when(
    Year >= 1998 & Year <= 2003 & Julian> 220 & nectar_species == "CIPU4" ~ "CIDI",
    TRUE ~ nectar_species))|> 
  mutate(Comments = case_when(
    Year >= 1998 & Year <= 2003 & Julian > 220 & nectar_species == "CIDI" ~ 
      "Changed from CIPU4 to CIDI on 03/07/25 by GCC with approval from FIG",
    TRUE ~ ifelse(is.na(Comments), "", Comments)  # Ensure comment is not NA
  ))

#Assign weeks based on Julian Day
week_matrix<- mutated_matrix |> 
  mutate (Week=ceiling((Julian-122)/7))

#most updated matrix
write.csv(week_matrix,"C:/Users/GCano/Documents/GitHub/phd_code/updated_matrix_gcc_03_07_25.csv", row.names = FALSE)

#regals -------------------------

#Only regals
regal_df<- week_matrix |> 
  filter(butterfly_species== "ARID")

#New df with behavior is nectaring
regal_remove_na<- regal_df |> 
  filter(!is.na(Sex)) |> 
  filter(Behavior==3) #|> 
  #filter(nectar_species %in% c("ASTU", "MOFI2", "CIDI", "CIPU4", "ASSY","CENTA"))#|> 
  #filter(Year %in% c(2000, 2003, 2006, 2009, 2012, 2015))

#Count of behaviors by year
regal_behavior_count <- regal_remove_na %>%
  group_by(Week, Julian, nectar_species, Year, Sex) %>%
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

table(regal_df$Sex, regal_df$Year)
   

ggplot(regal_behavior_count, aes(x=Julian, y=factor(Year), fill=nectar_species))+
  geom_density_ridges (scale= 1, alpha=.7)+
  facet_wrap(~factor(Sex, levels = c("M", "F")))+
  scale_y_discrete(limits=rev)+
  labs(title= "Nectar plant usage over time by male and female Eastern Regal Fritillaries",
       x= "Julian Day", y="Year", fill= "Nectar Species")+
  scale_fill_manual(labels= c("APOCY"= "Dogbane", 
                                "ASSY"= "Common Milkweed", 
                                "ASTU"= "Butterfly Milkweed", 
                                "CENTA"= "Knapweed", 
                                "CIDI"= "Field Thistle", 
                                "CIPU4"= "Pasture Thistle", 
                                "MOFI2"= "Bee Balm"),
                    values= okabe_ito_extended)+
  theme_minimal()


#Just females
female_regal_behavior<- regal_behavior_count |> 
  filter(Sex=="F")

ggplot(female_regal_behavior, aes(x=Julian, y=factor(Year), fill=nectar_species))+
  geom_density_ridges (scale= 1, alpha=.5)+
  scale_y_discrete(limits=rev)+
  labs(title= "Nectar plant usage over time by female Eastern Regal Fritillaries",
       x= "Julian Day", y="Year", fill= "Nectar Species")+
  scale_fill_manual(labels= c("APOCY"= "Dogbane", 
                              "ASSY"= "Common Milkweed", 
                              "ASTU"= "Butterfly Milkweed", 
                              "CENTA"= "Knapweed", 
                              "CIDI"= "Field Thistle", 
                              "CIPU4"= "Pasture Thistle", 
                              "MOFI2"= "Bee Balm"), 
                    values= c("#009E73", "#F0E442", "#0072B2",
                              "#D55E00", "#CC79A7"))+
  theme_minimal()

okabe_ito_extended <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
  "#D55E00", "#CC79A7", "#999999",  # Original Okabe-Ito (8 colors)
  "#882255", "#44AA99", "#117733", "#88CCEE", "#DDCC77",
  "#AA4499", "#332288", "#661100", "#BBBBBB", # Previously added (9 colors)
  "#6699CC", "#AA7744"  # Two additional colors
)
#gsa and mona -----------

#filter for only GSA and Monarch
gsa_mona_df<- week_matrix |> 
  filter(butterfly_species== "ARCA" | butterfly_species== "DAPL")

#choose 6 most common nectaring plants
gsa_remove_na<- gsa_mona_df |> 
  filter(Behavior==3) |> 
  filter(butterfly_species== "ARCA") |> 
  filter(nectar_species != "NA")#|> 
  #filter(nectar_species %in% c("ASTU", "MOFI2", "CIDI", "CIPU4", "ASSY","CENTA"))
  

# regal_behavior_count <- regal_remove_na %>%
#   group_by(Week, nectar_species, Year, Sex) %>%
#   summarise(count = n(), .groups = "drop") |> 
#   filter(Sex != "MF", Sex!= "UNK")

#Plot that bitch!
ggplot(gsa_remove_na, aes(x=Julian, y= factor(Year), fill=nectar_species))+
  geom_density_ridges (scale= 1, alpha=.5)+
  facet_wrap(~butterfly_species)+
  scale_y_discrete(limits=rev)+
  labs(title= "Nectar plant usage over time by Great Spangled/Aphrodite Fritillaries",
       x= "Julian Day", y="Year", fill= "Nectar Species")+
  scale_fill_manual(labels= c("APOCY"= "Dogbane", 
                                "ASSY"= "Common Milkweed", 
                                "ASTU"= "Butterfly Milkweed", 
                                "CENTA"= "Knapweed", 
                                "CIDI"= "Field Thistle", 
                                "CIPU4"= "Pasture Thistle", 
                                "MOFI2"= "Bee Balm",
                                "ACHIL"= "Yarrow",
                                "AGERA2"= "Snakeroot",
                                "ASIN"= "Swamp Milkweed",
                                "CIAR4"= "Canada Thistle",
                                "DACA6"= "Queen Anne's Lace",
                                "EUPAT"= "Eupatorium",
                                "EUPE3"= "Boneset",
                                "LEVU"= "Oxeye Daisy",
                                "PYCNA"= "Mountain Mint",
                                "ROMU"= "Multiflora Rose",
                                "SECUR3"= "Crownvetch",
                                "SOLID"= "Goldenrod"),
                    values= okabe_ito_extended)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "#f1eeeb", color= NA),
        plot.background = element_rect(fill= "#f1eeeb", color= NA))

table(gsa_mona_df$butterfly_species, gsa_mona_df$Year)

#looking into mofi -------------------------------

mofi_matrix<- week_matrix |> 
  filter(Behavior==3) |> 
  filter(nectar_species== c("MOFI2", "ASSY", "ASTU", "CENTA", "CIDI", "CIPU4")) |> 
  filter(Year>2005) #|> 
 # filter(Julian> 175)

mofi_count<- mofi_matrix |> 
  group_by (Year, Julian, butterfly_species, nectar_species) |> 
  summarise(MOFI2_count = n ())

ggplot(mofi_matrix, aes(x=Julian, y= factor(Year), fill= butterfly_species))+
  geom_density_ridges(scale= 1, alpha=.5)+
  theme_minimal()

ggplot(mofi_matrix, aes(x = Year, color= nectar_species, alpha = ifelse(nectar_species == "MOFI2", 1, .999))) +
  geom_line(stat = "count", size= 1.5) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "red") + 
  scale_x_continuous(breaks = seq(min(mofi_matrix$Year), max(mofi_matrix$Year), by = 1))+# Marks MOFI2 introduction
  scale_color_manual(values= okabe_ito_extended,
                     labels= c(
                       "ASSY"= "A. syriaca",
                       "ASTU"= "A. tuberosa",
                       "CENTA"= "Centaurea sp.",
                       "CIDI"= "C. discolor",
                       "CIPU4"= "C. pumilum",
                       "MOFI2"= "M. fistulosa"))+
  labs(x = "Year", y = "Number of Observations", color = "Nectar sp") +
  theme_minimal()+
  guides(alpha="none")

# Create a contingency table: Before vs. After 2010
behavior_table <- table(mofi_matrix$Year >= 2010, mofi_matrix$Behavior)

# Perform chi-square test
chisq.test(behavior_table)