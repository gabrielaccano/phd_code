#Look at flowers regals are using
library(tidyverse)
library(NatParksPalettes)
library (readxl)
library(gdata)
library(openxlsx)
library(ggridges)
library(ggpubr)
library(nnet)
library(cowplot) 
okabe_ito_extended <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#D55E00", "#0072B2","#CC79A7", "#999999",  # Original Okabe-Ito (8 colors)
  "#882255", "#44AA99", "#117733", "#88CCEE", "#DDCC77",
  "#AA4499", "#332288", "#661100", "#BBBBBB", # Previously added (9 colors)
  "#6699CC", "#AA7744",
  "white", "black", "turquoise", "hotpink" # Two additional colors
)

plant_colors<-c(
  "APOCY" = "hotpink",
  "ASIN" = "#56B4E9",
  "ASSY" = "#009E73",
  "ASTER" = "#F0E442",
  "ASTU" = "#D55E00",
  "CENTA" = "#0072B2",
  "CIAR4" = "#999999",
  "CIDI" = "#117733",
  "CIPU4" = "turquoise",
  "ERIGE2" = "#44AA99",
  "EUPAT" = "#88CCEE",
  "EUPE3" =  "#DDCC77",
  "EUTRO" = "#AA4499",
  "LESPE" = "#332288",
  "LEVU" = "#661100",
  "MOFI2" = "#882255",
  "Other" = "#BBBBBB",
  "PENS" = "#6699CC",
  "PRUNE" = "#AA7744",
  "PYCNA" = "purple",
  "ROMU" = "black",
  "SOLID" = "#E69F00"
)

setwd("C:/Users/GCano/Documents/GitHub/phd_code/")
original_matrix<- read_excel("Matrix_nt_2016-04-28 - Copy.xlsx", sheet = "Matrix")
# updated_p_and_b<- read_excel("Matrix_nt_2016-04-28 - Copy.xlsx", sheet= "updated_p_and_b")

original_matrix_cleaned<-original_matrix |> 
  mutate(nectar_species_cleaned= nectar_species,
         butterfly_species_cleaned= butterfly_species)

original_matrix<- read_excel("updated_matrix_gcc_03_11_25.xlsx", sheet = "mutated_matrix")
updated_b<- read_excel("updated_matrix_gcc_03_11_25.xlsx", sheet= "butterfly_codes")
updated_p<- read_excel("updated_matrix_gcc_03_11_25.xlsx", sheet= "plant_codes")
#fix dataframe naming -----------------

updated_matrix<-original_matrix_cleaned
# Create a named vector for plant_name -> plant_code mapping
plant_code_map <- setNames(updated_p$plant_code, updated_p$plant_name)

# Replace nectar_species_cleaned in original_matrix with corresponding plant_code
updated_matrix$nectar_species_cleaned <- sapply(updated_matrix$nectar_species_cleaned,
                                         function(x) plant_code_map[x])

# Create a named vector for plant_name -> plant_code mapping
butterfly_code_map <- setNames(updated_b$butterfly_code, updated_b$butterfly_name)

# Replace nectar_species_cleaned in original_matrix with corresponding plant_code
updated_matrix$butterfly_species_cleaned <- sapply(updated_matrix$butterfly_species_cleaned,
                                            function(x) butterfly_code_map[x])
#pre-thistle replacement
write.csv(updated_matrix,"C:/Users/GCano/Documents/GitHub/phd_code/pre_thistle_matrix_gcc_03_07_25.csv", row.names = FALSE)

#Include missing values for 1998-2004 and fix thistle
mutated_matrix<- updated_matrix |> 
  mutate(butterfly_species_cleaned= ifelse(Year>= 1998 & Year<=2004, "ARID", butterfly_species_cleaned)) |> 
  mutate(nectar_species_cleaned = case_when(
    Year >= 1998 & Year <= 2003 & Julian> 220 & nectar_species_cleaned == "CIPU4" ~ "CIDI",
    TRUE ~ nectar_species_cleaned))|> 
  mutate(Comments = case_when(
    Year >= 1998 & Year <= 2003 & Julian > 220 & nectar_species_cleaned == "CIDI" ~ 
      "Changed from CIPU4 to CIDI on 03/07/25 by GCC with approval from FIG",
    TRUE ~ ifelse(is.na(Comments), "", Comments)  # Ensure comment is not NA
  ))

#Assign weeks based on Julian Day
week_matrix<- mutated_matrix |> 
  mutate (Week=ceiling((Julian-122)/7))

#most updated matrix
write.xlsx(week_matrix,"C:/Users/GCano/Documents/GitHub/phd_code/updated_matrix_gcc_03_07_25.csv", row.names = FALSE)

#regals -------------------------

matrix_98_24<- read_xlsx("C:/Users/GCano/Documents/GitHub/phd_code/butterfly_data_16_24/gcc_complete_pollard_04_30_25.xlsx",
                         col_types = c(
                           "numeric",
                           "numeric",
                           "date",
                           "numeric",
                           "numeric",
                           "text",
                           "text",
                           "text",
                           "text",
                           "text",
                           "numeric",
                           "numeric",
                           "numeric",
                           "numeric",
                           "numeric",
                           "numeric",
                           "numeric",
                           "text",
                           "text",
                           "text",
                           "numeric",
                           "text",
                           "text",
                           "text"
                         ))

week_matrix<- matrix_98_24 |> 
       mutate(week = isoweek(date))

str(matrix_98_24)

matrix_98_24<- matrix_98_24_pre_clean |> 
  select(year, month, date, week, julian, observer, site, start, end, duration,s_percent_sun, e_percent_sun, s_wind, e_wind, s_temp,
         e_temp, total, individual, section, sex, behavior, nectar_species_cleaned, butterfly_species_cleaned, comments)
  

#Only regals
regal_df<- matrix_98_24 |> 
  filter(butterfly_species_cleaned == "ARID")

#New df with behavior is nectaring
regal_remove_na<- regal_df |> 
  filter(sex=="M"| sex=="F") |> 
  filter(behavior==3) |> 
  filter(nectar_species_cleaned!= "NA") |> 
  filter(nectar_species_cleaned %in% c("ASTU", "ASSY", "CIDI", "CENTA", "CIPU4", "MOFI2"))

#Count of behaviors by year
regal_behavior_count <- regal_remove_na %>%
  group_by(week, julian, nectar_species_cleaned, year, sex) %>%
  summarise(count = n(), .groups= "keep") 

sum(regal_behavior_count$count)
view(regal_behavior_count |> 
       distinct(nectar_species_cleaned))

  # filter(sex != "MF", sex!= "UNK", sex!= "NA")

ggplot(regal_behavior_count, aes(x=Week, y=factor(Year), fill=nectar_species_cleaned))+
  geom_density_ridges (scale= 1, alpha=.5)+
  facet_wrap(~Sex)+
  scale_y_discrete(limits=rev)+
  theme_minimal()

table(regal_df$Sex, regal_df$Year)

summary(regal_remove_na$julian)

view(regal_behavior_count|> 
          filter(year==2019))
   
#plotting density values--eliminates small values (regal nectar use over years)
ggplot(regal_remove_na, aes(x=julian, y=factor(year), fill=nectar_species_cleaned))+
  geom_density_ridges(scale= 1, alpha=.5, rel_min_height= .1)+
  facet_wrap(~factor(sex, levels = c("M", "F")))+
  scale_y_discrete(limits=rev)
  labs(#title= "Nectar plant usage over time by male and female Eastern Regal Fritillaries",
       x= "Julian Day", y="Year", fill= "Nectar Species")+
  scale_x_continuous()+
 scale_fill_manual(labels= c("APOCY"= "Apocynum spp.", 
                           "ASSY"= "Asclepias syriaca", 
                             "ASTU"= "Asclepias tuberosa", 
                             "CENTA"= "Centaurea sp.", 
                              "CIDI"= "Cirsium discolor", 
                             "CIPU4"= "Cirsium pumilum", 
                            "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea sp.",
                             "AGERA2"= "Ageratina sp",
                            "ASIN"= "Asclepias incarnata",
                             "CIAR4"= "Cirsium arvense",
                            "DACA6"= "Daucus carota",
                             "EUPAT"= "Eupatorium sp.",
                            "EUPE3"= "Eupatorium perfoliatum",
                             "LEVU"= "Leucanthemum vulgare",
                            "PYCNA"= "Pycanthemum sp.",
                              "ROMU"= "Rosa multiflora",
                           "SECUR3"= "Securigera sp.",
                             "SOLID"= "Solidago spp."))+
   theme(legend.text = element_text(face = "italic"),
         axis.text.x = element_text(color = "black"),
         axis.text.y = element_text(color = "black"))
  
#regal nectar use overall, not by year  
ggplot(regal_remove_na, aes(x=julian, fill=nectar_species_cleaned))+
    geom_density(alpha= .5)+
    facet_wrap(~factor(sex, levels = c("M", "F")))+
    # scale_y_discrete(limits=rev)
  labs(#title= "Nectar plant usage over time by male and female Eastern Regal Fritillaries",
    x= "Julian Day", fill= "Nectar Species")+
    scale_x_continuous()+
    scale_fill_manual(values= okabe_ito_extended,
      labels= c("APOCY"= "Apocynum spp.", 
                                "ASSY"= "Asclepias syriaca", 
                                "ASTU"= "Asclepias tuberosa", 
                                "CENTA"= "Centaurea sp.", 
                                "CIDI"= "Cirsium discolor", 
                                "CIPU4"= "Cirsium pumilum", 
                                "MOFI2"= "Monarda fistulosa",
                                "ACHIL"= "Achillea sp.",
                                "AGERA2"= "Ageratina sp",
                                "ASIN"= "Asclepias incarnata",
                                "CIAR4"= "Cirsium arvense",
                                "DACA6"= "Daucus carota",
                                "EUPAT"= "Eupatorium sp.",
                                "EUPE3"= "Eupatorium perfoliatum",
                                "LEVU"= "Leucanthemum vulgare",
                                "PYCNA"= "Pycanthemum sp.",
                                "ROMU"= "Rosa multiflora",
                                "SECUR3"= "Securigera sp.",
                                "SOLID"= "Solidago spp."))+
    theme(legend.text = element_text(face = "italic"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"))

view(regal_remove_na |> 
       distinct(nectar_species_cleaned))

ggplot(regal_remove_na, aes(x=julian, fill= nectar_species_cleaned))+
  geom_histogram()+
  facet_wrap(~sex)

#calculate proportion----------------------------------------------
regal_week<- week_matrix |> 
  filter(butterfly_species_cleaned_cleaned== "ARID",
         nectar_species_cleaned!= "NA",
        sex=="M"| sex=="F",
        behavior==3,
        nectar_species_cleaned %in% c("ASTU", "ASSY", "APOCY", "CENTA", "CIDI", "CIPU4", "MOFI2"))

view(regal_week |> 
       distinct(week))

prop_data_regal <- regal_week |>
  group_by(year, week, nectar_species_cleaned, sex) |>
  summarize(count = n_distinct(nectar_species_cleaned), .groups = "drop") |>
  group_by(year, week) |>
  mutate(total = sum(count),
         proportion = count / total)

ggplot(prop_data_regal, aes(x=as.factor(week), y= proportion, fill= nectar_species_cleaned))+
  geom_bar(stat= "identity")+
  scale_fill_manual(values = okabe_ito_extended)+
  facet_grid(year ~ sex, scales = "free_y", space = "free") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          strip.text.y.left = element_text(angle = 0),   # Horizontal text
          strip.placement = "outside") +
          facet_grid(rows = vars(year), cols = vars(sex), switch = "y")

erf_prop_all_plant<-ggplot(prop_data_regal, aes(x= as.factor(week), y= proportion, fill= nectar_species_cleaned))+
  geom_bar(stat= "identity", position= "fill")+
  facet_wrap(~factor(sex, levels = c("M", "F")))+
  scale_fill_manual(values= okabe_ito_extended)+
  labs(title= "Eastern Regal")
  

?geom_density_ridges
#plotting all data points---------------------
ggplot(regal_behavior_count, aes(x=Julian, y=factor(Year), fill=nectar_species_cleaned))+
  geom_density_ridges (scale= 1, alpha=.7, stat= "binline", bins= 20)+
  facet_wrap(~factor(Sex, levels = c("M", "F")))+
  scale_y_discrete(limits=rev)+
  labs(title= "Nectar plant usage over time by male and female Eastern Regal Fritillaries",
       x= "Julian Day", y="Year", fill= "Nectar Species")+
  scale_x_continuous(breaks= seq(min(regal_behavior_count$Julian), 
                                 max(regal_behavior_count$Julian), 
                                 by = 20))+
  scale_fill_manual(values= okabe_ito_extended)+
  theme_minimal()


ggplot(regal_remove_na, aes(x=julian, y=factor(year), fill=nectar_species_cleaned))+
  geom_violin (scale= "width", draw_quantiles = c(0.25, 0.5, 0.75))+
  facet_wrap(~factor(sex, levels = c("M", "F")))+
  scale_y_discrete(limits=rev)+
  labs(title= "Nectar plant usage over time by male and female Eastern Regal Fritillaries",
       x= "Julian Day", y="Year", fill= "Nectar Species")
  # scale_x_continuous(breaks= seq(min(regal_behavior_count$Julian), 
  #                                max(regal_behavior_count$Julian), 
  #                                by = 20))

#Just females
female_regal_behavior<- regal_behavior_count |> 
  filter(Sex=="F") |> 
  filter(Julian>170, Julian <275)

ggplot(female_regal_behavior, aes(x=Julian, y=factor(Year), fill=nectar_species_cleaned))+
  geom_density_ridges (scale= 1, alpha=.5, rel_min_height= .1)+
  scale_y_discrete(limits=rev)+
  labs(title= "Nectar plant usage over time by female Eastern Regal Fritillaries",
       x= "Julian Day", y="Year", fill= "Nectar Species")+
  scale_fill_manual( 
                    values= c("#009E73", "#F0E442", "#0072B2",
                              "#D55E00", "#CC79A7"))+
  theme_minimal()

#gsa and mona -----------

#filter for only GSA and Monarch
gsa_mona_df<- week_matrix |> 
  filter(butterfly_species_cleaned_cleaned== "ARCA" | butterfly_species_cleaned_cleaned== "DAPL")

#choose 6 most common nectaring plants
gsa_remove_na<- week_matrix |> 
  filter(behavior==3) |> 
  filter(butterfly_species_cleaned_cleaned== "ARCA") |> 
  filter(nectar_species_cleaned != "NA")#|> 
  #filter(nectar_species_cleaned %in% c("ASTU", "MOFI2", "CIDI", "CIPU4", "ASSY","CENTA"))
  

# regal_behavior_count <- regal_remove_na %>%
#   group_by(Week, nectar_species_cleaned, Year, Sex) %>%
#   summarise(count = n(), .groups = "drop") |> 
#   filter(Sex != "MF", Sex!= "UNK")

#Plot that bitch!
ggplot(gsa_remove_na, aes(x=julian, y= factor(year), fill=nectar_species_cleaned))+
  geom_density_ridges (scale= 1, alpha=.5)+
  facet_wrap(~butterfly_species_cleaned_cleaned)+
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

table(gsa_mona_df$butterfly_species_cleaned, gsa_mona_df$Year)

#GSA's use of highlight sp
gsa_ft_species<- gsa_remove_na |> 
  filter(nectar_species_cleaned_cleaned %in% c("ASTU", "ASSY", "CIDI", "CENTA", "CIPU4", "MOFI2"))

ggplot(gsa_ft_species, aes(x=julian, y= factor(year), fill=nectar_species_cleaned))+
  geom_density_ridges (scale= 1, alpha=.5)+
  facet_wrap(~butterfly_species_cleaned_cleaned)+
  scale_y_discrete(limits=rev)+
  labs(title= "Nectar plant usage over time by Great Spangled/Aphrodite Fritillaries",
       x= "Julian Day", y="Year", fill= "Nectar Species")+
  scale_fill_manual(values= okabe_ito_extended)
#proportion of use GSA
prop_data_gsa <- gsa_remove_na |>
  group_by(year, week, nectar_species_cleaned, sex) |>
  summarize(count = n_distinct(nectar_species_cleaned_cleaned), .groups = "drop") |>
  group_by(year, week) |>
  mutate(total = sum(count),
         proportion = count / total)

ggplot(prop_data_gsa, aes(x=as.factor(week), y= proportion, fill= nectar_species_cleaned_cleaned))+
  geom_bar(stat= "identity")+
 facet_wrap(~year)

gsa_all_plant_prop<- ggplot(prop_data_gsa, aes(x= as.factor(week), y= proportion, fill= nectar_species_cleaned_cleaned))+
  geom_bar(stat= "identity", position= "fill")+
  labs(title="Great Spangled and Aphrodite")

ggarrange(gsa_all_plant_prop, erf_prop_all_plant)

#looking into mofi -------------------------------

mofi_matrix<- week_matrix |> 
  filter(Behavior==3) |> 
  filter(nectar_species_cleaned_cleaned== c("MOFI2", "ASSY", "ASTU", "CENTA", "CIDI", "CIPU4")) |> 
  filter(Year>2005) #|> 
 # filter(Julian> 175)

mofi_count<- mofi_matrix |> 
  group_by (Year, Julian, butterfly_species_cleaned_cleaned, nectar_species_cleaned) |> 
  summarise(MOFI2_count = n ())

ggplot(mofi_matrix, aes(x=Julian, y= factor(Year), fill= butterfly_species_cleaned))+
  geom_density_ridges(scale= 1, alpha=.5)+
  theme_minimal()

ggplot(mofi_matrix, aes(x = Year, color= nectar_species_cleaned, alpha = ifelse(nectar_species_cleaned == "MOFI2", 1, .999))) +
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
#ARCA and ARID ----------------------------------
gsa_regal<- matrix_98_24|> 
  filter(behavior==3) |> 
  filter(butterfly_species_cleaned== "ARCA"| butterfly_species_cleaned== "ARID") |> 
  filter(nectar_species_cleaned != "NA") |> 
  filter(year>2004)  
  # group_by(week, julian, nectar_species_cleaned, year, butterfly_species_cleaned) |> 
  # summarise(count = n(), .groups= "keep") 

ggplot(gsa_regal, aes(x=julian, y=factor(year), fill=nectar_species_cleaned))+
  geom_density_ridges (scale= 1, alpha=.7, rel_min_height= .1, panel_scaling= TRUE)+
  facet_wrap(~butterfly_species_cleaned, labeller = labeller(butterfly_species_cleaned = c("ARCA"= "Argynnis cybele/aphrodite", "ARID"= "Argynnis i. idalia")))+
  scale_y_discrete(limits=rev)+
  labs(#title= "Nectar plant usage over time by male and female Eastern Regal Fritillaries",
    x= "Julian Day", y="Year", fill= "Nectar Species")+
  # scale_x_continuous(breaks= seq(min(regal_behavior_count$Julian), 
  #max(regal_behavior_count$Julian), 
  #by = 20))+
  scale_fill_manual(values= okabe_ito_extended,
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca", 
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea sp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea sp.",
                              "AGERA2"= "Ageratina sp",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium sp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum sp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera sp.",
                              "SOLID"= "Solidago spp.",
                              "PACKE"= "Packera sp.",
                                "RUBUS"= "Rubus sp.",
                              "ROCA4"= "Rosa carolina",
                              "RUHI2"= "Rudbeckia hirta"))+
  theme(legend.text = element_text(face = "italic"),
        strip.text= element_text(face= "italic"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))+
  guides(fill = guide_legend(ncol = 1))


gsa<- matrix_98_24|> 
  filter(behavior==3) |> 
  filter(butterfly_species_cleaned== "ARCA") |> 
  filter(nectar_species_cleaned != "NA") |> 
  filter(year>2004)

ggplot(gsa, aes(x=julian, y=factor(year), fill=nectar_species_cleaned))+
  geom_density_ridges (scale= 1, alpha=.7, rel_min_height= .1, panel_scaling= TRUE)+
  # facet_wrap(~butterfly_species_cleaned, labeller = labeller(butterfly_species_cleaned = c("ARCA"= "Argynnis cybele/aphrodite", "ARID"= "Argynnis i. idalia")))+
  scale_y_discrete(limits=rev)+
  labs(#title= "Nectar plant usage over time by male and female Eastern Regal Fritillaries",
    x= "Julian Day", y="Year", fill= "Nectar Species")+
  # scale_x_continuous(breaks= seq(min(regal_behavior_count$Julian), 
  #max(regal_behavior_count$Julian), 
  #by = 20))+
  scale_fill_manual(values= c("#999999","#882255","#E69F00", "#44AA99","#56B4E9","#009E73","#F0E442",
                              "#117733","#0072B2","#D55E00","#88CCEE","#DDCC77","#AA4499","#332288",
                              "#CC79A7", "#661100", "#BBBBBB",
                              "#6699CC", "#AA7744", "brown"),
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca", 
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea sp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea sp.",
                              "AGERA2"= "Ageratina sp",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium sp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum sp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera sp.",
                              "SOLID"= "Solidago spp."))+
  theme(legend.text = element_text(face = "italic"),
        strip.text= element_text(face= "italic"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))


#Grouped by site
regal_no_plot<- regal_remove_na |> 
  filter(site %in% c("B12","B12A", "C4", "D1", "D3", "R23"))

ggplot(regal_no_plot, aes(x=julian, y=factor(year), fill=nectar_species_cleaned))+
  geom_density_ridges (scale= 1, alpha=.5, rel_min_height= .1)+
  facet_grid(~site)+
  scale_y_discrete(limits=rev)

regal_thistle<- regal_remove_na |> 
  filter(nectar_species_cleaned=="CIDI") |> 
  filter(site %in% c("B12", "B12A", "C4", "D1", "D3", "R23"))

ggplot(regal_thistle, aes(x=site, fill= factor(year)))+
  geom_bar()

regal_plant_count<- regal_remove_na |> 
  # filter(site=="D3") |> 
  group_by(year, nectar_species_cleaned, site) |>
  summarise(n_observations = n(), .groups = "drop") |> 
  filter(site %in% c("B12", "B12A", "C4", "D1", "D3", "R23"))

ggplot(regal_plant_count, aes(x=factor(year), fill=site))+
  geom_bar()
  ylim(0,50)
  
astu_cidi<- matrix_98_24 |> 
  filter(nectar_species_cleaned %in% c("ASTU", "CIDI"))

ggplot(astu_cidi, aes(x=julian, y= factor(year), fill=nectar_species_cleaned))+
  geom_density_ridges(alpha=.5)

regal_sex<-regal_df |> 
  filter(sex %in% c("M", "F"))

ggplot(regal_sex, aes(x=julian, y=factor(year), fill=sex))+
  geom_density_ridges(alpha=.5)

view(matrix_98_24 |>        count(butterfly_species_cleaned, name = "observations") |> 
       arrange(desc(observations)))

#Community wide---------------------------------

ft_sp<- matrix_98_24 |> 
  #filter(nectar_species_cleaned %in% c("ASSY", "CENTA", "ASTU", "CIDI", "CIPU4", "MOFI2", "APOCY"))

ggplot(ft_sp, aes(x=julian, fill=nectar_species_cleaned))+
  geom_density(alpha=.5)+
  scale_fill_manual(values = okabe_ito_extended)

mode_calc <- ft_sp |>
  group_by(nectar_species_cleaned, year) |>
  summarize(
    year_mode = Mode(julian),
    .groups = "drop"
  ) |> 
  group_by(nectar_species_cleaned) |>
  summarize(
    average_mode = mean(year_mode, na.rm = TRUE),
    .groups = "drop"
  )

nectar_timing <- ft_sp |>
  group_by(nectar_species_cleaned) |>
  summarize(
    mean_julian = mean(julian),
    min_julian = min(julian),
    max_julian = max(julian),
    .groups = "drop"
  ) |> 
  left_join(mode_calc, by="nectar_species_cleaned")

print(nectar_timing)

erf_ft_sp<- ft_sp |> 
  filter(butterfly_species_cleaned== "ARID") |> 
  filter(week!= "NA")

erf_timing<- erf_ft_sp |> 
  group_by(nectar_species_cleaned) |>
  summarize(
    mean_julian = mean(julian),
    min_julian = min(julian),
    max_julian = max(julian),
    mode_julian= Mode(julian),
    .groups = "drop"
  )
 print(erf_timing)  
print(nectar_timing)


erf_dense<- ggplot(erf_ft_sp, aes(x=julian, fill=nectar_species_cleaned))+
  geom_density(alpha=.5)+
  labs(title= "Eastern Regal Fritillary")

arca_ft_sp<- ft_sp |> 
  filter(butterfly_species_cleaned== "ARCA") |> 
  filter(week!= "NA")

arca_dense<- ggplot(arca_ft_sp, aes(julian, fill= nectar_species_cleaned))+
  geom_density(alpha=.5)+
  labs(title= "Great Spangled/Aphrodite Fritillary")

epcl_ft_sp<- ft_sp |> 
  filter(butterfly_species_cleaned== "EPCL") |> 
  filter(week!="NA")

epcl_dense<-ggplot(epcl_ft_sp, aes(julian, fill= nectar_species_cleaned))+
  geom_density(alpha= .5)+
  labs(title= "Silver Spotted Skipper")

patr_ft_sp<- ft_sp |> 
  filter(butterfly_species_cleaned=="PATR") |> 
  filter(week!= "NA")

patr_dense<-ggplot(patr_ft_sp, aes(julian, fill= nectar_species_cleaned))+
  geom_density(alpha= .5)+
  labs(title= "Spicebush Swallowtail")

pier_ft_sp<- ft_sp |> 
  filter(butterfly_species_cleaned== "PIER") |> 
  filter(week!= "NA")

pier_dense<- ggplot(pier_ft_sp, aes(julian, fill= nectar_species_cleaned))+
  geom_density(alpha= .5)+
  labs(title= "Sulphurs")

all_sp<- ggplot(ft_sp, aes(x=julian, fill= nectar_species_cleaned))+
  geom_density(alpha=.5)+
  labs(title= "General distribution of focal species")

ggarrange(pier, arca, erf, epcl, patr, all_sp)


erf_prop<- ggplot(erf_ft_sp, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "erf")

arca_prop<- ggplot(arca_ft_sp, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "gsa")

epcl_prop<- ggplot(epcl_ft_sp, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "ssk")

pier_prop<- ggplot(pier_ft_sp, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title="sulphur")

patr_prop<- ggplot(patr_ft_sp, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "spicebush")

all_prop<- ggplot(ft_sp, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "all")

ggarrange(erf_prop, arca_prop, epcl_prop, pier_prop, patr_prop, all_prop)+
  scale_fill_manual(values=okabe_ito_extended)

#all plant sp-------------------------


ggarrange(gsa_all_plant_prop, erf_prop_all_plant, epcl_prop_all_plant, pier_prop_all_plant, patr_prop_all_plant, all_prop_all_plant)

epcl_filter<-matrix_98_24 |> 
  filter(butterfly_species_cleaned=="EPCL") |> 
  filter(behavior==3)

epcl_prop_all_plant<- ggplot(epcl_filter, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "ssk")

pier_filter<-matrix_98_24 |> 
  filter(butterfly_species_cleaned=="PIER") |> 
  filter(behavior==3)

pier_prop_all_plant<- ggplot(pier_filter, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title="sulphur")

patr_filter<-matrix_98_24 |> 
  filter(butterfly_species_cleaned=="PATR") |> 
  filter(behavior==3)

patr_prop_all_plant<- ggplot(patr_filter, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "spicebush")

filtered<- matrix_98_24 |> 
  filter(nectar_species_cleaned!="NA") |> 
  filter(week!= "NA") |> 
  filter(behavior==3)

all_prop_all_plant<- ggplot(filtered, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "all")

ggarrange(erf_prop, arca_prop, epcl_prop, pier_prop, patr_prop, all_prop)+
  scale_fill_manual(values=okabe_ito_extended)

 

# Step 1: Identify which nectar species have fewer than 10 observations per butterfly
species_classification <- matrix_98_24 |>
  group_by(butterfly_species_cleaned, nectar_species_cleaned) |>
  summarize(n = n(), .groups = "drop") |>
  mutate(
    nectar_species_cleaned_new = if_else(n < 10, "Other", nectar_species_cleaned)
  )

# Step 2: Join back to the original data
matrix_collapsed <- matrix_98_24 |>
  left_join(species_classification |> select(butterfly_species_cleaned, nectar_species_cleaned, nectar_species_cleaned_new),
            by = c("butterfly_species_cleaned", "nectar_species_cleaned")) |>
  mutate(nectar_species_cleaned = nectar_species_cleaned_new) |>
  select(-nectar_species_cleaned_new)

nectar_matrix<- matrix_collapsed |> 
  filter(behavior==3) |> 
  filter(nectar_species_cleaned!="NA")

matrix_collapsed|>
  filter(butterfly_species_cleaned == "EPCL") |>
  count(nectar_species_cleaned) |>
  ggplot(aes(x = n, y = reorder(nectar_species_cleaned, n))) +
  geom_point(size = 3)

epcl_filter<-nectar_matrix |> 
  filter(butterfly_species_cleaned=="EPCL") |> 
  filter(behavior==3)

ggplot(epcl_filter, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "ssk")

ggarrange(gsa_prop_all_plant, erf_prop_all_plant, epcl_prop_all_plant, pier_prop_all_plant, patr_prop_all_plant, all_prop_all_plant)

epcl_filter<-nectar_matrix |> 
  filter(butterfly_species_cleaned=="EPCL") |> 
  filter(behavior==3)

epcl_prop_all_plant<- ggplot(epcl_filter, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "ssk")

pier_filter<-nectar_matrix |> 
  filter(butterfly_species_cleaned=="PIER") |> 
  filter(behavior==3)

pier_prop_all_plant<- ggplot(pier_filter, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title="sulphur")

patr_filter<-nectar_matrix |> 
  filter(butterfly_species_cleaned=="PATR") |> 
  filter(behavior==3)

patr_prop_all_plant<- ggplot(patr_filter, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "spicebush")

filtered<- nectar_matrix |> 
  filter(nectar_species_cleaned!="NA") |> 
  filter(week!= "NA") |> 
  filter(behavior==3)

all_prop_all_plant<- ggplot(filtered, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "all")

gsa_filter<- nectar_matrix |> 
  filter(butterfly_species_cleaned=="ARCA") |> 
  filter(behavior==3)

erf_filter<- nectar_matrix |> 
  filter(butterfly_species_cleaned=="ARID") |> 
  filter(behavior==3)

ggplot(erf_filter, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "erf")

gsa_prop_all_plant<- ggplot(gsa_filter, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "gsa")

all_nectar<- matrix_98_24 |> 
  filter(behavior==3) |> 
  filter(nectar_species_cleaned!= "NA")

ggarrange(gsa_prop_all_plant, erf_prop_all_plant, all_prop_all_plant)

#rot 3 all nectar use------------------------------------------

view(all_nectar |> 
       filter(year>=2010) |> 
       count(butterfly_species_cleaned, sort = TRUE))

# Step 1: Identify which nectar species have fewer than 10 observations per butterfly
species_classification <- all_nectar |>
  group_by(butterfly_species_cleaned, nectar_species_cleaned) |>
  summarize(n = n(), .groups = "drop") |>
  mutate(
    nectar_species_cleaned_new = if_else(n < 30, "Other", nectar_species_cleaned)
  )

# Step 2: Join back to the original data
nectar_collapsed <- all_nectar |>
  left_join(species_classification |> select(butterfly_species_cleaned, nectar_species_cleaned, nectar_species_cleaned_new),
            by = c("butterfly_species_cleaned", "nectar_species_cleaned")) |>
  mutate(nectar_species_cleaned = nectar_species_cleaned_new) |>
  select(-nectar_species_cleaned_new)

arca_all<-nectar_collapsed |> 
  filter(butterfly_species_cleaned== "ARCA") |> 
  filter(week>=24)

erf_all<- nectar_collapsed |> 
  filter(butterfly_species_cleaned== "ARID") |> 
  filter(week>=24)

arca_pres<-ggplot(arca_all, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors,
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca", 
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea sp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea sp.",
                              "AGERA2"= "Ageratina sp",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium sp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum sp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera sp.",
                              "SOLID"= "Solidago spp."))+
  labs(title= "GSA",
       y= "Proportion of use",
       x= "Week of the year")

erf_pres<-ggplot(erf_all, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors,
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca", 
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea sp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea sp.",
                              "AGERA2"= "Ageratina sp",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium sp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum sp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera sp.",
                              "SOLID"= "Solidago spp."))+
  labs(title= "Regal",
       y= "Proportion of Use",
       x= "Week of the year")

ggarrange(arca_pres, erf_pres)

post_2010_collapsed<- nectar_collapsed |> 
  filter(year>= 2010) |> 
  filter(week>=23)

arca_collapse<- post_2010_collapsed |> 
  filter(butterfly_species_cleaned=="ARCA")

arca_plot<-ggplot(arca_collapse, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= okabe_ito_extended)+
  labs(title= "GSA")

patr_collapse<- post_2010_collapsed |> 
  filter(butterfly_species_cleaned=="PATR")

patr_plot<-ggplot(patr_collapse, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors) +
  labs(title= "Spicebush")+
  theme(axis.text.x = element_text(size = 8))

epcl_collapse<- post_2010_collapsed |> 
  filter(butterfly_species_cleaned=="EPCL")

ggplot(epcl_collapse, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors)+
  labs(title= "SSK")+
  guides(fill = guide_legend(ncol = 2))+
  theme(axis.text.x = element_text(size = 8))

arid_collapse<- post_2010_collapsed |> 
  filter(butterfly_species_cleaned=="ARID")

ggplot(arid_collapse, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors)+
  labs(title= "Eastern Regal")+
  theme(axis.text.x = element_text(size = 8))

dapl_collapse<- post_2010_collapsed |> 
  filter(butterfly_species_cleaned=="DAPL")

dapl_plot<-ggplot(dapl_collapse, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors)+
  labs(title= "Monarch")+
  theme(axis.text.x = element_text(size = 8))

pagl_collapse<- post_2010_collapsed |> 
  filter(butterfly_species_cleaned=="PAGL")

pagl_plot<-ggplot(pagl_collapse, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors)+
  labs(title= "ETS")+
  theme(axis.text.x = element_text(size = 8))


all_plot<-ggplot(post_2010_collapsed, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors)+
  labs(title= "All")+
  theme(axis.text.x = element_text(size = 8))

ggarrange(pagl_plot, epcl_plot, dapl_plot, arid_plot, patr_plot, all_plot)

filter_week_patr<- patr_collapse |> 
  filter(week>=34) |> 
  filter(week<=39)

patr_week<-ggplot(filter_week_patr, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors,
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca", 
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea spp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea spp..",
                              "AGERA2"= "Ageratina spp.",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium spp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum spp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera spp.",
                              "SOLID"= "Solidago spp.",
                              "ASTER"= 'Aster spp.',
                              "ERIGE2"= "Erigeron spp.",
                              "EUTRO"= "Eutrochium spp.",
                              "LESPE"= "Lespedeza spp.",
                              "PENS"= "Penstemon spp.",
                              "PRUNE"= "Prunella spp."
                    ))+
  labs(title= "Spicebush Swallowtail",
       x= "Week of the year",
       y= "Proportion",
       fill= "Nectar species")+
  theme(legend.position = "none")

filter_week_dapl<- dapl_collapse |> 
  filter(week>=34)|> 
  filter(week<=39)

dapl_week<-ggplot(filter_week_dapl, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors,
                    labels= c("APOCY"= "Apocynum spp.", 
            "ASSY"= "Asclepias syriaca", 
            "ASTU"= "Asclepias tuberosa", 
            "CENTA"= "Centaurea spp.", 
            "CIDI"= "Cirsium discolor", 
            "CIPU4"= "Cirsium pumilum", 
            "MOFI2"= "Monarda fistulosa",
            "ACHIL"= "Achillea spp..",
            "AGERA2"= "Ageratina spp.",
            "ASIN"= "Asclepias incarnata",
            "CIAR4"= "Cirsium arvense",
            "DACA6"= "Daucus carota",
            "EUPAT"= "Eupatorium spp.",
            "EUPE3"= "Eupatorium perfoliatum",
            "LEVU"= "Leucanthemum vulgare",
            "PYCNA"= "Pycanthemum spp.",
            "ROMU"= "Rosa multiflora",
            "SECUR3"= "Securigera spp.",
            "SOLID"= "Solidago spp.",
            "ASTER"= 'Aster spp.',
            "ERIGE2"= "Erigeron spp.",
            "EUTRO"= "Eutrochium spp.",
            "LESPE"= "Lespedeza spp.",
            "PENS"= "Penstemon spp.",
            "PRUNE"= "Prunella spp."
  ))+
  labs(title= "Monarch",
       x= "Week of the year",
       y= "Proportion",
       fill= "Nectar species")+
  theme(legend.position = "none")

filter_week_all<- post_2010_collapsed|> 
  filter(week>=34)|> 
  filter(week<=39)

all_week<-ggplot(filter_week_all, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors,
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca", 
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea spp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea spp..",
                              "AGERA2"= "Ageratina spp.",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium spp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum spp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera spp.",
                              "SOLID"= "Solidago spp.",
                              "ASTER"= 'Aster spp.',
                              "ERIGE2"= "Erigeron spp.",
                              "EUTRO"= "Eutrochium spp.",
                              "LESPE"= "Lespedeza spp.",
                              "PENS"= "Penstemon spp.",
                              "PRUNE"= "Prunella spp."
                    ))+
  labs(title= "All butterflies",
       x= "Week of the year",
       y= "Proportion",
       fill= "Nectar species")+
  theme(legend.position= "none") #comment out to get the legend on its own.

week_legend <- get_legend(all_week)

# 3. Display just the legend
plot_grid(week_legend)
  
filter_week_arid<- post_2010_collapsed |> 
  filter(butterfly_species_cleaned== "ARID") |> 
  filter(week>=34) |> 
  filter(week<=39)

arid_week<-ggplot(filter_week_arid, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors)+
  labs(title= "All")+
  theme(axis.text.x = element_text(size = 8))+
  guides(fill = guide_legend(ncol = 2))+
  scale_fill_manual(labels= c("APOCY"= "Apocynum spp.", 
            "ASSY"= "Asclepias syriaca", 
            "ASTU"= "Asclepias tuberosa", 
            "CENTA"= "Centaurea spp.", 
            "CIDI"= "Cirsium discolor", 
            "CIPU4"= "Cirsium pumilum", 
            "MOFI2"= "Monarda fistulosa",
            "ACHIL"= "Achillea spp..",
            "AGERA2"= "Ageratina spp.",
            "ASIN"= "Asclepias incarnata",
            "CIAR4"= "Cirsium arvense",
            "DACA6"= "Daucus carota",
            "EUPAT"= "Eupatorium spp.",
            "EUPE3"= "Eupatorium perfoliatum",
            "LEVU"= "Leucanthemum vulgare",
            "PYCNA"= "Pycanthemum spp.",
            "ROMU"= "Rosa multiflora",
            "SECUR3"= "Securigera spp.",
            "SOLID"= "Solidago spp.",
            "ASTER"= 'Aster spp.',
            "ERIGE2"= "Erigeron spp.",
            "EUTRO"= "Eutrochium spp.",
            "LESPE"= "Lespedeza spp.",
            "PENS"= "Penstemon spp.",
            "PRUNE"= "Prunella spp."
  ))+
  labs(title= "Eastern Regal Fritillary",
       x= "Week of the year",
       y= "Proportion",
       fill= "Nectar species")+
  theme(legend.position= "none")

ggarrange(patr_week, dapl_week, all_week)

blank <- ggplot() + theme_void()

ggarrange(
  ggarrange(patr_week, dapl_week, blank, nrow = 1,
            widths = c(1, 1, 0)),
  ggarrange(blank, all_week, blank, nrow = 1,
            widths = c(0.5, 1, 0.5)),
  nrow = 2)


epcl_arid<- post_2010_collapsed |> 
  filter(butterfly_species_cleaned== "ARID" | butterfly_species_cleaned== "EPCL")

model <- multinom(nectar_species_cleaned ~ butterfly_species_cleaned + week, data = epcl_arid)
summary(model)

summary_model <- summary(model)
z <- summary_model$coefficients / summary_model$standard.errors
p <- 2 * (1 - pnorm(abs(z)))  # two-tailed test

# View significant results
round(p, 4)

library(vegan)

# Create species-by-week-site matrix
community_matrix <- epcl_arid%>%
  group_by(butterfly_species_cleaned, week, nectar_species_cleaned) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = nectar_species_cleaned, values_from = n, values_fill = 0)

# Run PERMANOVA
adonis2(community_matrix[,-c(1:2)] ~ butterfly_species_cleaned, data = community_matrix, method = "bray")

nmds <- metaMDS(community_matrix, distance = "bray", k = 2)  # Bray-Curtis is common for ecology

# Basic plot
plot(nmds, type = "n")  # Empty plot
points(nmds, display = "sites", col = group_colors, pch = 16)  # Add colored points
ordihull(nmds, groups = group_variable, draw = "polygon", col = group_colors)  # Group outlines

few_obs<- matrix_98_24 |> 
  filter(behavior==3) |> 
  filter(year>=2010)

view(few_obs |> 
       group_by(year) |> 
       count(butterfly_species_cleaned) |> 
       filter(n>4) |> 
       group_by(butterfly_species_cleaned) |> 
       summarize(years_present = n_distinct(year)) |> 
       filter(years_present >= 4))

# Step 1: Count per species per year
  species_year_counts <- matrix_98_24 %>%
  group_by(butterfly_species_cleaned, year) %>%
  tally(name = "n_yearly")

# Step 2: Species with <4 obs in any year
species_few_obs_years <- species_year_counts %>%
  filter(n_yearly < 4) %>%
  pull(butterfly_species_cleaned) %>%
  unique()

# Step 3: Species in <4 unique years
species_few_years <- matrix_98_24 %>%
  group_by(butterfly_species_cleaned) %>%
  summarise(n_years = n_distinct(year)) %>%
  filter(n_years < 4) %>%
  pull(butterfly_species_cleaned)

# Step 4: Filter original data
filtered_df <- matrix_98_24 %>%
  filter(
    butterfly_species_cleaned %in% union(species_few_obs_years, species_few_years),
    behavior == 3,
    year >= 2010
  )
view(filtered_df |> 
       count(butterfly_species_cleaned, sort= TRUE))

erf<- matrix_98_24 |> 
  filter(behavior==3) |> 
  filter(butterfly_species_cleaned== "ARID") |> 
  filter(nectar_species_cleaned!= "NA")

ggplot(arca, aes(x= as.factor(week), fill= nectar_species_cleaned))+
  geom_bar(position= "fill")

view(matrix_98_24 |> 
  filter(behavior==3) |> 
  filter(butterfly_species_cleaned %in% c("ARID", "EPCL", "PAGL", "DAPL", "PATR", "CEPE")) |> 
  group_by(butterfly_species_cleaned, week)  |> 
  summarise(n_nectaring = n(), .groups = "drop"))

species_classification <- all_nectar |>
  group_by(butterfly_species_cleaned, nectar_species_cleaned) |>
  summarize(n = n(), .groups = "drop") |>
  mutate(
    nectar_species_cleaned_new = if_else(n < 20, "Other", nectar_species_cleaned)
  )

# Step 2: Join back to the original data
nectar_collapsed <- all_nectar |>
  left_join(species_classification |> select(butterfly_species_cleaned, nectar_species_cleaned, nectar_species_cleaned_new),
            by = c("butterfly_species_cleaned", "nectar_species_cleaned")) |>
  mutate(nectar_species_cleaned = nectar_species_cleaned_new) |>
  select(-nectar_species_cleaned_new)

nectar_matrix<- nectar_collapsed |> 
  filter(behavior==3) |> 
  filter(nectar_species_cleaned!="NA") |> 
  group_by(butterfly_species_cleaned, week) |> 
  filter(n() >= 7) |> 
  ungroup()



all_focal<- nectar_matrix |> 
  filter(week>= 21) |> 
  filter(week<= 40)

focal_all<-ggplot(all_focal, aes(x= as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors,
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca", 
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea spp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea spp..",
                              "AGERA2"= "Ageratina spp.",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium spp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum spp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera spp.",
                              "SOLID"= "Solidago spp.",
                              "ASTER"= 'Aster spp.',
                              "ERIGE2"= "Erigeron spp.",
                              "EUTRO"= "Eutrochium spp.",
                              "LESPE"= "Lespedeza spp.",
                              "PENS"= "Penstemon spp.",
                              "PRUNE"= "Prunella spp."
                              ))+
  labs(title= "All butterflies",
       x= "Week of the year",
       y= "Proportion",
       fill= "Nectar species")+
  theme_minimal()+
  theme(
    strip.text= element_text(face= "italic"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.text = element_text(size = 8),           # Smaller axis text
    axis.title = element_text(size = 10),
    legend.position= "none")+
  guides(fill = guide_legend(ncol = 1))
  
focal_legend<- get_legend(focal_all)

plot_grid(focal_legend)

post_2010_focal<- species_week_counts |> 
  filter(year>=2010)

all_weeks <- as.character (21:40)  # Replace with actual range

# Ensure 'week' includes all possible levels, even if some are missing in the data
post_2010_focal$week <- factor(post_2010_focal$week, levels = all_weeks)

regal_exclude<- post_2010_focal |> 
  filter(butterfly_species_cleaned=="ARID") |> 

ssk_exclude<- post_2010_focal |> 
  filter(butterfly_species_cleaned=="EPCL")

ets_exclude<- post_2010_focal |> 
  filter(butterfly_species_cleaned=="PAGL")

spice_exclude<- post_2010_focal |> 
  filter(butterfly_species_cleaned=="PATR")

pc_exclude<- post_2010_focal |> 
  filter(butterfly_species_cleaned=="PHTH")

monarch_exclude<- post_2010_focal |> 
  filter(butterfly_species_cleaned=="DAPL")

ggplot(regal_exclude, aes(x= as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors,
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca", 
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea spp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea spp..",
                              "AGERA2"= "Ageratina spp.",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium spp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum spp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera spp.",
                              "SOLID"= "Solidago spp.",
                              "ASTER"= 'Aster spp.',
                              "ERIGE2"= "Erigeron spp.",
                              "EUTRO"= "Eutrochium spp.",
                              "LESPE"= "Lespedeza spp.",
                              "PENS"= "Penstemon spp.",
                              "PRUNE"= "Prunella spp."
                    ))+
  labs(title= "Eastern Regal Fritillary",
       x= "Week of the year",
       y= "Proportion",
       fill= "Nectar species")+
  theme_minimal()+
  theme(
        strip.text= element_text(face= "italic"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text = element_text(size = 8),           # Smaller axis text
        axis.title = element_text(size = 10),         # Optional: smaller axis titles
        legend.position="none")+
  scale_x_discrete(drop = FALSE)

epcl<-ggplot(ssk_exclude, aes(x= as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors,
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca", 
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea spp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea spp..",
                              "AGERA2"= "Ageratina spp.",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium spp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum spp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera spp.",
                              "SOLID"= "Solidago spp.",
                              "ASTER"= 'Aster spp.',
                              "ERIGE2"= "Erigeron spp.",
                              "EUTRO"= "Eutrochium spp.",
                              "LESPE"= "Lespedeza spp.",
                              "PENS"= "Penstemon spp.",
                              "PRUNE"= "Prunella spp."
                    ))+
  labs(title= "Silver Spotted Skipper",
       x= "Week of the year",
       y= "Proportion",
       fill= "Nectar species")+
  theme_minimal()+
  theme(
    strip.text= element_text(face= "italic"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.text = element_text(size = 8),           # Smaller axis text
    axis.title = element_text(size = 10))+
  scale_x_discrete(drop = FALSE)

epcl_legend<-get_legend(epcl)

plot_grid(epcl_legend)

pagll<- ggplot(ets_exclude, aes(x= as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors,
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca", 
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea spp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea spp..",
                              "AGERA2"= "Ageratina spp.",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium spp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum spp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera spp.",
                              "SOLID"= "Solidago spp.",
                              "ASTER"= 'Aster spp.',
                              "ERIGE2"= "Erigeron spp.",
                              "EUTRO"= "Eutrochium spp.",
                              "LESPE"= "Lespedeza spp.",
                              "PENS"= "Penstemon spp.",
                              "PRUNE"= "Prunella spp."
                    ))+
  labs(title= "Eastern Tiger Swallowtail",
       x= "Week of the year",
       y= "Proportion",
       fill= "Nectar species")+
  theme_minimal()+
  theme(
    strip.text= element_text(face= "italic"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.text = element_text(size = 8),           # Smaller axis text
    axis.title = element_text(size = 10),         # Optional: smaller axis titles
    legend.position="none")+
  scale_x_discrete(drop = FALSE)

patr<- ggplot(spice_exclude, aes(x= as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors,
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca", 
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea spp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea spp..",
                              "AGERA2"= "Ageratina spp.",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium spp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum spp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera spp.",
                              "SOLID"= "Solidago spp.",
                              "ASTER"= 'Aster spp.',
                              "ERIGE2"= "Erigeron spp.",
                              "EUTRO"= "Eutrochium spp.",
                              "LESPE"= "Lespedeza spp.",
                              "PENS"= "Penstemon spp.",
                              "PRUNE"= "Prunella spp."
                    ))+
  labs(title= "Spicebush Swallowtail",
       x= "Week of the year",
       y= "Proportion",
       fill= "Nectar species")+
  theme_minimal()+
  theme(
        strip.text= element_text(face= "italic"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text = element_text(size = 8),           # Smaller axis text
        axis.title = element_text(size = 10),         # Optional: smaller axis titles
        legend.position="none")+
  scale_x_discrete(drop = FALSE)

phth <- ggplot(pc_exclude, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors)+
  labs(title= "Pearl Crescent")+
  theme(axis.text.x = element_text(size = 8))

dapl<- ggplot(monarch_exclude, aes(x= as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  scale_fill_manual(values= plant_colors,
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca", 
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea spp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea spp..",
                              "AGERA2"= "Ageratina spp.",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium spp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum spp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera spp.",
                              "SOLID"= "Solidago spp.",
                              "ASTER"= 'Aster spp.',
                              "ERIGE2"= "Erigeron spp.",
                              "EUTRO"= "Eutrochium spp.",
                              "LESPE"= "Lespedeza spp.",
                              "PENS"= "Penstemon spp.",
                              "PRUNE"= "Prunella spp."
                    ))+
  labs(title= "Monarch",
       x= "Week of the year",
       y= "Proportion",
       fill= "Nectar species")+
  theme_minimal()+
  theme(
    strip.text= element_text(face= "italic"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.text = element_text(size = 8),           # Smaller axis text
    axis.title = element_text(size = 10),         # Optional: smaller axis titles
    legend.position="none")+
  scale_x_discrete(drop = FALSE)

ggarrange(pagll, epcl, dapl, arid, patr, focal_all)


view(nectar_matrix |> 
       group_by(nectar_species_cleaned, butterfly_species_cleaned) %>%
       summarise(n_visits = n(), .groups = "drop") %>%
       arrange(nectar_species_cleaned, desc(n_visits)))

