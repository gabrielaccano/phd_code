library(vegan)
library(tidyverse)
library(rlang)
library(readxl)
library(ggridges)
library(writexl)

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


setwd("C:/Users/tut43799/OneDrive - Temple University/Documents/GitHub/phd_code")
matrix_98_24<- read_xlsx("C:/Users/GCano/Documents/GitHub/phd_code/butterfly_data_16_24/complete pollard data CRT_October2025.xlsx")

matrix_98_24<- matrix_98_24 |> 
  mutate(month = as.integer(month))

run_nmds_plot <- function(species_name) {
  
  # Dynamically build variable names
  prefix <- tolower(species_name)
  
  # Step 1: Filter and prep data
  prep <- matrix_98_24 |> 
    filter(butterfly_species_cleaned == species_name) |> 
    filter(behavior == 3) |> 
    filter(!is.na(nectar_species_cleaned), field != "Boyer") |> 
    filter(nectar_species_cleaned!= "NA", field != "Middle Creek")
  
  dist_matrix <- prep |> 
    count(year, month, field, nectar_species_cleaned) |> 
    pivot_wider(names_from = nectar_species_cleaned,
                values_from = n,
                values_fill = list(n = 0))
  
  rownames(dist_matrix) <- paste(dist_matrix$year, dist_matrix$month, dist_matrix$field, sep = "_")
  
  numeric_only <- as.data.frame(dist_matrix[, !(names(dist_matrix) %in% c("year", "month", "field"))]) 
  rownames(numeric_only) <- rownames(dist_matrix)
  
  nectar_counts <- colSums(numeric_only != 0) 
  keep_cols <- nectar_counts >= 10
  filtered_outliers <- numeric_only[, keep_cols]
  
  row_sums <- rowSums(filtered_outliers, na.rm = TRUE)
  keep_rows <- row_sums >= 5
  numeric_filtered <- filtered_outliers[keep_rows, ]
  
  # Step 2: NMDS
  set.seed(82402)
  nmds <- metaMDS(numeric_filtered,
                  distance = "bray",
                  wascores = TRUE,
                  trymax = 500,
                  autotransform = FALSE)
  
  nmds_points <- as.data.frame(nmds$points)
  
  numeric_filtered$combo_id <- rownames(numeric_filtered)
  parts <- do.call(rbind, strsplit(numeric_filtered$combo_id, "_"))
  colnames(parts) <- c("year", "month", "field")
  matrix_scores <- cbind(parts, nmds_points)
  
  # Convex hulls by month
  hull_data_month <- matrix_scores |> 
    group_by(month) |> 
    slice(chull(MDS1, MDS2))
  
  # Colors
  okabe_ito_extended <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                          "#0072B2", "#D55E00", "#CC79A7", "#999999")
  
  #plant scores
  plant_scores <- scores(nmds, display = "species")
  plant_df <- as.data.frame(plant_scores)
  plant_df$nectar_species_cleaned <- rownames(plant_df)
  
  # Plot
  p <- ggplot() +
    geom_point(data = matrix_scores, aes(x = MDS1, y = MDS2, color = as.factor(month), shape = as.factor(month)),
               size = 3) +
    geom_label(data= plant_df, aes(x= NMDS1, y=NMDS2, label= nectar_species_cleaned))+
    geom_polygon(data = hull_data_month, 
                 aes(x = MDS1, y = MDS2, group = month, fill = as.factor(month)), 
                 alpha = 0.1, color = "black") +
    scale_fill_manual(values = okabe_ito_extended) +
    scale_color_manual(values = okabe_ito_extended) +
    stat_ellipse(data = matrix_scores, aes(x = MDS1, y = MDS2, color = month), level = 0.50) +
    labs(title = paste("NMDS of nectar use for", species_name)) +
    theme_minimal()
  
  # Save objects to global environment if needed
  assign(paste0(prefix, "_prep"), prep, envir = .GlobalEnv)
  assign(paste0(prefix, "_dist_matrix"), dist_matrix, envir = .GlobalEnv)
  assign(paste0(prefix, "_numeric_filtered"), numeric_filtered, envir = .GlobalEnv)
  assign(paste0(prefix, "_nmds"), nmds, envir = .GlobalEnv)
  assign(paste0(prefix, "_matrix_scores"), matrix_scores, envir = .GlobalEnv)
  assign(paste0(prefix, "_hull_data_month"), hull_data_month, envir = .GlobalEnv)
  assign(paste0(prefix, "_plant_scores"), plant_scores, envir = .GlobalEnv)
  assign(paste0(prefix, "_plant_df"), plant_df, envir = .GlobalEnv)
  
  return(p)
}


#prep for primer

dapl_numeric_filtered$combo_id <- rownames(dapl_numeric_filtered)

# Then separate that column back into original parts
dapl_parts <- do.call(rbind, strsplit(dapl_numeric_filtered$combo_id, "_"))
colnames(dapl_parts) <- c("year", "month", "field")
dapl_primer<- cbind(dapl_parts, dapl_numeric_filtered)
write_xlsx(dapl_primer, "C:/Users/tut43799/OneDrive - Temple University/Documents/GitHub/phd_code/dapl_primer.xlsx")

run_nmds_plot("ARID")

run_nmds_plot("DAPL")

run_nmds_plot("ARCA")

run_nmds_plot("EPCL")

run_nmds_plot("PAGL")

run_nmds_plot("PATR")

patr_parts <- do.call(rbind, strsplit(patr_numeric_filtered$combo_id, "_"))
patr_primer<- cbind(patr_parts, patr_numeric_filtered)
write_xlsx(patr_primer, "C:/Users/tut43799/OneDrive - Temple University/Documents/GitHub/phd_code/patr_primer.xlsx")

regal_remove_na<- matrix_98_24 |> 
  filter(butterfly_species_cleaned=="ARID") |> 
  filter(behavior==3) |> 
  filter(nectar_species_cleaned!= "NA") |> 
  group_by(nectar_species_cleaned) |> 
  filter(n() >= 10) |> 
  ungroup() |> 
  filter(week>23) |> 
  filter(week<40)

dapl_remove_na<- matrix_98_24 |> 
  filter(butterfly_species_cleaned=="DAPL") |> 
  filter(behavior==3) |> 
  filter(nectar_species_cleaned!= "NA") |> 
  group_by(nectar_species_cleaned) |> 
  filter(n() >= 10) |> 
  ungroup() |> 
  filter(week>27)


dapl_plant_scores<-scores(dapl_nmds, display= "species")
dapl_plant_df<-as.data.frame(dapl_plant_scores)
dapl_plant_df$nectar_species_cleaned<-rownames(dapl_plant_df)


ggplot() +
  geom_point(data = dapl_matrix_scores, aes(x = MDS1, y = MDS2, color = as.factor(month), shape = as.factor(month)),
             size = 3) +
  #geom_label(data= dapl_plant_df, aes(x= NMDS1, y=NMDS2, label= nectar_species_cleaned))+
  geom_polygon(data = dapl_hull_data_month, 
               aes(x = MDS1, y = MDS2, group = month, fill = as.factor(month)), 
               alpha = 0.1, color = "black") +
  scale_fill_manual(values = okabe_ito_extended) +
  scale_color_manual(values = c("7"="#56B4E9",
                                "8"= "#009E73", 
                                "9"="#F0E442",
                                "10"="#D55E00")) +
  stat_ellipse(data = dapl_matrix_scores, aes(x = MDS1, y = MDS2, color = month), level = 0.5) +
  labs(title = "NMDS of nectar use by Monarch") +
  theme_minimal()+
  labs(color= "Month", shape= "Month", fill= "Month")

dapl_prop<-ggplot(dapl_remove_na, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "Proportion of nectar use by week of Monarchs")+ 
  scale_fill_manual(values=plant_colors,
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca",
                              "ASTER"= "Aster spp.",
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea spp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea spp.",
                              "AGERA2"= "Ageratina spp.",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium spp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "EUTRO"= "Eutrochium spp.",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum spp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera spp.",
                              "SOLID"= "Solidago spp."))+
  labs(x= "Week of the year", y= "Proportion", fill= "Nectar species")+
  theme(legend.text = element_text(face = "italic"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))

ggsave("dapl_prop.png",
       plot = dapl_prop,
       width = 13.22,
       height = 8.7,
       units = "in",
       dpi = 300)

regal_plant_scores<-scores(arid_nmds, display= "species")
regal_plant_df<-as.data.frame(regal_plant_scores)
regal_plant_df$nectar_species_cleaned<-rownames(regal_plant_df)

ggplot() +
  geom_point(data = arid_matrix_scores, aes(x = MDS1, y = MDS2, color = as.factor(month), shape = as.factor(month)),
             size = 3) +
  #geom_label(data= regal_plant_df, aes(x= NMDS1, y=NMDS2, label= nectar_species_cleaned))+
  geom_polygon(data = arid_hull_data_month, 
               aes(x = MDS1, y = MDS2, group = month, fill = as.factor(month)), 
               alpha = 0.1, color = "black") +
  scale_fill_manual(values = okabe_ito_extended) +
  scale_color_manual(values = okabe_ito_extended) +
  stat_ellipse(data = arid_matrix_scores, aes(x = MDS1, y = MDS2, color = month), level = 0.95) +
  labs(title = "NMDS of nectar use for arid") +
  theme_minimal()+
  labs(title= "NMDS of nectar plant use by Eastern Regal Fritillary", color= "Month", shape= "Month", fill= "Month")


ggplot(regal_remove_na, aes(x=as.factor(week), fill=nectar_species_cleaned))+
  geom_bar(position= "fill")+
  labs(title= "Proportion of nectar use by week of Eastern Regal Fritillary")+ 
  scale_fill_manual(values=plant_colors,
                    labels= c("APOCY"= "Apocynum spp.", 
                              "ASSY"= "Asclepias syriaca",
                              "ASTER"= "Aster spp.",
                              "ASTU"= "Asclepias tuberosa", 
                              "CENTA"= "Centaurea spp.", 
                              "CIDI"= "Cirsium discolor", 
                              "CIPU4"= "Cirsium pumilum", 
                              "MOFI2"= "Monarda fistulosa",
                              "ACHIL"= "Achillea spp.",
                              "AGERA2"= "Ageratina spp.",
                              "ASIN"= "Asclepias incarnata",
                              "CIAR4"= "Cirsium arvense",
                              "DACA6"= "Daucus carota",
                              "EUPAT"= "Eupatorium spp.",
                              "EUPE3"= "Eupatorium perfoliatum",
                              "EUTRO"= "Euthrochium spp.",
                              "LEVU"= "Leucanthemum vulgare",
                              "PYCNA"= "Pycanthemum spp.",
                              "ROMU"= "Rosa multiflora",
                              "SECUR3"= "Securigera spp.",
                              "SOLID"= "Solidago spp."))+
  labs(x= "Week of the year", y= "Proportion", fill= "Nectar species")+
  theme(legend.text = element_text(face = "italic"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))


ggplot(regal_remove_na, aes(x=julian, y=factor(year), fill=nectar_species_cleaned))+
  geom_density_ridges(scale= 1, alpha=.5, rel_min_height= .1)+
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


ggplot(dapl_remove_na, aes(x=julian, y=factor(year), fill=nectar_species_cleaned))+
  geom_density_ridges(scale= 1, alpha=.5, rel_min_height= .1)+
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




arca_plant_scores<-scores(arca_nmds, display= "species")
arca_plant_df<-as.data.frame(arca_plant_scores)
arca_plant_df$nectar_species_cleaned<-rownames(arca_plant_df)

ggplot() +
  geom_point(data = arca_matrix_scores, aes(x = MDS1, y = MDS2, color = as.factor(month), shape = as.factor(month)),
             size = 3) +
  geom_label(data= arca_plant_df, aes(x= NMDS1, y=NMDS2, label= nectar_species_cleaned))+
  geom_polygon(data = arca_hull_data_month, 
               aes(x = MDS1, y = MDS2, group = month, fill = as.factor(month)), 
               alpha = 0.1, color = "black") +
  scale_fill_manual(values = okabe_ito_extended) +
  scale_color_manual(values = okabe_ito_extended) +
  stat_ellipse(data = arca_matrix_scores, aes(x = MDS1, y = MDS2, color = month), level = 0.95) +
  theme_minimal()+
  labs(title= "NMDS of nectar plant use by GSA", color= "Month", shape= "Month", fill= "Month")


