library(lme4)
library(devtools)
library(tidyverse)
library(vegan)
library (readxl)
library(gdata)
library(openxlsx)
library(ggridges)
library(ggpubr)
library(nnet)
library(cowplot) 
library(writexl)

okabe_ito_extended <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#D55E00", "#0072B2","#CC79A7", "#999999",  # Original Okabe-Ito (8 colors)
  "#882255", "#44AA99", "#117733", "#88CCEE", "#DDCC77",
  "#AA4499", "#332288", "#661100", "#BBBBBB", # Previously added (9 colors)
  "#6699CC", "#AA7744",
  "white", "black", "turquoise", "hotpink" # Two additional colors
)

setwd("C:/Users/GCano/Documents/GitHub/phd_code/")
matrix_98_24<- read_xlsx("C:/Users/GCano/Documents/GitHub/phd_code/butterfly_data_16_24/complete pollard data CRT 7May2025.xlsx"
                         # col_types = c(
                         #   "numeric",
                         #   "numeric",
                         #   "date",
                         #   "numeric",
                         #   "numeric",
                         #   "text",
                         #   "text",
                         #   "text",
                         #   "text",
                         #   "text",
                         #   "text",
                         #   "numeric",
                         #   "numeric",
                         #   "numeric",
                         #   "numeric",
                         #   "numeric",
                         #   "numeric",
                         #   "numeric",
                         #   "text",
                         #   "text",
                         #   "text",
                         #   "numeric",
                         #   "text",
                         #   "text",
                         #   "text",
                         #   "text"
                         )

#regal sex model -----------------
#fit model with known sexes

known_rg_sex<- matrix_98_24 |> 
  filter(butterfly_species_cleaned=="ARID") |> 
  filter(sex %in% c("M", "F"))

#convert sex to binary (male= 1, female= 0)

known_rg_binary<- known_rg_sex |> 
  mutate(sex_binary= ifelse(sex=="M", 1, 0))

#fit model

unk_sex_model<- glmer(
  sex_binary~ year+julian+s_wind+s_temp+s_percent_sun+ year*site + year*observer+
    julian*site+
    (1|site) + (1|section)+ (1|observer),
  data=known_rg_binary,
  family = binomial(link="logit")
)


#unknown sexes df

unknown_rg <- matrix_98_24 |> 
  filter(butterfly_species_cleaned== "ARID") |> 
  filter(is.na(sex))


predicted_probs <- predict(unk_sex_model, newdata = unknown_rg, type = "response")
unknown_rg$predicted_sex <- ifelse(predicted_probs > 0.5, "M", "F")
#regal nmds model----------------

regal_prep<- matrix_98_24 |> 
  filter(butterfly_species_cleaned=="ARID") |> 
   # filter(year>=2010) |> #for regals, goes back to '98, can alter here
  filter(behavior==3) |> 
  filter(nectar_species_cleaned!= "NA") |> 
  filter(field!= "Boyer")

regal_dist_matrix<- regal_prep |> 
  count(year, month, field, nectar_species_cleaned) |> 
  pivot_wider(names_from = nectar_species_cleaned,
              values_from = n,
              values_fill = list(n = 0))
# #Remove outlier columns
# regal_nectar_counts <- colSums(regal_dist_matrix != 0) 
# regal_keep_cols<- regal_nectar_counts>=10
# regal_filtered_outliers<- regal_dist_matrix[, regal_keep_cols]
# regal_filtered_outliers<- as.data.frame(regal_filtered_outliers)

rownames(regal_dist_matrix) <- paste(regal_dist_matrix$year,
                                   regal_dist_matrix$month,
                                   regal_dist_matrix$field,
                                   sep = "_")


#remove metadata
regal_numeric_only <- as.data.frame(regal_dist_matrix[, !(names(regal_dist_matrix) %in% c("year", "month", "field"))]) 
rownames(regal_numeric_only) <- rownames(regal_dist_matrix)


#remove outlier columns and rows
regal_nectar_counts <- colSums(regal_numeric_only != 0) 
regal_keep_cols<- regal_nectar_counts>=10
regal_filtered_outliers<- regal_numeric_only[, regal_keep_cols]
regal_row_sums <- rowSums(regal_filtered_outliers, na.rm = TRUE)
regal_keep_rows<-regal_row_sums>=5
regal_numeric_filtered<- regal_filtered_outliers[regal_keep_rows, ]


str(regal_numeric_filtered)

set.seed(82402)
regal_nmds<- metaMDS(regal_numeric_filtered,
                     distance= "bray",
                     wascores = TRUE,
                     trymax= 500,
                     autotransform = FALSE)

regal_plant_scores<-scores(arid_nmds, display= "species")
regal_plant_df<-as.data.frame(regal_plant_scores)
regal_plant_df$nectar_species_cleaned<-rownames(regal_plant_df)

# plot(regal_nmds)
# 
 nmds_points<- as.data.frame(regal_nmds$points)
# 
# filter<- nmds_points |> 
#   filter(MDS1 < ) |> 
#   filter(MDS2< 500)
# 
# plot(filter)
# 
# scores_regal<- as.data.frame(scores(nmds_points, display = "sites"))
# 
# ggplot(scores_regal, aes(x=MDS1, y= MDS2))+
#   geom_point()

 #get metadata attached
 regal_numeric_filtered$combo_id <- rownames(regal_numeric_filtered)
 
 # Then separate that column back into original parts
 parts <- do.call(rbind, strsplit(regal_numeric_filtered$combo_id, "_"))
 colnames(parts) <- c("year", "month", "field")

 # Bind the split columns to your data frame
matrix_scores <- cbind(parts, nmds_points)

 
 # Optionally remove the combo_id column
 regal_filtered_outliers$combo_id <- NULL
#matrix_scores<- cbind(nmds_points, regal_metadata)
 
#clustering! (spoiler, the cluster is by month)
 kmeans_result <- kmeans(nmds_points, centers = 5)
 clusters <- kmeans_result$cluster
nmds_points$cluster <- as.factor(clusters)
 ggplot(nmds_points, aes(x = MDS1, y = MDS2, color = cluster)) +
   geom_point(size = 3) +
   labs(title = "NMDS Clustering") +
   theme_minimal()
 
 #prep for primer
 primerbalh<- cbind(parts, regal_numeric_filtered)
write.xlsx(primerbalh, "C:/Users/GCano/OneDrive - Temple University/primermatrix.xlsx")
 
 
 
 
 
 ?metaMDS
 

#plot with species scores
ggplot()+
  geom_point(data= matrix_scores, aes(x=MDS1, y=MDS2, shape= as.factor(month)),
             size=3)+
  geom_label(data= regal_plant_df, aes(x= NMDS1, y=NMDS2, label= nectar_species_cleaned))+
  stat_ellipse(data= matrix_scores, aes(x=MDS1,y=MDS2, color=month),level = 0.50)

#finding convex hulls for the months  
hull_data_month<-matrix_scores |> 
  group_by(month) |> 
  slice(chull(MDS1, MDS2))

#plot with month polygons
ggplot()+
  geom_point(data= arid_matrix_scores, aes(x=MDS1, y=MDS2, color= as.factor(month), shape= as.factor(month)),
             size=3)+
  geom_label(data= regal_plant_df, aes(x= NMDS1, y=NMDS2, label= nectar_species_cleaned))+
geom_polygon(data = arid_hull_data_month, 
             aes(x = MDS1, y = MDS2, group = month, fill = as.factor(month)), 
             alpha = 0.1, color = "black")+
  scale_shape_manual(values = c(15, 17, 19, 8)) +  # square, triangle, circle
  scale_fill_manual(values= okabe_ito_extended) +
  scale_color_manual(values = okabe_ito_extended)+
  stat_ellipse(data= arid_matrix_scores, aes(x=MDS1,y=MDS2, color=month),level = 0.50) 

ggplot()+
  geom_point(data= matrix_scores, aes(x=MDS1, y=MDS2, color= as.factor(year)),
             size=3)+
  geom_polygon(data = hull_data_month, 
               aes(x = MDS1, y = MDS2, group = month, fill = as.factor(month)), 
               alpha = 0.3, color = "black")+
  facet_grid(~month)

#month/year combinations
ymcombo_matrix_scores <- matrix_scores %>%
  mutate(month_year = paste0(year, "_", month))

ym_hull_data <- ymcombo_matrix_scores %>%
  group_by(month_year) %>%
  slice(chull(MDS1, MDS2)) 

ggplot(ymcombo_matrix_scores, aes(x = MDS1, y = MDS2, color=year, shape= month)) +
   geom_point() +
  geom_polygon(data = ym_hull_data,
                aes(fill = year, group = month_year),
                color = "black", alpha = 0.3)+
  facet_wrap(~month)+
  scale_shape_manual(values = c(15, 17, 19, 8)) # square, triangle, circle
  


#month/site combinations
mfcombo_matrix_scores <- matrix_scores %>%
  mutate(month_field = paste0(month, "_", field))

mf_hull_data <- mfcombo_matrix_scores %>%
  group_by(month_field) %>%
  slice(chull(MDS1, MDS2)) 

ggplot(mfcombo_matrix_scores, aes(x = MDS1, y = MDS2)) +
  geom_point() +
  geom_polygon(data = mf_hull_data,
               aes(fill = month_field, group = month_field),
               color = "black", alpha = 0.3)+
  facet_wrap(~month)

#finding convex hulls for the years
hull_data_year<-matrix_scores |> 
  group_by(year) |> 
  slice(chull(MDS1, MDS2))

#plot with year polygons
ggplot()+
  geom_point(data= matrix_scores, aes(x=MDS1, y=MDS2, color= as.factor(year), shape= as.factor(month)),
             size=3)+
  geom_polygon(data = hull_data_year, 
               aes(x = MDS1, y = MDS2, group = year, fill = as.factor(year)), 
               alpha = 0.3, color = "black")+
  facet_grid(~ month)

#finding convex hulls for the field
hull_data_field<-matrix_scores |> 
  group_by(field) |> 
  slice(chull(MDS1, MDS2))

#plot with year polygons
ggplot()+
  geom_point(data= matrix_scores, aes(x=MDS1, y=MDS2, color= as.factor(year), shape= as.factor(month)),
             size=3)+
  geom_label(data= regal_plant_df, aes(x= NMDS1, y=NMDS2, label= nectar_species_cleaned))+
  geom_polygon(data = hull_data_field, 
               aes(x = MDS1, y = MDS2, group = field, fill = as.factor(field)), 
               alpha = 0.3, color = "black")



?vegdist

#regal permanova--------------------
set.seed(82402)


group1_sites <- c("R23", "C4", "B12")
group2_sites <- c("D1", "D3")

regal_return_matrix$site_group <- ifelse(regal_return_matrix$field %in% group1_sites, 
                                         "Group1", 
                                         ifelse(regal_return_matrix$field %in% group2_sites, 
                                                "Group2", NA))

grouped_data <- regal_return_matrix[!is.na(regal_return_matrix$site_group), ]


adonis2(regal_perm_dist~site_group,
        data=grouped_data,
        permutations = 10000)





regal_perm_prep<- regal_numeric_filtered$combo_id <- NULL

regal_perm_dist<- vegdist(regal_numeric_filtered, method= "bray",
                          )

meandist(dist = vegdist(regal_numeric_filtered),
         grouping = parts)


set.seed(82402)
trial_nmds<- metaMDS(regal_perm_dist)
scores(trial_nmds) |> 
  as_tibble(rownames= "sample") |> 
  ggplot(aes(x=NMDS1, y=NMDS2))+
  geom_p

regal_return_matrix<- cbind(parts, regal_numeric_filtered) |> 
  as.data.frame() |> 
  mutate(year= as.factor(year),
         month= as.factor(month),
         field= as.factor(field)) |> 
  select("year", "month", "field")

?adonis2

#year as factor
regal_perm_results <- list(
  year= adonis2(regal_perm_dist ~ year, 
        data = regal_return_matrix, 
        permutations = 999),
month= adonis2(regal_perm_dist ~ month,
        data = regal_return_matrix, 
        permutations = 999),
year_month= adonis2(regal_perm_dist~year*month,
        data= regal_return_matrix, 
        permutations = 999),
field= adonis2(regal_perm_dist~field,
        data= regal_return_matrix, 
        permutations = 999),
month_field= adonis2(regal_perm_dist~ month*field,
        data= regal_return_matrix, 
        permutations = 999),
year_field= adonis2(regal_perm_dist~ field*year,
        data= regal_return_matrix,
        permutations = 999))

summary_table <- do.call(rbind, lapply(names(regal_perm_results), function(name) {
  result <- regal_perm_results[[name]]
  data.frame(
    model = name,
    R2 = result$R2[1],
    F = result$F[1],
    p = result$`Pr(>F)`[1],
    row.names = NULL
  )
}))

# View the summary table
print(summary_table)
#year as covariate (continuous)

regal_return_matrix$year <- as.numeric(as.character(regal_return_matrix$year))

regal_year_num_perm_results <- list(
  year            = adonis2(regal_perm_dist ~ year, 
                            data = regal_return_matrix, 
                            permutations = 10000),
  
  month           = adonis2(regal_perm_dist ~ month,
                            data = regal_return_matrix, 
                            permutations = 10000),
  
  year_month      = adonis2(regal_perm_dist ~ year + month + year:month,
                            data = regal_return_matrix, 
                            permutations = 10000),
  
  field           = adonis2(regal_perm_dist ~ field,
                            data = regal_return_matrix, 
                            permutations = 10000),
  
  month_field     = adonis2(regal_perm_dist ~ month * field,
                            data = regal_return_matrix, 
                            permutations = 10000),
  
  year_field      = adonis2(regal_perm_dist ~ year + field + year:field,
                            data = regal_return_matrix, 
                            permutations = 10000)
)

yr_num_summary_table <- do.call(rbind, lapply(names(regal_year_num_perm_results), function(name) {
  result <- regal_year_num_perm_results[[name]]
  data.frame(
    model = name,
    R2 = result$R2[1],
    F = result$F[1],
    p = result$`Pr(>F)`[1],
    row.names = NULL
  )
}))

# View the summary table
print(yr_num_summary_table)


#instead of month, every 2 weeks

regal_week<- regal_prep |> 
  mutate(week_2= floor((week-1)/2)+1) |> 
  group_by(year, week_2, nectar_species_cleaned, field) |> 
  summarize(count= n(), .groups= "drop") |> 
  pivot_wider(names_from = nectar_species_cleaned,
              values_from = count,
              values_fill = list(count = 0))

rownames(regal_week) <- paste(regal_week$year,
                                     regal_week$week_2,
                                     regal_week$field,
                                     sep = "_")


#remove metadata
regal_week_numeric_only <- as.data.frame(regal_week[, !(names(regal_week) %in% c("year", "week_2", "field"))]) 
rownames(regal_week_numeric_only) <- rownames(regal_week)


#remove outlier columns and rows
regal_week_nectar_counts <- colSums(regal_week_numeric_only != 0) 
regal_week_keep_cols<- regal_week_nectar_counts>=10
regal_week_filtered_outliers<- regal_week_numeric_only[, regal_week_keep_cols]
regal_week_row_sums <- rowSums(regal_week_filtered_outliers, na.rm = TRUE)
regal_week_keep_rows<-regal_week_row_sums>=5
regal_week_numeric_filtered<- regal_week_filtered_outliers[regal_week_keep_rows, ]

regal_week_perm_dist<- vegdist(regal_week_numeric_filtered, method= "bray",
)

#get metadata attached
regal_week_numeric_filtered$combo_id <- rownames(regal_week_numeric_filtered)

# Then separate that column back into original parts
week_parts <- do.call(rbind, strsplit(regal_week_numeric_filtered$combo_id, "_"))
colnames(week_parts) <- c("year", "week", "field")

regal_week_return_matrix<- cbind(week_parts, regal_week_numeric_filtered) |> 
  as.data.frame() |> 
  mutate(year= as.factor(year),
         week= as.factor(week),
         field= as.factor(field)) |> 
  select("year", "week", "field")

regal_week_perm_results <- list(
  year= adonis2(regal_week_perm_dist ~ year, 
                data = regal_week_return_matrix, 
                permutations = 10000),
  week= adonis2(regal_week_perm_dist ~ week,
                 data = regal_week_return_matrix, 
                 permutations = 10000),
  year_week= adonis2(regal_week_perm_dist~year*week,
                      data= regal_week_return_matrix, 
                      permutations = 10000),
  field= adonis2(regal_week_perm_dist~field,
                 data= regal_week_return_matrix, 
                 permutations = 10000),
  week_field= adonis2(regal_week_perm_dist~ week*field,
                       data= regal_week_return_matrix, 
                       permutations = 1000),
  year_field= adonis2(regal_week_perm_dist~ field*year,
                      data= regal_week_return_matrix, 
                      permutations = 10000))

week_summary_table <- do.call(rbind, lapply(names(regal_week_perm_results), function(name) {
  result <- regal_week_perm_results[[name]]
  data.frame(
    model = name,
    R2 = result$R2[1],
    F = result$F[1],
    p = result$`Pr(>F)`[1],
    row.names = NULL
  )
}))

# View the summary table
print(week_summary_table)






# adonis2(regal_perm_dist~ year + month + field,
#                            data= regal_return_matrix, permutations=1000,
#         by= "margin")
# 
# adonis2(regal_perm_dist~ year: month + year:fiel
#         data= regal_return_matrix, permutations=1000,
#         by= "margin")

# regal_perm_results <- list(
#   year           = adonis2(regal_perm_dist ~ year, method= "bray" , perm= 999, data = regal_return_matrix, by= "margin"),
#   month          = adonis2(regal_perm_dist ~ month, method= "bray" , perm= 999, data = regal_return_matrix, by= "margin"),
#   field           = adonis2(regal_perm_dist ~ field,method= "bray" , perm= 999, data = regal_return_matrix, by= "margin"),
#   field_within_year   = adonis2(regal_perm_dist ~ year/field,method= "bray" , perm= 999, data = regal_return_matrix, by= "margin"),
#   month_within_year  = adonis2(regal_perm_dist ~ year/month,method= "bray" , perm= 999, data = regal_return_matrix, by= "margin"),
#   field_within_month  = adonis2(regal_perm_dist ~ month/field, method= "bray" , perm= 999, data = regal_return_matrix, by= "margin"),
#   year_within_field= adonis2(regal_perm_dist ~ field/year,method= "bray" , perm= 999, data = regal_return_matrix, by= "margin"),
#   year_within_month= adonis2(regal_perm_dist ~ month/year,method= "bray" , perm= 999, data = regal_return_matrix, by= "margin"),
#   month_within_field= adonis2(regal_perm_dist ~ field/month,method= "bray" , perm= 999, data = regal_return_matrix, by= "margin")
# )

#not a distance matrix? you get the same stats so i am a little confused here
# nd_regal_perm_results <- list(
#   year           = adonis2(regal_numeric_filtered ~ year, method= "bray" , perm= 999, data = regal_return_matrix),
#   month          = adonis2(regal_numeric_filtered ~ month, method= "bray" , perm= 999, data = regal_return_matrix),
#   field           = adonis2(regal_numeric_filtered ~ field,method= "bray" , perm= 999, data = regal_return_matrix),
#   field_within_year   = adonis2(regal_numeric_filtered ~ year/field,method= "bray" , perm= 999, data = regal_return_matrix),
#   month_within_year  = adonis2(regal_numeric_filtered ~ year/month,method= "bray" , perm= 999, data = regal_return_matrix),
#   field_within_month  = adonis2(regal_numeric_filtered ~ month/field, method= "bray" , perm= 999, data = regal_return_matrix),
#   year_within_field= adonis2(regal_numeric_filtered ~ field/year,method= "bray" , perm= 999, data = regal_return_matrix),
#   year_within_month= adonis2(regal_numeric_filtered ~ month/year,method= "bray" , perm= 999, data = regal_return_matrix),
#   month_within_field= adonis2(regal_numeric_filtered ~ field/month,method= "bray" , perm= 999, data = regal_return_matrix)
# )

#summary of perm results
summary_table <- do.call(rbind, lapply(names(regal_perm_results), function(name) {
  result <- regal_perm_results[[name]]
  data.frame(
    model = name,
    R2 = result$R2[1],
    F = result$F[1],
    p = result$`Pr(>F)`[1],
    row.names = NULL
  )
}))

# View the summary table
print(summary_table)



#ets nmds model-----------------------------------------------

ets_prep<- matrix_98_24 |> 
  filter(butterfly_species_cleaned=="PAGL") |> 
  # filter(year>=2010) |> #for regals, goes back to '98, can alter here
  filter(behavior==3) |> 
  filter(nectar_species_cleaned!= "NA") |> 
  filter(field %in% c("B12", "C4", "D1", "D3", "R23"))

ets_dist_matrix<- ets_prep |> 
  count(year, month, field, nectar_species_cleaned) |> 
  pivot_wider(names_from = nectar_species_cleaned,
              values_from = n,
              values_fill = list(n = 0))

rownames(ets_dist_matrix) <- paste(ets_dist_matrix$year,
                                     ets_dist_matrix$month,
                                     ets_dist_matrix$field,
                                     sep = "_")


#remove metadata
ets_numeric_only <- as.data.frame(ets_dist_matrix[, !(names(ets_dist_matrix) %in% c("year", "month", "field"))]) 
rownames(ets_numeric_only) <- rownames(ets_dist_matrix)


#remove outlier columns and rows
ets_nectar_counts <- colSums(ets_numeric_only != 0) 
ets_keep_cols<- ets_nectar_counts>=10
ets_filtered_outliers<- ets_numeric_only[, ets_keep_cols]
ets_row_sums <- rowSums(ets_filtered_outliers, na.rm = TRUE)
ets_keep_rows<-ets_row_sums>=5
ets_numeric_filtered<- ets_filtered_outliers[ets_keep_rows, ]

# #Remove outliers
# ets_nectar_counts <- colSums(ets_dist_matrix != 0) 
# ets_keep_cols<- ets_nectar_counts>=10
# ets_filtered_outliers<- ets_dist_matrix[, ets_keep_cols]
# ets_row_obs <- rowSums(ets_filtered_outliers != 0)
# ets_filtered_matrix <- ets_filtered_outliers[ets_row_obs >= 5, ]

set.seed(82402)
ets_nmds<- metaMDS(ets_numeric_filtered,
                     distance= "bray",
                     wascores = TRUE,
                     trymax= 500,
                     autotransform = FALSE)


ets_metadata <- ets_filtered_matrix |> 
  select(year, month, field)


ets_numeric_only<-ets_filtered_matrix|> 
  select(-year, -month, -field)

str(ets_numeric_only)

set.seed(82402)
ets_nmds<- metaMDS(ets_numeric_only,
                     distance= "bray",
                     wascores = TRUE,
                     trymax= 100)

ets_plant_scores<-scores(ets_nmds, display= "species")
ets_plant_df<-as.data.frame(ets_plant_scores)
ets_plant_df$nectar_species_cleaned<-rownames(ets_plant_df)

plot(ets_nmds)

ets_nmds_points<- as.data.frame(ets_nmds$points)

#get metadata attached
ets_numeric_filtered$combo_id <- rownames(ets_numeric_filtered)

# Then separate that column back into original parts
ets_parts <- do.call(rbind, strsplit(ets_numeric_filtered$combo_id, "_"))
colnames(ets_parts) <- c("year", "month", "field")

# Bind the split columns to your data frame
ets_matrix_scores <- cbind(ets_parts, ets_nmds_points)

ggplot()+
  geom_point(data= ets_matrix_scores, aes(x=MDS1, y=MDS2, shape= as.factor(month)),
             size=3)+
  geom_label(data= ets_plant_df, aes(x= NMDS1, y=NMDS2, label= nectar_species_cleaned))+
  stat_ellipse(data= ets_matrix_scores, aes(x=MDS1,y=MDS2, color=month),level = 0.50)

#ets permanova--------------------------------
ets_perm_prep<- ets_numeric_filtered$combo_id <- NULL

ets_perm_dist<- vegdist(ets_numeric_filtered, method= "bray",
)

ets_return_matrix<- cbind(ets_parts, ets_numeric_filtered) |> 
  as.data.frame() |> 
  mutate(year= as.factor(year),
         month= as.factor(month),
         field= as.factor(field)) |> 
  select("year", "month", "field")

?adonis2

ets_perm_results <- list(
  year= adonis2(ets_perm_dist ~ year, 
                data = ets_return_matrix, 
                permutations = 10000),
  month= adonis2(ets_perm_dist ~ month,
                 data = ets_return_matrix, 
                 permutations = 10000),
  year_month= adonis2(ets_perm_dist~year*month,
                      data= ets_return_matrix, 
                      permutations = 10000),
  site= adonis2(ets_perm_dist~field,
                data= ets_return_matrix, 
                permutations = 10000),
  month_site= adonis2(ets_perm_dist~ month*field,
                      data= ets_return_matrix, 
                      permutations = 10000),
  year_site= adonis2(ets_perm_dist~ field*year,
                     data= ets_return_matrix, 
                     permutations = 10000))


ets_summary_table <- do.call(rbind, lapply(names(ets_perm_results), function(name) {
  result <- ets_perm_results[[name]]
  data.frame(
    model = name,
    R2 = result$R2[1],
    F = result$F[1],
    p = result$`Pr(>F)`[1],
    row.names = NULL
  )
}))

# View the summary table
print(ets_summary_table)


plot(filter)

scores_ets<- as.data.frame(scores(ets_points, display = "sites"))

ggplot(scores_regal, aes(x=MDS1, y= MDS2))+
  geom_point()


ets_matrix_scores<- cbind(ets_nmds_points, ets_metadata)

#plot with species scores
ggplot()+
  geom_point(data= ets_matrix_scores, aes(x=MDS1, y=MDS2, color= as.factor(year), shape= as.factor(month)),
             size=3)+
  geom_label(data= ets_plant_df, aes(x= NMDS1, y=NMDS2, label= nectar_species_cleaned))

#finding convex hulls for the months  
ets_hull_data_month<-ets_matrix_scores |> 
  group_by(month) |> 
  slice(chull(MDS1, MDS2))

#plot with month polygons
ggplot()+
  geom_point(data= ets_matrix_scores, aes(x=MDS1, y=MDS2, color= as.factor(year), shape= as.factor(month)),
             size=3)+
  geom_label(data= ets_plant_df, aes(x= NMDS1, y=NMDS2, label= nectar_species_cleaned))+
  geom_polygon(data = ets_hull_data_month, 
               aes(x = MDS1, y = MDS2, group = month, fill = as.factor(month)), 
               alpha = 0.3, color = "black")

#finding convex hulls for the years
hull_data_year<-matrix_scores |> 
  group_by(year) |> 
  slice(chull(MDS1, MDS2))

#plot with year polygons
ggplot()+
  geom_point(data= matrix_scores, aes(x=MDS1, y=MDS2, color= as.factor(year), shape= as.factor(month)),
             size=3)+
  geom_label(data= regal_plant_df, aes(x= NMDS1, y=NMDS2, label= nectar_species_cleaned))+
  geom_polygon(data = hull_data_year, 
               aes(x = MDS1, y = MDS2, group = year, fill = as.factor(year)), 
               alpha = 0.3, color = "black")

#finding convex hulls for the field
ets_hull_data_field<-ets_matrix_scores |> 
  group_by(field) |> 
  slice(chull(MDS1, MDS2))

#plot with year polygons
ggplot()+
  geom_point(data= ets_matrix_scores, aes(x=MDS1, y=MDS2, color= as.factor(year), shape= as.factor(month)),
             size=3)+
  geom_label(data= ets_plant_df, aes(x= NMDS1, y=NMDS2, label= nectar_species_cleaned))+
  geom_polygon(data = ets_hull_data_field, 
               aes(x = MDS1, y = MDS2, group = field, fill = as.factor(field)), 
               alpha = 0.3, color = "black")



?vegdist
