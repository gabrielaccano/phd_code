#Network Analysis
#Gabriela Cano
#Nov 10, 2025

#import packages
library(tidyverse)
library(readxl)
library(bipartite)
library(ggpubr)


#read in data set
matrix<- read_excel("C:/Users/GCano/Documents/GitHub/phd_code/butterfly_data_16_25/complete pollard data CRT_January2026.xlsx")

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
  filter(!is.na(nectar_species_cleaned)) |> 
  mutate(month= as.factor(month),
         year= as.factor(year))
  
  
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
plotweb(net_df,
        scaling = "relative",
        higher_color = "pink",
        lower_color = "lightblue",
        text_size = .5,
        sorting = "decr")#web

visweb(net_df,
       labsize= 2) #heatmap

?plotweb

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


#do the network indices change over years?-------------------------


years <- 2007:2025

results_year <- lapply(years, function(y) {
  
  # Filter for this year
  use_butt <- g_sp_matrix |>
    filter(butterfly_species_cleaned %in% highlight_butt$butterfly_species_cleaned) |>
    filter(year == y)
  
  # Create bipartite matrix
  net_butt <- table(use_butt$butterfly_species_cleaned,
                    use_butt$nectar_species_cleaned)
  
  net_df <- as.data.frame.matrix(net_butt)
  
  # Compute network indices
  ind <- networklevel(net_df, index = c("connectance", "nestedness", "H2"))
  
  # Return a row as a data frame
  data.frame(
    year = y,
    connectance = ind["connectance"],
    nestedness = ind["nestedness"],
    H2 = ind["H2"]
  )
})

# Combine into a single dataframe
network_indices_by_year <- do.call(rbind, results_year)

connect<- ggplot(network_indices_by_year, aes(y=connectance, x=year))+
  geom_point()+
  geom_smooth()

nest<- ggplot(network_indices_by_year, aes(y=nestedness, x=year))+
  geom_point()+
  geom_smooth()

h2<- ggplot(network_indices_by_year, aes(y=H2, x=year))+
  geom_point()+
  geom_smooth()


ggarrange(connect, nest, h2)

#connectance appears to decrease suggesting that fewer expected interactions are
#occuring over the years, which may suggest an overall specialization or greater
#phenological mismatch
#nestedness also decreases which suggest that specialists and generalists are may 
#be facing some sort of community fragmentation
#H2 increases which means butterflies are using fewer species and the interactions are
#becoming more exclusive

#how do the indices change in each month?---------------------
#network values for each month
jun_butt<-use_butt |> 
  filter(month==6)
jun_net_butt <- table(jun_butt$butterfly_species_cleaned, jun_butt$nectar_species_cleaned) #cross product of columns 25 and 26 (butterfly_species_cleaned and nectar_species_cleaned)
jun_net_df<- as.data.frame.matrix(jun_net_butt)
jun_net_indices<- networklevel(jun_net_df, index=c("connectance", "nestedness", "H2"))
print(jun_net_indices)

#connectance  nestedness          H2 
#0.1748971   5.1774693   0.2244043 

jul_butt<-use_butt |> 
  filter(month==7)
jul_net_butt <- table(jul_butt$butterfly_species_cleaned, jul_butt$nectar_species_cleaned) #cross product of columns 25 and 26 (butterfly_species_cleaned and nectar_species_cleaned)
jul_net_df<- as.data.frame.matrix(jul_net_butt)
jul_net_indices<- networklevel(jul_net_df, index=c("connectance", "nestedness", "H2"))
print(jul_net_indices)

#connectance  nestedness          H2 
#0.2095960   8.3120520   0.1948036 

aug_butt<-use_butt |> 
  filter(month==8)
aug_net_butt <- table(aug_butt$butterfly_species_cleaned, aug_butt$nectar_species_cleaned) #cross product of columns 25 and 26 (butterfly_species_cleaned and nectar_species_cleaned)
aug_net_df<- as.data.frame.matrix(aug_net_butt)
aug_net_indices<- networklevel(aug_net_df, index=c("connectance", "nestedness", "H2"))
print(aug_net_indices)

#connectance  nestedness          H2 
#0.2193126   6.3256924   0.1812610

sep_butt<-use_butt |> 
  filter(month==9)
sep_net_butt <- table(sep_butt$butterfly_species_cleaned, sep_butt$nectar_species_cleaned) #cross product of columns 25 and 26 (butterfly_species_cleaned and nectar_species_cleaned)
sep_net_df<- as.data.frame.matrix(sep_net_butt)
sep_net_indices<- networklevel(sep_net_df, index=c("connectance", "nestedness", "H2"))
print(sep_net_indices)

#connectance  nestedness          H2 
#0.1801948   8.9641785   0.3715341

#by year month combinations
network_in_by_month<-rbind(network_indices_by_aug, network_indices_by_july, network_indices_by_june, network_indices_by_sep)

connect_month<- ggplot(network_in_by_month, aes(y=connectance, x=year, color=as.factor(month)))+
  geom_point(alpha=.4)+
  geom_smooth(se=FALSE)

nest_month<- ggplot(network_in_by_month, aes(y=nestedness, x=year, color=as.factor(month)))+
  geom_point(alpha=.4)+
  geom_smooth(se=FALSE)

h2_month<- ggplot(network_in_by_month, aes(y=H2, x=year, color= as.factor(month)))+
  geom_point(alpha=.4)+
  geom_smooth(se=FALSE)

ggarrange(connect_month, nest_month, h2_month)
#june-----------
results_june <- lapply(years, function(y) {
  
  # Filter for this year
  use_butt <- g_sp_matrix |>
    filter(butterfly_species_cleaned %in% highlight_butt$butterfly_species_cleaned) |>
    filter(year == y) |> 
    filter(month==6)
  
  # Create bipartite matrix
  net_butt <- table(use_butt$butterfly_species_cleaned,
                    use_butt$nectar_species_cleaned)
  
  net_df <- as.matrix(net_butt)
  
  #drops empty rows/columns because not every combination happens every month
  net_df <- net_df[rowSums(net_df) > 0, colSums(net_df) > 0]
  #Making sure there are enough observations
  if (nrow(net_df) < 2 || ncol(net_df) < 2) return(NULL)
  
  # Compute network indices
  ind <- networklevel(net_df, index = c("connectance", "nestedness", "H2"))
  
  # Return a row as a data frame
  data.frame(
    year = y,
    month=6,
    connectance = ind["connectance"],
    nestedness = ind["nestedness"],
    H2 = ind["H2"]
  )
})

network_indices_by_june <- do.call(rbind, results_june)

connect_june<- ggplot(network_indices_by_june, aes(y=connectance, x=year))+
  geom_point()+
  geom_smooth(method= "lm")

nest_june<- ggplot(network_indices_by_june, aes(y=nestedness, x=year))+
  geom_point()+
  geom_smooth(method="lm")

h2_june<- ggplot(network_indices_by_june, aes(y=H2, x=year))+
  geom_point()+
  geom_smooth(method='lm')


ggarrange(connect_june, nest_june, h2_june)
#july-----------------
results_july <- lapply(years, function(y) {
  
  # Filter for this year
  use_butt <- g_sp_matrix |>
    filter(butterfly_species_cleaned %in% highlight_butt$butterfly_species_cleaned) |>
    filter(year == y) |> 
    filter(month==7)
  
  # Create bipartite matrix
  net_butt <- table(use_butt$butterfly_species_cleaned,
                    use_butt$nectar_species_cleaned)
  
  net_df <- as.matrix(net_butt)
  
  #drops empty rows/columns because not every combination happens every month
  net_df <- net_df[rowSums(net_df) > 0, colSums(net_df) > 0]
  #Making sure there are enough observations
  if (nrow(net_df) < 2 || ncol(net_df) < 2) return(NULL)
  
  # Compute network indices
  ind <- networklevel(net_df, index = c("connectance", "nestedness", "H2"))
  
  # Return a row as a data frame
  data.frame(
    year = y,
    month=7,
    connectance = ind["connectance"],
    nestedness = ind["nestedness"],
    H2 = ind["H2"]
  )
})

network_indices_by_july <- do.call(rbind, results_july)

connect_july<- ggplot(network_indices_by_july, aes(y=connectance, x=year))+
  geom_point()+
  geom_smooth()

nest_july<- ggplot(network_indices_by_july, aes(y=nestedness, x=year))+
  geom_point()+
  geom_smooth()

h2_july<- ggplot(network_indices_by_july, aes(y=H2, x=year))+
  geom_point()+
  geom_smooth()


ggarrange(connect_july, nest_july, h2_july)
#august-------------------
results_aug <- lapply(years, function(y) {
  
  # Filter for this year
  use_butt <- g_sp_matrix |>
    filter(butterfly_species_cleaned %in% highlight_butt$butterfly_species_cleaned) |>
    filter(year == y) |> 
    filter(month==8)
  
  # Create bipartite matrix
  net_butt <- table(use_butt$butterfly_species_cleaned,
                    use_butt$nectar_species_cleaned)
  
  net_df <- as.matrix(net_butt)
  
  #drops empty rows/columns because not every combination happens every month
  net_df <- net_df[rowSums(net_df) > 0, colSums(net_df) > 0]
  #Making sure there are enough observations
  if (nrow(net_df) < 2 || ncol(net_df) < 2) return(NULL)
  
  # Compute network indices
  ind <- networklevel(net_df, index = c("connectance", "nestedness", "H2"))
  
  # Return a row as a data frame
  data.frame(
    year = y,
    month=8,
    connectance = ind["connectance"],
    nestedness = ind["nestedness"],
    H2 = ind["H2"]
  )
})

network_indices_by_aug <- do.call(rbind, results_aug)

connect_aug<- ggplot(network_indices_by_aug, aes(y=connectance, x=year))+
  geom_point()+
  geom_smooth(method="lm")

nest_aug<- ggplot(network_indices_by_aug, aes(y=nestedness, x=year))+
  geom_point()+
  geom_smooth(method="lm")

h2_aug<- ggplot(network_indices_by_aug, aes(y=H2, x=year))+
  geom_point()+
  geom_smooth(method="lm")


ggarrange(connect_aug, nest_aug, h2_aug)
#september---------------------------
results_sep <- lapply(years, function(y) {
  
  # Filter for this year
  use_butt <- g_sp_matrix |>
    filter(butterfly_species_cleaned %in% highlight_butt$butterfly_species_cleaned) |>
    filter(year == y) |> 
    filter(month==9)
  
  # Create bipartite matrix
  net_butt <- table(use_butt$butterfly_species_cleaned,
                    use_butt$nectar_species_cleaned)
  
  net_df <- as.matrix(net_butt)
  
  #drops empty rows/columns because not every combination happens every month
  net_df <- net_df[rowSums(net_df) > 0, colSums(net_df) > 0]
  #Making sure there are enough observations
  if (nrow(net_df) < 2 || ncol(net_df) < 2) return(NULL)
  
  # Compute network indices
  ind <- networklevel(net_df, index = c("connectance", "nestedness", "H2"))
  
  # Return a row as a data frame
  data.frame(
    year = y,
    month=9,
    connectance = ind["connectance"],
    nestedness = ind["nestedness"],
    H2 = ind["H2"]
  )
})

network_indices_by_sep <- do.call(rbind, results_sep)

connect_sep<- ggplot(network_indices_by_sep, aes(y=connectance, x=year))+
  geom_point()+
  geom_smooth()

nest_sep<- ggplot(network_indices_by_sep, aes(y=nestedness, x=year))+
  geom_point()+
  geom_smooth()

h2_sep<- ggplot(network_indices_by_sep, aes(y=H2, x=year))+
  geom_point()+
  geom_smooth()


ggarrange(connect_sep, nest_sep, h2_sep)

#what happens if i throw climate into this mix------------------------

climate<-read.csv("C:/Users/GCano/Documents/GitHub/phd_code/climate_data/weather data Harrisburg International.csv") |> 
  janitor::clean_names()

summer_clim<- climate |> 
  mutate(month= month(date),
         year= year(date)) |> 
  filter(year>=2007,
         month>=5,
         month<=9)

monthly_climate <- summer_clim |>
  group_by(year, month) |>
  summarise(
    max_temp = max(tmax, na.rm = TRUE),
    mean_precip= mean(prcp, na.rm= TRUE),
    min_temp= min(tmin, na.rm= TRUE),
    avg_temp= mean(((tmax+tmin)/2), na.rm= TRUE),
    .groups = "drop"
  )

#function making connectance, nestedness, and h2 values for each month/year combination

ym = expand_grid(year = 2007:2025,
                 month = 5:9)
ym$year.month = interaction(ym$year, ym$month, drop = TRUE)

data = use_butt |> 
  mutate(month = as.numeric(month(date)),
         year.month = interaction(year, month, drop = TRUE)) %>% 
  filter(year.month %in% ym$year.month)

ym= ym %>% 
  filter(year.month %in% data$year.month)

ym$year.month= droplevels(ym$year.month)

data$year.month = droplevels(data$year.month)

y.m = list()
net_butt = list()
net_df = list()
ind = list()
select= list ()
ind_list= list()

for(i in 1:nrow(ym)) {
  select[[i]] = data |> 
    filter(year.month == ym$year.month[[i]])
  select[[i]] = data.frame(select[[i]])
  
  net_butt[[i]] <- table(
    select[[i]]$butterfly_species_cleaned,
    select[[i]]$nectar_species_cleaned
  )
  
  net_df[[i]] <- as.matrix(net_butt[[i]])
  
  #drops empty rows/columns because not every combination happens every month
  net_df[[i]] <- net_df[[i]][rowSums(net_df[[i]]) > 0, colSums(net_df[[i]]) > 0, drop= FALSE]
  #Making sure there are enough observations
   #if (nrow(net_df[[i]]) < 2 ||
       #ncol(net_df[[i]]) < 2) {
      # return(NULL)}
  
  # Compute network indices
  ind[[i]] <- networklevel(net_df[[i]], index = c("connectance", "nestedness", "H2"))
  ind_list[[i]] <- list(
    year  = ym$year[[i]],
    month = ym$month[[i]],
    year.month = ym$year.month[[i]],
    ind = ind[[i]]
  )
      
}

#dataframe with indices and ym values
ind_df <- do.call(rbind, lapply(ind_list, function(x) {
  if (is.null(x)) return(NULL)
  
  data.frame(
    year        = x$year,
    month       = x$month,
    year_month  = x$year.month,
    connectance = x$ind["connectance"],
    nestedness  = x$ind["nestedness"],
    H2          = x$ind["H2"]
  )}))



# y.m = list()
# net_butt = list()
# net_df = list()
# ind = list()
# select= list ()
# ind_list= list()
# 
# 
# for(i in 1:nrow(ym)) {
#   
#   select[[i]] = data |> 
#     filter(year.month == ym$year.month[[i]])
#   select[[i]] = data.frame(select[[1]])
#   
#   net_butt[[i]] <- table(
#     new$butterfly_species_cleaned,
#     new$nectar_species_cleaned
#   )
#   
#   net_df[[i]] <- as.matrix(net_butt[[i]])
#   
#   #drops empty rows/columns because not every combination happens every month
#   net_df[[i]] <- net_df[[i]][rowSums(net_df[[i]]) > 0, colSums(net_df[[i]]) > 0]
#   #Making sure there are enough observations
#    # if (is.null(net_df[[i]]) ||
#    #     nrow(net_df[[i]]) < 2 ||
#    #     ncol(net_df[[i]]) < 2) {
#    #     return(NULL)}
#   
#   # Compute network indices
#   ind[[i]] <- networklevel(net_df[[i]], index = c("connectance", "nestedness", "H2")
#   
# }
# 
# 
# 
# 
# results_ym <- lapply(year_months, function(y) {
#   
#   y<- year_months$year [y]
#   m<- year_months$month [y]
#   
#   # Filter for this year
#   use_butt <- g_sp_matrix |>
#     filter(butterfly_species_cleaned %in% highlight_butt$butterfly_species_cleaned) |>
#     filter(year == y) |> 
#     filter(month==m)
#   
#   # Create bipartite matrix
#   net_butt <- table(use_butt$butterfly_species_cleaned,
#                     use_butt$nectar_species_cleaned)
#   
#   net_df <- as.matrix(net_butt)
#   
#   #drops empty rows/columns because not every combination happens every month
#   net_df <- net_df[rowSums(net_df) > 0, colSums(net_df) > 0]
#   #Making sure there are enough observations
#   if (nrow(net_df) < 2 || ncol(net_df) < 2) return(NULL)
#   
#   # Compute network indices
#   ind <- networklevel(net_df, index = c("connectance", "nestedness", "H2"))
#   
#   # Return a row as a data frame
#   data.frame(
#     year = y,
#     month= m,
#     connectance = ind["connectance"],
#     nestedness = ind["nestedness"],
#     H2 = ind["H2"]
#   )
# })

network_by_climate <- do.call(rbind, results)

ggplot(network_by_climate, aes(x=max_temp, y= connectance))+
  geom_point()+
  geom_smooth(method=lm)

ggplot(network_by_climate, aes(x=max_temp, y= nestedness))+
  geom_point()+
  geom_smooth(method=lm)

ggplot(network_by_climate, aes(x=max_temp, y= H2))+
  geom_point()+
  geom_smooth(method=lm)

ggplot(network_by_climate, aes(x=precip, y= connectance))+
  geom_point()+
  geom_smooth(method=lm)+
  scale_x_continuous(limits = c(0,.4))

ggplot(network_by_climate, aes(x=precip, y= nestedness))+
  geom_point()+
  geom_smooth(method=lm)+
  scale_x_continuous(limits = c(0,.4))

ggplot(network_by_climate, aes(x=precip, y= H2))+
  geom_point()+
  geom_smooth(method=lm)+
  scale_x_continuous(limits = c(0,.4))


#climate in months

ym_clim<- monthly_climate |> 
  mutate(year_month=paste(year, month, sep = "."))
ym_clim$year_month= droplevels(ym$year.month)

#I lose one here, where does it go and which one is it? Does it matter?
network_clim<- inner_join(ym_clim, ind_df, by= "year_month")

network_clim |> 
  filter(month.x==7) |> 
  ggplot(aes(x=avg_temp, y= H2))+
  geom_point()+
  geom_smooth(method= "lm")
