#:et's look at the climate data!

library (tidyverse)
library(readxl)
library(ggpubr)

climate<- read_csv("C:/Users/GCano/Documents/GitHub/phd_code/climate_data/weather data Harrisburg International.csv")

climate<- janitor::clean_names(climate)

#for the months of the pollard data
summer_climate<-climate |> 
  filter(month(date) %in% 5:9)

ggplot(summer_climate, aes(x=date, y=tmax))+
  geom_point()+
  geom_smooth(method=lm)

#by year/month combo
grouped_climate<- summer_climate |> 
  mutate(
    year  = year(date),
    month = month(date)
  ) |>
  group_by(year, month) |>
  summarise(
    min_temp  = min(tmin, na.rm = TRUE),
    max_temp  = max(tmax, na.rm = TRUE),
    mean_temp = mean(tavg, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(grouped_climate, aes(x=year, y=max_temp))+
  geom_point()+
  geom_smooth()

#each month individually
may<-grouped_climate |> 
  filter(month==5)

june<-grouped_climate |> 
  filter(month==6)

july<-grouped_climate |> 
  filter(month==7)

august<-grouped_climate |> 
  filter(month==8)

may_min<- ggplot(may, aes(x=year, y=min_temp))+
  geom_point()+
  geom_smooth(method=lm)

june_min<- ggplot(june, aes(x=year, y=min_temp))+
  geom_point()+
  geom_smooth(method=lm)

july_min<- ggplot(july, aes(x=year, y=min_temp))+
  geom_point()+
  geom_smooth(method=lm)

august_min<- ggplot(august, aes(x=year, y=min_temp))+
  geom_point()+
  geom_smooth(method=lm)

ggarrange(may_min, june_min, july_min, august_min)

#precipitation

precip_climate<- climate |> 
  mutate(
    year  = year(date),
    month = month(date),
    date = date
  ) |>
  group_by(year, month) |> 
  summarise(
    min_precip  = min(prcp, na.rm = TRUE),
    max_precip  = max(prcp, na.rm = TRUE),
    mean_precip = mean(prcp, na.rm = TRUE),
    .groups = "drop"
  )

summer_precip<-precip_climate |> 
  filter(month %in% 5:9)

ggplot(summer_precip, aes(x = year, y = factor(month), fill = mean_precip)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal()

ggplot(summer_precip, aes(x= year, y=mean_precip, color=as.factor(month)))+
  geom_point()+
  geom_smooth(method= lm, se= FALSE)
