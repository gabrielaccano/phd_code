#:et's look at the climate data!

library (tidyverse)
library(readxl)
library(ggpubr)
library(emmeans)

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
    mean_temp = mean(((tmax+tmin)/2), na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(month= as.factor(month))

avg_clim<- summer_climate |> 
  mutate(mean_t=((tmax+tmin)/2)) |> 
  mutate(
    year  = year(date),
    month = as.factor(month(date))
  )

ggplot(avg_clim, aes(x=year, y=mean_t))+
  geom_smooth(method="lm")+
  facet_wrap(~month)

#staistical model for temperature
mod<- lm(mean_t ~ year + month + year:month, data = avg_clim)
summary(mod)
(car::Anova(mod, type = 3))

#posthoc: categorical vars
(emm.month = emmeans(mod, specs = pairwise ~ month, 
                    type = "response", adjust = "tukey"))

# posthoc plotting continuous vars
emmip(mod, month ~ year, cov.reduce = range, type = "response")

# posthoc continuous vars
(emm.monthyear = emtrends(mod, pairwise ~ month, var = "year", 
                          adjust = "tukey", type= "response") |> test(null=0))


#precipitiation
ggplot(avg_clim, aes(x=year, y=prcp))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~month)

all_clim<-climate |>
  mutate(year  = year(date),
         month = month(date))

spring<- all_clim |> 
  filter(month%in%c(3,4,5))
  

ggplot(spring, aes(x=year, y=prcp))+
  geom_smooth()+
  facet_wrap(~month)


#statistical model for precipitation
mod_p<- lm(prcp ~ year + month + year:month, data = spring)
summary(mod_p)
(car::Anova(mod_p, type = 3))



#each month individually, min temp----------
may<-grouped_climate |> 
  filter(month==5)

june<-grouped_climate |> 
  filter(month==6)

july<-grouped_climate |> 
  filter(month==7)

august<-grouped_climate |> 
  filter(month==8)

september<- grouped_climate |> 
  filter(month==9)

may_min<- ggplot(may, aes(x=year, y=mean_temp))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "May")

june_min<- ggplot(june, aes(x=year, y=mean_temp))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "June")

july_min<- ggplot(july, aes(x=year, y=mean_temp))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "July")

august_min<- ggplot(august, aes(x=year, y=mean_temp))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "August")

sep_min<- ggplot(september, aes(x=year, y=mean_temp))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "September")

ggarrange(june_min, july_min, august_min, sep_min)



#precipitation--------------------

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
    total_precip= sum(prcp, na.rm= TRUE),
    .groups = "drop"
  )

summer_precip<-precip_climate |> 
  filter(month %in% 5:9)

ggplot(summer_precip, aes(x = year, y = factor(month), fill = mean_precip)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal()

ggplot(summer_precip, aes(x= year, y=total_precip, color=as.factor(month)))+
  geom_point()+
  geom_smooth(method= lm, se= FALSE)


june<-summer_precip |> 
  filter(month==6)

july<-summer_precip |> 
  filter(month==7)

august<-summer_precip |> 
  filter(month==8)

september<- summer_precip |> 
  filter(month==9)


june_prcp<- ggplot(june, aes(x=year, y=mean_precip))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "June")

july_prcp<- ggplot(july, aes(x=year, y=mean_precip))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "July")

august_prcp<- ggplot(august, aes(x=year, y=mean_precip))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "August")

sep_prcp<- ggplot(september, aes(x=year, y=mean_precip))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "September")

ggarrange(june_prcp, july_prcp, august_prcp, sep_prcp)
