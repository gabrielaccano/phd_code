library(stats)
library(mgcv)
#look at the timing of median date of butterfly-plant interaction
#weighted mean date--where is the mass of observations and standard deviation
# make regression for each year for each plant+butterfly interaction

regal<- matrix_98_24 |> 
  filter(butterfly_species_cleaned== "ARID") |> 
  filter(behavior==3) |> 
  filter(nectar_species_cleaned!= "NA")


wm_prep<- regal |> 
  group_by(year, julian, nectar_species_cleaned) |> 
  count(year, julian, nectar_species_cleaned, name="count") |> 
  summarize(sp_visits= sum(count), .groups= "drop_last") |> 
  filter(sp_visits>=4)

wm_std<-wm_prep |> 
  group_by(year, nectar_species_cleaned) |> 
  mutate(total_visits= sum(sp_visits), #total visits per year
         relative_abundance= sp_visits/ total_visits) |> # relative abundance
  summarize(wm_julian= sum(relative_abundance*julian),
            sd_julian= sd(julian),
            total_obs= sum(sp_visits),
            .groups= "drop") |> 
  filter(sd_julian!="NA")


ggplot(wm_std, aes(x=as.factor(year), y= wm_julian, color= nectar_species_cleaned, group= nectar_species_cleaned))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin= wm_julian-sd_julian, ymax= wm_julian+sd_julian)) #make that three blue lines 

?geom_point

visit_count<- regal |> 
  count(nectar_species_cleaned, julian, year) |> 
  rename(count=n)

interaction_summary <- visit_count %>%
  group_by(year, nectar_species_cleaned) %>%
  summarize(
    weighted_mean = weighted.mean(julian, w = count),
    weighted_sd = sqrt(weighted.mean((julian - weighted.mean(julian, w = count))^2, w = count)),
    total_obs = sum(count),
    .groups = "drop"
  )

filtered_data_counts <- visit_count %>%
  group_by(year,nectar_species_cleaned) %>%
  filter(n() >= 4, sum(count)>5) %>%  # or use sum(count) >= 5 to require at least 5 obs
  ungroup()

nested_models <- filtered_data_counts %>%
  group_by(year, nectar_species_cleaned) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(count ~ julian, data = .x)),
    model_summary = map(model, tidy),
    model_glance = map(model, glance)
  ) %>%
  unnest(model_summary, names_sep = "_")


slope_data <- nested_models %>%
  filter(model_summary_term == "julian") %>%
  mutate(
    is_significant = model_summary_p.value < 0.05,
    interaction = paste(nectar_species_cleaned, year, sep = "_")
  )

ggplot(slope_data, aes(x = reorder(interaction, model_summary_estimate), 
                       y = model_summary_estimate, 
                       color = is_significant)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "red")) +
  labs(
    title = "Slopes of count ~ Julian day per butterfly-plant-year interaction",
    x = "Interaction",
    y = "Slope (Julian day effect on count)",
    color = "Significant (p < 0.05)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())


#defining diapause-----------------------

obs_count<- regal |> 
  count(julian, year) |> 
  rename(count=n) |> 
  filter(count<50)

ggplot(obs_count, aes(x=julian, y=count, color= as.factor(year)))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~year)

#find the dips

dips<-obs_count |> 
  group_by(year) |> 
  arrange(julian) |> 
  summarise({loess_fit<-loess(count~julian, span= .2)
  smoothed<- predict(loess_fit)
  min<- which.min(smoothed)
  julian_dip<- julian[min]
  obs_dip<- smoothed[min]
  tibble(julian_dip= julian_dip, obs_dip=obs_dip)}) |> 
  ungroup()

ggplot(dips, aes(x = as.numeric(year), y = julian_dip)) +
  geom_point(size = 3) +
  geom_smooth() +
  theme_minimal()

#let's calculate a real dip

#50% decr in nectaring obs to count as diapause
dip_threshold<- .5

julian_seq<- 1:365

dip_analysis<- obs_count |> 
  group_by(year) |> 
  summarise({
    gam_fit<-gam(count~s(julian, bs="cs"))
    predictors<-predict(gam_fit, newdata=data.frame(julian=julian_seq))
    min<-which.min(predictors)
    max<-which.max(predictors)
    julian_dip<-julian_seq[min]
    obs_at_dip<-predictors[min]
    julian_peak<-julian_seq[max]
    obs_at_peak<-julian_seq[max]
    drop_ratio<-(obs_at_peak-obs_at_dip)/obs_at_peak
    diapause<-drop_ratio>=dip_threshold
    tibble(
      julian_peak=julian_peak,
      obs_at_peak= obs_at_peak,
      julian_dip= julian_dip,
      obs_at_dip= obs_at_dip,
      drop_ratio= drop_ratio,
      diapause= diapause
    )}) |> 
  ungroup()


ggplot(dip_analysis, aes(x = as.numeric(year), y = julian_dip, color = diapause)) +
  geom_point(size = 3) +
  geom_line(aes(group = 1)) +
  scale_color_manual(values = c("gray50", "firebrick"), labels = c("No Dip", "Diapause")) +
  labs(
    x = "Year",
    y = "Julian Day of Dip",
    color = "Diapause Detected",
    title = "Timing of Mid-Season Dips and Diapause Classification"
  ) +
  theme_minimal()

diapause_detected<- obs_count |> 
  filter(!year %in% c(2001, 2006, 2007, 2013, 2014, 2021))

detected_dips<-diapause_detected |> 
  group_by(year) |> 
  arrange(julian) |> 
  summarise({loess_fit<-loess(count~julian, span= .2)
  smoothed<- predict(loess_fit)
  min<- which.min(smoothed)
  julian_dip<- julian[min]
  obs_dip<- smoothed[min]
  tibble(julian_dip= julian_dip, obs_dip=obs_dip)}) |> 
  ungroup()

ggplot(diapause_detected, aes(x=julian, y=count, color= as.factor(year)))+
  geom_point()+
  geom_smooth(se=FALSE)

#measure length of diapause

diapause_time <- diapause_detected |> 
  group_by(year) |> 
  arrange(julian) |> 
  summarise({
    peak_day <- julian[which.max(count)]
    peak_val <- max(count, na.rm = TRUE)
    threshold <- 0.5 * peak_val
    
    after_peak <- filter(cur_data(), julian > peak_day)
    
    drop_day <- after_peak |> 
      filter(count < threshold) |> 
      slice_head(n = 1) |> 
      pull(julian)
    
    if (length(drop_day) == 0) {
      tibble(drop_day = NA, rise_day = NA, dip_length = NA)
    } else {
      after_drop <- filter(after_peak, julian > drop_day)
      
      rise_day <- after_drop |> 
        filter(count >= threshold) |> 
        slice_head(n = 1) |> 
        pull(julian)
      
      if (length(rise_day) == 0) {
        tibble(drop_day = drop_day, rise_day = NA, dip_length = NA)
      } else {
        dip_length <- rise_day - drop_day
        tibble(drop_day = drop_day, rise_day = rise_day, dip_length = dip_length)
      }
    }
  }) |> 
  ungroup()
