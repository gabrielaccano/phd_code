library(ggridges)
ggplot(regal_behavior_count, aes(x=Week, y=factor(Year), fill=nectar_species))+
  stat_density_ridges (geom= "density_ridges", scale= 1, alpha=.5,
                       trim=TRUE)+
  facet_wrap(~Sex)+
  scale_y_discrete(limits=rev)+
  theme_minimal()

ggplot(regal_behavior_count, aes(x = Julian, y = factor(Year), fill = nectar_species)) +
  geom_density_ridges(scale = 1, alpha = 0.5, adjust = 1) +  # Adjust controls smoothing
  facet_wrap(~Sex) +
  scale_y_discrete(limits = rev) +
  coord_cartesian(xlim = range(regal_behavior_count$Julian)) +  # Clips tails at observed limits
  theme_minimal()

?geom_density_ridges


ggplot(regal_behavior_count, aes(x=Julian, y=factor(Year), fill=nectar_species))+
  geom_density_ridges2(scale=1, alpha=.5)+
  facet_wrap(~Sex)+
  scale_y_discrete(limits=rev)+
  theme_minimal()


ggplot(regal_behavior_count, aes(x = Julian, y = factor(Year), fill = nectar_species)) +
  geom_density_ridges2(scale = 1, alpha = 0.5) +
  facet_wrap(~Sex) +
  scale_y_discrete(limits = rev) +
  coord_cartesian(xlim = c(min(regal_behavior_count$Julian), max(regal_behavior_count$Julian))) +  # Clip tails at observed limits
  theme_minimal()

ggplot(regal_behavior_count, aes(x = Julian, y = factor(Year), fill = nectar_species, height= ..density..)) +
  geom_density_ridges(stat= "density", scale = 1, alpha = 0.5, trim= TRUE) +
  facet_wrap(~Sex) +
  scale_y_discrete(limits = rev) +
  theme_minimal()

julian_limits <- regal_behavior_count %>%
  group_by(nectar_species, Year, Sex) %>%
  summarize(
    min_julian = min(Julian),  
    max_julian = max(Julian),  
    .groups = "drop"
  )

regal_behavior_trimmed <- regal_behavior_count %>%
  left_join(julian_limits, by = c("nectar_species", "Year", "Sex")) %>%
  filter(Julian >= min_julian & Julian <= max_julian)  # Trim tails
library(ggformula)

# ggplot(regal_behavior_trimmed, aes(x = Julian, y = factor(Year), fill = nectar_species)) +
  gf_dens(data= regal_behavior_count, ~nectar_species, scale = 1, alpha = 0.5, trim = TRUE, group="Sex") +  # `trim = TRUE` prevents artificial tails
  # facet_wrap(~Sex) +
  # scale_y_discrete(limits = rev) +
  # theme_minimal()

?geom_density_ridges2

  ggplot(regal_behavior_count, aes(x = Julian, y = factor(Year), color = nectar_species)) +
    geom_jitter(alpha = 0.5, width = 3, height = 0.1) +  # Adds jitter to avoid overplotting
    facet_wrap(~Sex) +
    scale_y_discrete(limits = rev) +
    theme_minimal()
  
  ggplot(regal_behavior_count, aes(x = factor(Year), y = Julian, fill = nectar_species)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Avoids plotting extreme outliers
    facet_wrap(~Sex) +
    theme_minimal() +
    coord_flip()
  
  ggplot(regal_behavior_count, aes(x = factor(Year), y = Julian, fill = nectar_species)) +
    geom_violin(alpha = 0.5, draw_quantiles = c(0.25, 0.5, 0.75)) +  # Adds median & quartiles
    facet_wrap(~Sex) +
    theme_minimal() +
    coord_flip()
  
  
  ggplot(regal_behavior_count, aes(x=Julian, y=factor(Year), fill=nectar_species))+
    geom_density_ridges2 (scale= 1, alpha=.7, rel_min_height= .1, stat="binline", bins = 20)+
    facet_wrap(~factor(Sex, levels = c("M", "F")))+
    scale_y_discrete(limits=rev)+
    labs(title= "Nectar plant usage over time by male and female Eastern Regal Fritillaries",
         x= "Julian Day", y="Year", fill= "Nectar Species")+
    scale_x_continuous(breaks= seq(min(regal_behavior_count$Julian), 
                                   max(regal_behavior_count$Julian), 
                                   by = 20))+
    scale_fill_manual(labels= c("APOCY"= "Dogbane", 
                                "ASSY"= "Common Milkweed", 
                                "ASTU"= "Butterfly Milkweed", 
                                "CENTA"= "Knapweed", 
                                "CIDI"= "Field Thistle", 
                                "CIPU4"= "Pasture Thistle", 
                                "MOFI2"= "Bee Balm"),
                     values= pals::stepped(20))+
    theme_minimal()
  
  
  
  summarize(regal_behavior_count$nectar_species)
  