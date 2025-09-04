
gsa <- matrix_98_24 |> 
  filter(butterfly_species_cleaned == "ARCA")

gsa_count<- gsa|> 
  group_by(year, julian) |> 
  summarise(count = n(), .groups = "drop")

ggplot(gsa_count, aes(x=julian, y=count))+
  geom_point()

ovi<- gsa |> 
  filter(behavior==6 | behavior== 8) |> 
  group_by(year, julian) |> 
  summarise(count = n(), .groups = "drop") |> 
  filter(julian>200)

ggplot(ovi, aes(x=julian))+
  geom_density()

peak_day <- ovi |> 
  filter(count == max(count, na.rm = TRUE))

ggplot(gsa, aes(x=julian))+
  geom_density()

mode_calc <- ft_species |>
  group_by(nectar_species, year) |>
  summarize(
    year_mode = Mode(julian),
    .groups = "drop"
  ) |> 
  group_by(nectar_species) |>
  summarize(
    average_mode = mean(year_mode, na.rm = TRUE),
    .groups = "drop"
  )

nectar_timing <- ft_species |>
  group_by(nectar_species) |>
  summarize(
    mean_julian = mean(julian),
    min_julian = min(julian),
    max_julian = max(julian),
    .groups = "drop"
  ) |> 
  left_join(mode_calc, by="nectar_species")

set.seed(31)
cluster<- kmeans(nectar_timing$average_mode, centers=2)