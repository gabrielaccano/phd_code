library(tidyverse)

plant_table<- read_xlsx("C:/Users/GCano/OneDrive - Temple University/plant_code_sheet.xlsx")

plant_table_filter<- plant_table |> 
  distinct(plant_genus, plant_sp, plant_code, .keep_all = TRUE) |> 
  mutate(scientific_name = paste(plant_genus, plant_sp, sep = " ")) |> 
  select(scientific_name, plant_code)

write.xlsx(plant_table_filter, "C:/Users/GCano/Documents/GitHub/phd_code/complete_plant_list_gcc_2025.xlsx")        
