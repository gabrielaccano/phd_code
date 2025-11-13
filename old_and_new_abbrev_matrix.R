library(tidyverse)

setwd("C:/Users/GCano/Documents/GitHub/phd_code/")
original_matrix<- read_excel("Matrix_nt_2016-04-28 - Copy.xlsx", sheet = "Matrix")
# updated_p_and_b<- read_excel("Matrix_nt_2016-04-28 - Copy.xlsx", sheet= "updated_p_and_b")

original_matrix_cleaned<-original_matrix |> 
  mutate(nectar_species_cleaned= nectar_species,
         butterfly_species_cleaned= butterfly_species,
         Start = format(Start, "%H:%M"),
         End = format(End, "%H:%M"),
         DuratMix = format(DuratMix, "%H:%M"),
         DuratMix = as.numeric(substr(DuratMix, 1, 2)) * 60 + as.numeric(substr(DuratMix, 4, 5)))

original_matrix<- read_excel("updated_matrix_gcc_03_11_25.xlsx", sheet = "mutated_matrix")
updated_b<- read_excel("updated_matrix_gcc_03_11_25.xlsx", sheet= "butterfly_codes")
updated_p<- read_excel("updated_matrix_gcc_03_11_25.xlsx", sheet= "plant_codes")
#fix dataframe naming -----------------

updated_matrix<-original_matrix_cleaned

# Create a named vector for plant_name -> plant_code mapping
plant_code_map <- setNames(updated_p$plant_code, updated_p$nectar_species)

# Replace nectar_species in original_matrix with corresponding plant_code
updated_matrix$nectar_species_cleaned <- sapply(updated_matrix$nectar_species_cleaned,
                                                function(x) plant_code_map[x])

# Create a named vector for plant_name -> plant_code mapping
butterfly_code_map <- setNames(updated_b$butterfly_code, updated_b$butterfly_species)

# Replace nectar_species in original_matrix with corresponding plant_code
updated_matrix$butterfly_species_cleaned <- sapply(updated_matrix$butterfly_species_cleaned,
                                                   function(x) butterfly_code_map[x])

updated_matrix<- updated_matrix |> 
  mutate(butterfly_species_cleaned = if_else(
    is.na(butterfly_species_cleaned) & Year >= 1998 & Year <= 2004 & Individual!=0,
    "ARID",
    butterfly_species_cleaned))

view(updated_matrix |>filter(Individual==0| is.na(Individual)))

write.csv(updated_matrix,"C:/Users/GCano/Documents/GitHub/phd_code/pre_thistle_matrix_gcc_10_25_25.csv", row.names = FALSE)

mutated_matrix<- updated_matrix |> 
  #mutate(butterfly_species= ifelse(Year>= 1998 & Year<=2004, "ARID", butterfly_species)) |> 
  mutate(nectar_species = case_when(
    Year >= 1998 & Year <= 2003 & Julian> 220 & nectar_species == "CIPU4" ~ "CIDI",
    TRUE ~ nectar_species))|> 
  mutate(Comments = case_when(
    Year >= 1998 & Year <= 2003 & Julian > 220 & nectar_species == "CIDI" ~ 
      "Changed from CIPU4 to CIDI on 03/07/25 by GCC with approval from FIG",
    TRUE ~ ifelse(is.na(Comments), "", Comments)  # Ensure comment is not NA
  ))

#Assign weeks based on Julian Day
week_matrix<- mutated_matrix |> 
  mutate (Week=ceiling((Julian)/7))

view(week_matrix |>  filter(Total==0))

write.xlsx(week_matrix,"C:/Users/GCano/Documents/GitHub/phd_code/updated_matrix_gcc_10_25_25.xlsx", row.names = FALSE)




list_recent_r_files <- function(base_dir, since = "2025-02-01") {
  # Include .R, .Rmd, .qmd, and .Rnw files
  files <- list.files(
    base_dir,
    pattern = "\\.(R|Rmd|qmd|Rnw)$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  # Get file metadata
  info <- file.info(files)
  
  # Filter to files modified since the given date
  recent <- subset(info, mtime >= as.POSIXct(since))
  
  # Sort newest first
  recent <- recent[order(recent$mtime, decreasing = TRUE), ]
  
  return(rownames(recent))
}

# Example usage:
list_recent_r_files("C:/Users/GCano/Documents/Github")
