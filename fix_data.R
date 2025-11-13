#Clean and organize FIG data
library(tidyverse)
library(NatParksPalettes)
library (readxl)
library(gdata)
library(openxlsx)
library(janitor)
library(fuzzyjoin)

setwd("C:/Users/GCano/Documents/GitHub/phd_code")

#Remove columns from 98-15-------------
#str(matrix_98_15)
matrix_98_15 <- read_xlsx("updated_matrix_gcc_10_25_25.xlsx", 
                          col_types= c("numeric",
                                       "numeric",
                                       "date",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "text",
                                       "text",
                                       "text",
                                       "text",
                                       "numeric",
                                       "text",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "numeric",
                                       "text",
                                       "text",
                                       "text",
                                       "text",
                                       "text", "text", "text", "text", "text", "text"))

matrix_98_15_standard<-matrix_98_15 |> 
  select(-Weeknt, -StartMixed, -EndMix, -Duration, -DurType) |>
   clean_names() |>
   rename(s_wind= wind_swind,
          s_percent_sun= percent_sun_ssun,
          e_percent_sun= esun,
          e_wind= ewind,
          s_temp= temp_stemp,
          e_temp= etemp,
          duration= durat_mix)

#Fix 2024 master to match 98-2015-----------
matrix_24<- read_excel(
  "butterfly_data_16_24/butterfly_24.xlsx", 
  col_types = c(
     "text", "date", "numeric", "text", "text",
     "date", "date", "date", "numeric", "numeric",
     "numeric", "numeric", "numeric", "numeric",
     "numeric", "numeric", "text", "text",
     "text", "text", "text", "text", "text", "text", "text"))

janitor::clean_names(matrix_24)

matrix_24_update<- matrix_24 |> 
  clean_names() |> 
  mutate(year= as.numeric(substr(j_date, 1,4)),
         julian= as.numeric(substr(j_date,5, 7)),
         date= as.Date(julian-1, origin=paste0(as.character(year), "-01-01"))) |> 
  mutate(month = month(date, label = FALSE)) |> 
  rename(sex= gender, start=s_time, end= e_time, nectar_species= nectar, butterfly_species= species) |> 
  select(year, month, date, week, julian, observer, site, start, end, duration,s_percent_sun, e_percent_sun, s_wind, e_wind, s_temp,
          e_temp, total, individual, section, sex, behavior, nectar_species, butterfly_species, comments)

#Fix 2023 master to match 98-2015--------------

matrix_23<- read_excel("butterfly_data_16_24/butterfly_23.xlsx")

janitor::clean_names(matrix_23)

matrix_23_update<- matrix_23 |> 
  clean_names() |> 
  mutate(year= as.numeric(substr(j_date, 1,4)),
         julian= as.numeric(substr(j_date,5, 7)),
         date= as.Date(julian-1, origin=paste0(as.character(year), "-01-01"))) |> 
  select(-x23, -x24, -x25) |> 
  mutate(month = month(date, label = FALSE)) |> 
  rename(sex= gender, start=s_time, end= e_time, nectar_species= nectar, butterfly_species= species) |> 
  select(year, month, date, week, julian, observer, site, start, end, duration,s_percent_sun, e_percent_sun, s_wind, e_wind, s_temp,
         e_temp, total, individual, section, sex, behavior, nectar_species, butterfly_species, comments)

#Fix 2022 master to match 98-2015------------
matrix_22<- read_excel("butterfly_data_16_24/butterfly_22.xlsx")

janitor::clean_names(matrix_22)

matrix_22_update<- matrix_22 |> 
  clean_names() |> 
  mutate(date = if_else(
    is.na(date),
    as.Date(j_date - 1, origin = paste0("2022-01-01")),
    date),
    month = month(date, label = FALSE),
    year= format(as.Date(date, format="%m%d%Y"),"%Y"))|> 
  rename(sex= gender, start=s_time, end= e_time, nectar_species= nectar, butterfly_species= species, julian= j_date) |> 
  select(year, month, date, week, julian, observer, site, start, end, duration,s_percent_sun, e_percent_sun, s_wind, e_wind, s_temp,
         e_temp, total, individual, section, sex, behavior, nectar_species, butterfly_species, comments)

#Fix 21 to match 98-15--------------
matrix_21<- read_csv("butterfly_data_16_24/butterfly_21.csv")

view(matrix_21 |> filter(species== "REGAL"))

janitor::clean_names(matrix_21)

matrix_21_update<- matrix_21 |> 
  clean_names()|> 
   mutate(year= as.numeric(substr(j_date, 1,4)),
          julian= as.numeric(substr(j_date,5, 7)),
          date= as.Date(julian-1, origin=paste0(as.character(year), "-01-01")))|>
   mutate(month = month(date, label = FALSE)) |> 
   rename(sex= gender, start=s_time, end= e_time, nectar_species= nectar_plant, butterfly_species= species) |> 
   select(year, month, date, week, julian, observer, site, start, end, duration,s_percent_sun, e_percent_sun, s_wind, e_wind, s_temp,
          e_temp, total, individual, section, sex, behavior, nectar_species, butterfly_species) |> 
  add_column(comments= NA)

view(matrix_21_update |>  filter(butterfly_species== "REGAL"))

#Fix 20 to match 98-2015 -----------------
matrix_20<- read_excel("butterfly_data_16_24/butterfly_20.xlsx",
                       col_types = c(
                         "logical",  # First column: Logical (TRUE/FALSE)
                       "date",     # Second column: Date (POSIXct can be interpreted as date)
                       "logical",  # Third column: Logical (TRUE/FALSE)
                       "text",     # Fourth column: Character (string)
                       "text",     # Fifth column: Character (string)
                       "numeric",  # Sixth column: Numeric
                       "numeric",  # Seventh column: Numeric
                       "logical",  # Eighth column: Logical (TRUE/FALSE)
                       "numeric",  # Ninth column: Numeric
                       "numeric",  # Tenth column: Numeric
                       "numeric",  # Eleventh column: Numeric
                       "numeric",  # Twelfth column: Numeric
                       "logical",  # Thirteenth column: Logical (TRUE/FALSE)
                       "logical",  # Fourteenth column: Logical (TRUE/FALSE)
                       "logical",  # Fifteenth column: Logical (TRUE/FALSE)
                       "logical",  # Sixteenth column: Logical (TRUE/FALSE)
                       "text",     # Seventeenth column: Character (string)
                       "text",     # Eighteenth column: Character (string)
                       "text",     # Nineteenth column: Character (string)
                       "text",     # Twentieth column: Character (string)
                       "text",     # Twenty-first column: Character (string)
                       "text"      # Twenty-second column: Character (string)
))
janitor::clean_names(matrix_20)

matrix_20$comments <- as.character(matrix_20$comments)
class(matrix_20$comments)

matrix_20_update<- matrix_20 |> 
  clean_names()|> 
  filter_all(any_vars(!is.na(.)))|> 
  mutate(julian= as.Date(date, format= "%y%m%d"),
         julian= format(julian, "%j"),
         year= format(as.Date(date, format="%Y%m%d"),"%Y"),
         month = month(date, label = FALSE),
         duration=(e_time-s_time),
         behavior = case_when(
           nectar_plant %in% c(3, 9, 2, 5) ~ as.character(nectar_plant),  # Assign nectar_plant values to behavior
           TRUE ~ as.character(behavior)  # Keep existing behavior for other values
         ),
         
         # Set nectar_plant to NA for values 3, 9, 2, 5
         nectar_plant = if_else(nectar_plant %in% c(3, 9, 2, 5), NA, nectar_plant),
         nectar_plant = if_else(species == "GR", "GR", nectar_plant)) |> 
  rename(sex= gender, start=s_time, end= e_time, nectar_species= nectar_plant, butterfly_species= species) |> 
  select(year, month, date, week, julian, observer, site, start, end, duration,s_percent_sun, e_percent_sun, s_wind, e_wind, s_temp,
         e_temp, total, individual, section, sex, behavior, nectar_species, butterfly_species, comments)
  

#Fix 19 to match 98-15------------------
matrix_19<- read_excel("butterfly_data_16_24/butterfly_19.xlsx")

janitor::clean_names(matrix_19)

matrix_19_update<- matrix_19 |> 
  clean_names()|> 
  mutate(month = month(date, label = FALSE),
         year= format(as.Date(date, format="%Y-%m-%d"),"%Y")) |> 
  rename(sex= gender, start=s_time, end= e_time, nectar_species= nectar_plant, butterfly_species= species, julian= j_date) |> 
  select(year, month, date, week, julian, observer, site, start, end, duration,s_percent_sun, e_percent_sun, s_wind, e_wind, s_temp,
         e_temp, total, individual, section, sex, behavior, nectar_species, butterfly_species, comments)
#Fix 18 to match 98-15-----------------------
matrix_18<- read_excel("butterfly_data_16_24/butterfly_18.xlsx",
                       col_types = c(
                         "text", "date", "numeric", "text", "text",
                         "date", "date", "date", "numeric", "numeric",
                         "numeric", "numeric", "numeric", "numeric",
                         "numeric", "numeric", "text", "text",
                         "text", "text", "text", "text"))

janitor::clean_names(matrix_18)

matrix_18_update<- matrix_18 |> 
  clean_names()|> 
  mutate(month = month(date, label = FALSE),
         year= format(as.Date(matrix_18$date, format="%m%d%Y"),"%Y"))|> 
  rename(sex= gender, start=s_time, end= e_time, nectar_species= nectar_plant, butterfly_species= species, julian= j_date) |> 
  select(year, month, date, week, julian, observer, site, start, end, duration,s_percent_sun, e_percent_sun, s_wind, e_wind, s_temp,
         e_temp, total, individual, section, sex, behavior, nectar_species, butterfly_species, comments)

#Fix 17 to match 98-15-----------------
matrix_17<- read_excel("butterfly_data_16_24/butterfly_17.xlsx")

view(matrix_17)

janitor::clean_names(matrix_17)

matrix_17_update<- matrix_17 |> 
  clean_names()|> 
  mutate(year= "2017",
         full_date= as.Date(date, origin=as.Date("2017-01-01")),
         month = format(full_date, "%m")) |> 
  rename(sex= gender, start=s_time, end= e_time, nectar_species= nectar_plant, butterfly_species= species, julian= date, date=full_date) |> 
  select(year, month, date, week, julian, observer, site, start, end, duration,s_percent_sun, e_percent_sun, s_wind, e_wind, s_temp,
         e_temp, total, individual, section, sex, behavior, nectar_species, butterfly_species, comments)

#Fix 16 to match 98-15-------------------
matrix_16<- read_excel("butterfly_data_16_24/butterfly_16.xlsx")

janitor::clean_names(matrix_16)

matrix_16_update<- matrix_16 |> 
  clean_names()|> 
  mutate(julian= as.Date(date, format= "%y%m%d"),
julian= format(julian, "%j"),
year= format(as.Date(matrix_16$date, format="%d%m%y"),"%Y"),
month = month(date, label = FALSE)) |> 
  rename(sex= gender, start=s_time, end= e_time, nectar_species= nectar_plant, butterfly_species= species) |> 
  select(year, month, date, week, julian, observer, site, start, end, duration,s_percent_sun, e_percent_sun, s_wind, e_wind, s_temp,
         e_temp, total, individual, section, sex, behavior, nectar_species, butterfly_species, comments)

#merge all new dfs into one to fix naming ---------------

#Import -15 data and plant/butterfly code
original_matrix<- read_excel("updated_matrix_gcc_03_11_25.xlsx", sheet = "mutated_matrix")
updated_b<- read_excel("updated_matrix_gcc_03_11_25.xlsx", sheet= "butterfly_codes")
updated_p<- read_excel("updated_matrix_gcc_03_11_25.xlsx", sheet= "plant_codes")

#Combined matrix
merged_matrix_16_24<- rbind(matrix_16_update, 
                      matrix_17_update,
                      matrix_18_update,
                      matrix_19_update,
                      matrix_20_update,
                      matrix_21_update,
                      matrix_22_update,
                      matrix_23_update,
                      matrix_24_update)

view(merged_matrix_16_24 |> filter(year== 2021, butterfly_species== "REGAL"))

#Fixing minor dataset-wide problems
merged_matrix_16_24_clean <- merged_matrix_16_24 |>
  mutate(year= as.numeric(year)) |> 
  mutate(year = case_when(
    year == 2108 ~ 2018, 
    year == 1900~ 2019,
    TRUE ~ year)) |> 
  filter(!if_all(everything(), is.na)) |> 
  ##LOOK TO SEE IF THIS IS GETTING RID OF NONES
  #filter(!(is.na(nectar_species) & is.na(butterfly_species))) |> 
  mutate(start = format(start, "%H:%M"),
         end = format(end, "%H:%M"),
        duration = format(duration, "%H:%M"),
        duration = as.numeric(substr(duration, 1, 2)) * 60 + as.numeric(substr(duration, 4, 5)),
          date= as.Date(date, format= "%Y%m%d")) |> 
  mutate(nectar_species_cleaned= nectar_species,
         butterfly_species_cleaned= butterfly_species)

#Export as compiled 
write.xlsx(merged_matrix_16_24_clean,"C:/Users/GCano/Documents/GitHub/phd_code/butterfly_data_16_24/combined_16_24_10_25_25.xlsx", 
           rowNames = FALSE)

to_combine<-merged_matrix_16_24_clean

to_combine<- read_xlsx("butterfly_data_16_24/combined_16_24_10_25_25.xlsx",
                       col_types= c("numeric",
                       "numeric",
                       "date",
                       "numeric",
                       "numeric",
                       "text",
                       "text",
                       "text",
                       "text",
                       "text",
                       "numeric",
                       "numeric",
                       "numeric",
                       "numeric",
                       "numeric",
                       "numeric",
                       "numeric",
                       "text",
                       "text",
                       "text",
                       "numeric",
                       "text",
                       "text",
                       "text",
                       "text",
                       "text"))

seeing_diff_butt<-anti_join(to_combine, updated_b, by= "butterfly_species")
diff_plant<- anti_join(to_combine, updated_p, by= "nectar_species")

view(seeing_diff_butt |> distinct(butterfly_species))

view(diff_plant |> 
       group_by(observer, date, nectar_species, site) |> 
       distinct(nectar_species) |> 
  filter(nectar_species!= "NA"))


# Create a named vector for plant_name -> plant_code mapping
plant_code_map <- setNames(updated_p$plant_code, updated_p$nectar_species)

# Replace nectar_species in original_matrix with corresponding plant_code
merged_matrix_16_24_clean$nectar_species_cleaned <- sapply(merged_matrix_16_24_clean$nectar_species_cleaned,
                                        function(x) plant_code_map[x])

# Create a named vector for plant_name -> plant_code mapping
butterfly_code_map <- setNames(updated_b$butterfly_code, updated_b$butterfly_species)

# Replace nectar_species in original_matrix with corresponding plant_code
merged_matrix_16_24_clean$butterfly_species_cleaned <- sapply(merged_matrix_16_24_clean$butterfly_species_cleaned,
                                           function(x) butterfly_code_map[x])

#Fix sex
colnames(to_combine)

view(merged_matrix_16_24_clean)

view(to_combine |> distinct(sex))

to_combine_sex<- merged_matrix_16_24_clean |> 
  mutate( sex = case_when(
    sex == "FEMALE"~ "F",
    sex == "MALE" ~ "M",
    sex == "Male" ~ "M",
    sex== "UNK" ~ NA_character_,
    sex== "UNKNOWN" ~ NA_character_,
    TRUE ~ sex))

rg<-to_combine_sex |> 
  filter(butterfly_species_cleaned=="ARID")


view(rg)

#MAKE ULTRA MEGA SUPER DUPER DATASET!
matrix_98_24<- rbind(matrix_98_15_standard,
                     to_combine_sex)

view(matrix_98_24 |> filter ((is.na(nectar_species) & is.na(butterfly_species) & year>=2005)))


write.xlsx(matrix_98_24,"C:/Users/GCano/Documents/GitHub/phd_code/butterfly_data_16_24/gcc_complete_pollard_10_25_25.xlsx", 
           rowNames = FALSE)

matrix_98_24<- read_xlsx("butterfly_data_16_24/gcc_full_matrix_04_29_25.xlsx",
                         col_types=c ("numeric",
                                      "numeric",
                                      "date",
                                      "numeric",
                                      "numeric",
                                      "text",
                                      "text",
                                      "text",
                                      "text",
                                      "text",
                                      "numeric",
                                      "numeric",
                                      "numeric",
                                      "numeric",
                                      "numeric",
                                      "numeric",
                                      "numeric",
                                      "text",
                                      "text",
                                      "text",
                                      "numeric",
                                      "text",
                                      "text",
                                      "text"
                  
                                      ))

str(matrix_98_24)
is_missing <- function(x) {
  is.na(x) | x == "" | x == "NA" | x== "NONE" | x== "None"
}

matrix_98_24_cleaned <- matrix_98_24 |> 
  mutate(
    new_comments = case_when(
      !is_missing(butterfly_species) & is_missing(butterfly_species_cleaned) &
        !is_missing(nectar_species) & is_missing(nectar_species_cleaned) ~
        paste0("butterfly species = ", butterfly_species, "; nectar species = ", nectar_species),
      
      !is_missing(butterfly_species) & is_missing(butterfly_species_cleaned) ~
        paste0("butterfly species = ", butterfly_species),
      
      !is_missing(nectar_species) & is_missing(nectar_species_cleaned) ~
        paste0("nectar species = ", nectar_species),
      
      TRUE ~ NA_character_
    ),
    comments = case_when(
      !is.na(new_comments) & (is.na(comments) | comments == "") ~ new_comments,
      !is.na(new_comments) ~ paste(comments, new_comments, sep = " | "),
      TRUE ~ comments
    )
  ) |>
  select(-new_comments) |>
  mutate(
    date = if_else(
      year(date) == 2108,
      make_date(year, month(date), day(date)),
      date
    ),
    week = isoweek(date)
  ) |> 
  mutate(sex = case_when(
    sex %in% c("M", "F") ~ sex,
    TRUE ~ NA_character_))

view(matrix_98_24_cleaned |> 
       distinct(sex))

view(matrix_98_24_cleaned |>
       group_by(date, observer, year) |> 
       filter(comments!= "NA") |> 
       distinct(comments))

matrix_98_24_thistle <- matrix_98_24_cleaned |> 
  mutate(butterfly_species_cleaned = ifelse(year >= 1998 & year <= 2004, "ARID", butterfly_species_cleaned)) |> 
  mutate(nectar_species_cleaned = case_when(
    year >= 1998 & year <= 2003 & julian > 220 & nectar_species_cleaned == "CIPU4" ~ "CIDI",
    year == 2013 & julian < 160 & nectar_species_cleaned == "CIDI" ~ "CIPU4",   # new rule
    TRUE ~ nectar_species_cleaned
  )) |> 
  mutate(comments = case_when(
    year >= 1998 & year <= 2003 & julian > 220 & nectar_species_cleaned == "CIDI" ~ 
      "Changed from CIPU4 to CIDI on 03/07/25 by GCC with approval from FIG",
    year == 2013 & julian < 160 & nectar_species_cleaned == "CIPU4" ~ 
      "Changed from CIDI to CIPU4 on 09/02/25 by GCC",   # new comment
    TRUE ~ ifelse(is.na(comments), "", comments)  # keep existing comments non-NA
  ))

str(matrix_98_24_thistle)

view(matrix_98_24_thistle |>  filter(!is.na(comments) & comments != ""))

matrix_98_24_remove<- matrix_98_24 |> 
  #select(-nectar_species, -butterfly_species) |> 
  select(year, month, date, week, julian, observer, segment_orig, segment, site_orig, field, route, start, end, duration,s_percent_sun, e_percent_sun, s_wind, e_wind, s_temp,
         e_temp, total, individual, sex, behavior, butterfly_species, nectar_species, nectar_species_cleaned, butterfly_species_cleaned, comments)


write.xlsx(matrix_98_24_thistle,"C:/Users/GCano/Documents/GitHub/phd_code/butterfly_data_16_24/gcc_complete_pollard_10_25_25.xlsx", 
           rowNames = FALSE)

new_matrix<-read_xlsx("butterfly_data_16_24/gcc_complete_pollard_10_09_25.xlsx",
                      col_types = c("numeric",
                      "numeric",
                      "date",
                      "numeric",
                      "numeric",
                      "text",
                      "text",
                      "text",
                      "text",
                      "text",
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric",
                      "text",
                      "text",
                      "text",
                      "numeric",
                      "text",
                      "text",
                      "text"))

fix_bob<- matrix_98_24_thistle|> 
  mutate(
    butterfly_species_cleaned = case_when(
      str_detect(comments, "butterfly species = (RBHS|RBSH\\?|RBH)") ~ "CACE",
      str_detect(comments, "butterfly species = BOB") ~ "CEPE",
      TRUE ~ butterfly_species_cleaned
    ),
    comments = if_else(
      str_detect(comments, "butterfly species = (RBHS|RBSH\\?|RBH|BOB)"),
      NA_character_,
      comments
    )
  )

view(fix_bob |> 
       filter(butterfly_species== "BOB"))

write.xlsx(fix_bob,"C:/Users/GCano/Documents/GitHub/phd_code/butterfly_data_16_24/gcc_complete_pollard_10_25_25.xlsx", 
           rowNames = FALSE)

view(matrix_98_24 |> 
       filter(butterfly_species=="ARID",
              year== "2024"))



new_rg<- matrix_98_24 |> 
  select(sex, butterfly_species, year, behavior) |> 
  filter(butterfly_species=="ARID") |> 
  filter(behavior==3) |> 
  filter(sex=="M"| sex=="F")

view(new_rg)


find_r_files_by_date <- function(base_dir, date = "2025-02-06") {
  files <- list.files(base_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  info <- file.info(files)
  
  # Define a small date window (±1 day)
  start_date <- as.POSIXct(paste0(date, " 00:00:00"))
  end_date <- as.POSIXct(paste0(date, " 23:59:59"))
  
  recent <- subset(info, mtime >= start_date & mtime <= end_date)
  recent[order(recent$mtime, decreasing = TRUE), , drop = FALSE]
}

# Example:
find_r_files_by_date("C:/Users/GCano")
