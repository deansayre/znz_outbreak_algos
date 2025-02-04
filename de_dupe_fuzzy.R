# will require some modification with actual column names and filepath (as well 
# as other potential changes; is not tested )


install.packages("pacman")

pacman::p_load(stringdist,
               rio, 
               devtools, 
               tidyverse)

devtools::install_github("reconhub/linelist")

data <- rio::import("file_path") %>% 
  linelist::clean_data(guess_dates = FALSE) %>% 
  mutate(id = row_number()) %>% 
  dplyr::select(## add columns to keep here... name, age, facility, shehia, date ##) %>% 
  full_join(., ., by = character()) %>% 
  filter(id.x < id.y) %>% 
  mutate(
    name.dist = stringdist(name.x, name.y), 
    shehia_same = shehia.x == shehia.y, 
    facility_same = facility.x == facility.y) %>% 
  arrange(name.dist) %>% 
  relocate(
    id.x, id.y,
    name.dist, 
    name.x, name.y, 
    date.x, date.y, 
    shehia_same, facility_same
  )

