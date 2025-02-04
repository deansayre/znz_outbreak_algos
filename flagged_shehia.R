
rm(list = ls())

pacman::p_load(tidyverse)

pacman::p_load_gh("reconhub/linelist")


dec9_data <- rio::import("data_humphrey_dec9_24.xlsx", which = 1) %>% 
  Rtesunate::act_clean() %>% 
  mutate(district_for_shehia = str_replace_all(district, "\\.", "_"))

names(dec9_data)

dec9_data1 <- dec9_data %>% 
  mutate(year_week = tsibble::yearweek(date_and_time_of_positive_results), 
         year_week_char = as.numeric(word(as.character(year_week), 2, 
                                          sep = "W")), 
         iso_week = lubridate::isoweek(date_and_time_of_positive_results), 
         tsibble_okay = iso_week == year_week_char, 
         excel_okay = iso_week == week)
  

date_issues <- dec9_data1 %>% 
  filter(tsibble_okay == FALSE | excel_okay == FALSE)

date_issues_dean <- dec9_data1 %>% 
  filter(tsibble_okay == FALSE)

filtered_shehia_alerts <- rio::import("filtered_shehia_alert_alpha_1.xlsx") %>% 
  select(-all_of(starts_with("population"))) %>% 
  select(-all_of(starts_with("state")))%>% 
  select(-all_of(starts_with("alarm"))) %>% 
  pivot_longer(-epoch) %>% 
  mutate(type = word(name, 1, sep = "\\."), 
         district_shehia = word(name, 2, sep = "\\.")) %>% 
  select(-name) %>% 
  left_join(., ., by = c("epoch", "district_shehia")) %>% 
  filter(type.x == "observed" & type.y == "upperbound") %>% 
  mutate(alert = value.x > value.y, 
         outbreak_mag = case_when(alert == TRUE ~ value.x - value.y, 
                                  .default = NA_real_)) %>% 
  rename(observed = value.x, 
         threshold = value.y, 
         week = epoch) %>% 
  select(-c(type.x, type.y)) %>% 
  arrange(district_shehia, week) %>% 
  group_by(district_shehia) %>% 
  mutate(geo_week = row_number(), 
         two_or_more = case_when((lag(alert) == TRUE & alert == TRUE) |
                                   (lead(alert) == TRUE & alert == TRUE) ~ TRUE, 
                                 .default = FALSE),
         absolute_mag = case_when(outbreak_mag > 4 ~ TRUE, 
                                  outbreak_mag <= 4 ~ FALSE, 
                                  .default = NA), 
         relative_mag = case_when(alert == FALSE ~ NA,
                                  observed/threshold >= 3 ~ TRUE, 
                                  observed/threshold < 3 ~ FALSE, 
                                  .default = NA), 
         defcon1 = case_when(alert == FALSE ~ "grey", 
                             alert == TRUE & (absolute_mag == TRUE | relative_mag == TRUE) & two_or_more == TRUE ~ "firebrick4", 
                             .default = "grey"),
         first_in_run = case_when(alert == TRUE & lag(alert) == FALSE ~ week, 
                                  .default = NA)) %>% 
  fill(first_in_run, .direction = "down") %>% 
  ungroup() %>% 
  mutate(first_in_run = ifelse(alert == TRUE, first_in_run, NA)) %>% 
  group_by(district_shehia, first_in_run) %>% 
  mutate(any_defcon = ifelse(alert == TRUE, list(defcon1), NA)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(ever_exceeded = "firebrick4" %in% any_defcon, 
         outbreak = case_when(ever_exceeded == TRUE ~ "firebrick4", 
                             .default = "grey")) %>% 
  ungroup()%>% 
  mutate(district = word(district_shehia, 1, sep = "-"), 
         shehia = word(district_shehia, 2, sep = "-"))




district_alerts <- rio::import("filtered_district_alert_alpha_1.xlsx") %>% 
  select(-all_of(starts_with("population"))) %>% 
  select(-all_of(starts_with("state")))%>% 
  select(-all_of(starts_with("alarm"))) %>% 
  pivot_longer(-epoch) %>% 
  mutate(type = word(name, 1, sep = "\\."), 
         district = word(name, 2, sep = "\\.")) %>% 
  select(-name) %>% 
  left_join(., ., by = c("epoch", "district")) %>% 
  filter(type.x == "observed" & type.y == "upperbound") %>% 
  mutate(alert = value.x > value.y, 
         outbreak_mag = case_when(alert == TRUE ~ value.x - value.y, 
                                  .default = NA_real_)) %>% 
  rename(observed = value.x, 
         threshold = value.y, 
         week = epoch) %>% 
  select(-c(type.x, type.y)) %>% 
  arrange(district, week) %>% 
  group_by(district) %>% 
  mutate(geo_week = row_number(), 
         two_or_more = case_when((lag(alert) == TRUE & alert == TRUE) |
                                   (lead(alert) == TRUE & alert == TRUE) ~ TRUE, 
                                 .default = FALSE),
         absolute_mag = case_when(outbreak_mag > 4 ~ TRUE, 
                                  outbreak_mag <= 4 ~ FALSE, 
                                  .default = NA), 
         relative_mag = case_when(alert == FALSE ~ NA,
                                  observed/threshold >= 3 ~ TRUE, 
                                  observed/threshold < 3 ~ FALSE, 
                                  .default = NA), 
         defcon1 = case_when(alert == FALSE ~ "grey", 
                             alert == TRUE & (absolute_mag == TRUE | relative_mag == TRUE) & two_or_more == TRUE ~ "firebrick4", 
                             .default = "grey"),
         first_in_run = case_when(alert == TRUE & lag(alert) == FALSE ~ week, 
                                  .default = NA)) %>% 
  fill(first_in_run, .direction = "down") %>% 
  ungroup() %>% 
  mutate(first_in_run = ifelse(alert == TRUE, first_in_run, NA)) %>% 
  group_by(district, first_in_run) %>% 
  mutate(any_defcon = ifelse(alert == TRUE, list(defcon1), NA)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(ever_exceeded = "firebrick4" %in% any_defcon, 
         outbreak = case_when(ever_exceeded == TRUE ~ "firebrick4", 
                             .default = "grey")) %>% 
  ungroup()

flagged <- rio::import("shehia_flagged.xlsx") %>% 
  Rtesunate::act_clean() %>% 
  mutate(district = str_replace_all(district, "\\.", "_"), 
         week_char = case_when(as.numeric(week) < 10 ~ paste0("0", as.character(week)), 
                               .default = as.character(week)),
         district_shehia = paste0(district, "-", shehia))


flagged_week_start <- function(y =1){
  district_alerts1 <- district_alerts %>% 
    mutate(year_week = tsibble::yearweek(week, week_start = y))
  
  filtered_shehia_alerts1 <- filtered_shehia_alerts%>% 
    mutate(year_week = tsibble::yearweek(as.Date(as.character(week)), 
                                         week_start = y))
  
  out <- flagged %>% 
    mutate(year_week = tsibble::yearweek(paste0(as.character(year), " W", 
                                                week_char), 
                                         week_start = y)) %>% 
    left_join(select(district_alerts1, c("year_week", "district", "outbreak")), 
                   by = c("district", "year_week")) %>% 
    filter(year_week <= tsibble::yearweek("2024 W38", week_start = y)) %>% 
    left_join(select(filtered_shehia_alerts1, 
                     c("district_shehia", "year_week", "outbreak")), 
              by = c("district_shehia", "year_week")) %>% 
    rename(outbreak_district_flagged = outbreak.x, 
           outbreak_shehia_flagged = outbreak.y)
}


flagged1 <- flagged_week_start()




flagged1 <- flagged %>% 
  mutate(week_date = as.Date(tsibble::yearweek(paste0(as.character(year), " W", 
                                              week_char), 
                                       week_start = 1)))

flagged_district <- flagged1 %>% 
  group_by(district, week_date) %>% 
  summarise(count = n())
  


district_alerts_sub <- district_alerts %>% 
  left_join(flagged_district, by = c("week" = "week_date", 
                             "district")) %>% 
  drop_na(count)


shehia_alerts_sub <- filtered_shehia_alerts %>% 
  left_join(flagged1, by = c("week" = "week_date", 
                             "district_shehia")) %>% 
  drop_na(shehia.y)


alarm_district <- map(c(sort(unique(district_alerts$district))), 
                      ~district_alerts %>% 
                        filter(district == .x)) %>% 
  set_names(c(sort(unique(district_alerts$district))))



alarm_district_sub <- map(c(sort(unique(district_alerts_sub$district))), 
                      ~district_alerts_sub %>% 
                        filter(district == .x)) %>% 
  set_names(c(sort(unique(district_alerts_sub$district))))


test1 <- district_alerts_sub %>% 
  filter(district == "chakechake")


alarm_district1 <- alarm_district[names(alarm_district_sub)]




alarm_shehia <- map(c(sort(unique(filtered_shehia_alerts$district))), 
                    ~filtered_shehia_alerts %>% 
                      filter(district == .x)) %>% 
  set_names(c(sort(unique(filtered_shehia_alerts$district))))


alarm_shehia_sub <- map(c(sort(unique(shehia_alerts_sub$district.x))), 
                    ~shehia_alerts_sub %>% 
                      filter(district.x == .x)) %>% 
  set_names(c(sort(unique(shehia_alerts_sub$district.x))))


alarm_shehia1 <- alarm_shehia[names(alarm_shehia_sub)]



b <- pmap(list(alarm_district1, 
               alarm_shehia1,
               sort(unique(shehia_alerts_sub$district.x)), 
               alarm_district_sub, 
               alarm_shehia_sub),
          # sort(unique(filtered_shehia_alerts1$district_n))), 
          function(a,b,c,d,e){col_alpha <- a %>% 
            as_tibble()  
          
          col_vivid <- d %>% 
            as_tibble() 
          
          top_panel <- ggplot()+
            geom_col(data = col_alpha,
                     aes(x = week, y = observed, fill = outbreak), 
                     alpha = 0.2)+
            geom_col(data = col_vivid,
                     aes(x = week, y = observed, fill = outbreak), 
                     alpha = 0.2)+
            scale_fill_identity()+
            geom_line(data = col_alpha,
                      aes(x = week, y = threshold), 
                      col='cadetblue',
                      linetype = "dotdash", 
                      lwd = 1)+
            theme_minimal()+
            theme(legend.position = "none", 
                  axis.title.x=element_blank(),
                 # axis.text.x=element_blank(),
                #  axis.ticks.x=element_blank()
                )+
            labs(title = str_to_title(str_replace_all(c, "_", " ")), 
                 x = element_blank(), 
                 y = "Case count")
          
           tile_alpha <- b %>% 
            mutate(color = case_when(observed == 0 ~ "#FFFFFF00", 
                                     alert == TRUE & two_or_more == TRUE & (absolute_mag == TRUE | relative_mag == TRUE) ~ "firebrick4", 
                                     alert == TRUE & (absolute_mag == TRUE | relative_mag == TRUE) ~ "blueviolet" , 
                                     alert == TRUE ~ "cadetblue", 
                                     alert == FALSE ~ "grey", 
                                     is.na(alert) ~ "grey",
                                     .default = "black"), 
                   Shehia = str_to_title(str_replace_all(shehia, "_", " "))) 
           
           tile_vivid <- e %>% 
             mutate(color = case_when(observed == 0 ~ "#FFFFFF00", 
                                      alert == TRUE & two_or_more == TRUE & (absolute_mag == TRUE | relative_mag == TRUE) ~ "firebrick4", 
                                      alert == TRUE & (absolute_mag == TRUE | relative_mag == TRUE) ~ "blueviolet" , 
                                      alert == TRUE ~ "cadetblue", 
                                      alert == FALSE ~ "grey", 
                                      is.na(alert) ~ "grey",
                                      .default = "black"), 
                    Shehia = str_to_title(str_replace_all(shehia.x, "_", " "))) 
           
           
            bottom_panel <- ggplot()+
            geom_tile(data = tile_alpha, 
                      aes(x = week, y = Shehia, fill = color), 
                      alpha = 0.2)+
              geom_tile(data = tile_vivid, 
                        aes(x = week, y = Shehia, fill = color))+  
            theme_minimal()+
            scale_fill_identity()+
            labs(x = "Week", 
                 y = "Shehia")
          
          cowplot::plot_grid(top_panel, NULL, bottom_panel, 
                             ncol = 1, 
                             rel_heights = c(1,-0.2, 1),
                             align = "hv", 
                             axis = "tblr")
          })







b[[2]]

district_conflict <- filter(flagged1, outbreak_district_flagged == "no")

shehia_conflict <- filter(flagged1, outbreak_shehia_flagged != "yes")


test <- flagged_week_start()

trim_mon <- test %>% 
  select(-c(week, year, week_char, shehia, district))

sun <- flagged_week_start(7)

trim_sum <- sun %>% 
  select(-c(week, year, week_char, shehia, district))

clean_up <- function(x, ...){x %>% 
    select(district = district_for_shehia, 
           shehia, 
           date_fix, 
           facility) %>% 
    mutate(year_week = tsibble::yearweek(date_fix, ...), 
           district_shehia = paste0(district, "-", shehia))}

data_filtered <- rio::import("data_filtered_for_tst.xlsx")
data_raw <- rio::import("znz_data_trends.xlsx")
data_deduped <- rio::import("deduped_unfiltered_data.xlsx") %>%  # 65244 obs
  select(-date_fix) %>% 
  rename(date_fix = date)


# issue with linking idsr data to flagged shehia data due to 
e_idsr <- rio::import("eIDSR_2021_2024_wk36.xlsx") %>% 
  Rtesunate::act_clean()%>% 
  mutate(district = str_replace_all(district, "\\.", "_"), 
         week_char = case_when(as.numeric(week) < 10 ~ paste0("0", as.character(week)), 
                               .default = as.character(week)),
         district_shehia = paste0(district, "-", shehia))



data_mon <- map(list(data_raw, data_deduped, data_filtered), 
            ~.x %>% 
              clean_up() %>% 
              group_by(district_shehia, year_week) %>% 
              summarise(count_mon = n()) %>% 
              ungroup() %>% 
              right_join(trim_mon, by = c("year_week", 
                                                 "district_shehia")) %>% 
              mutate(comp_mon = count_mon == total_cases))  %>% 
  set_names(c("raw", "deduped", "filtered"))


data_sun <- map(list(data_raw, data_deduped, data_filtered), 
            ~.x %>% 
              clean_up(week_start = 7) %>% 
              group_by(district_shehia, year_week) %>% 
              summarise(count_sun = n()) %>% 
              ungroup() %>% 
              right_join(trim_sum, by = c("year_week", 
                                  "district_shehia")) %>% 
              mutate(comp_sun = count_sun == total_cases) %>% 
              select(c("year_week", 
                       "district_shehia", 
                       "comp_sun", 
                       "count_sun")))  %>% 
  set_names(c("raw", "deduped", "filtered"))

sun_mon_counts <- map2(data_sun, data_mon, 
                       ~bind_cols(.x, .y))



test <- sun_mon_counts[[1]]

data1 <- data %>% 
  map(~.x %>% 
        group_by(year_week, district_shehia) %>% 
        mutate(linelist_count = n()) %>%
        ungroup() %>% 
        mutate(consistent = linelist_count == total_cases) %>% 
        filter(consistent == FALSE))


temp1 <- data1[[1]]


## plan to add arrows for those flagged 




temp <- filtered_shehia_alerts %>% 
  filter(district == "chakechake") %>% 
  distinct(shehia)


fu <- filter(flagged, is.na(district.y)) %>% 
  arrange(district.x, )
