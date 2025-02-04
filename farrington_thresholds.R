# Calculation of epidemic thresholds by improved Farrington for filtered data. 
# Output is exported Excel sheets for further manipulation
# rm(list = ls())

pacman::p_load(rio, 
               tsibble,
               surveillance,
               tidyverse)

pacman::p_load_gh("reconhub/linelist")


#data <- rio::import("znz_data_trends.xlsx") # 66077 obs

data_test <- rio::import("deduped_unfiltered_data.xlsx") # 65244 obs
data_test2 <- rio::import("data_unfiltered_clean.xlsx")
data_filtered <- rio::import("data_filtered_for_tst.xlsx")



# district
all_sts_df <- data %>% 
  mutate(year_week = yearweek(date)) %>% 
  count(year_week, district_for_shehia, .drop = FALSE) %>%   
  complete(year_week, district_for_shehia, fill = list(n = 0)) %>% 
  tsibble(key = district_for_shehia, index = year_week) %>% 
  mutate(year = epiyear(year_week), 
         week = isoweek(year_week))

filtered_sts_df <- data_filtered %>% 
  mutate(year_week = yearweek(date_fix)) %>% 
  count(year_week, district_for_shehia, .drop = FALSE) %>%   
  complete(year_week, district_for_shehia, fill = list(n = 0)) %>% 
  tsibble(key = district_for_shehia, index = year_week) %>% 
  mutate(year = epiyear(year_week), 
         week = isoweek(year_week))

# filtered_sts_df_temp <- data_filtered %>% 
#   mutate(year_week = yearweek(date_fix)) %>% 
#   group_by(year_week, district_for_shehia, facility) %>% 
#   summarise(counts = n())
# 
# filtered_sts_df_temp1 <- data_filtered %>% 
#   mutate(year_week = yearweek(date_fix)) %>% 
#   group_by(year_week, district_for_shehia) %>% 
#   summarise(counts = n())
# 
#   count(year_week, district_for_shehia, facility,.drop = FALSE) %>%   
#   complete(year_week, district_for_shehia, fill = list(n = 0)) %>% 
#   tsibble(key = district_for_shehia, index = year_week) %>% 
#   mutate(year = epiyear(year_week), 
#          week = epiweek(year_week))

# local only
#  there are multiple classications here (imported, in_progress, indigenous, 
#  induced, introduced, lost_to_followup, relapse, relapsing, unclassified)
#  keeping all EXCEPT 
all_sts_df_local <- data %>% 
  mutate(year_week = yearweek(date)) %>% 
  filter(!case_classification_categories %in% c("imported", "introduced")) %>% 
  count(year_week, district_for_shehia, .drop = FALSE) %>%   
  complete(year_week, district_for_shehia, fill = list(n = 0)) %>% 
  tsibble(key = district_for_shehia, index = year_week) %>% 
  mutate(year = epiyear(year_week), 
         week = isoweek(year_week))

filtered_sts_df_local <- data_filtered %>% 
  mutate(year_week = yearweek(date_fix)) %>%
  filter(!case_classification_categories %in% c("imported", "introduced")) %>% 
  count(year_week, district_for_shehia, .drop = FALSE) %>%   
  complete(year_week, district_for_shehia, fill = list(n = 0)) %>% 
  tsibble(key = district_for_shehia, index = year_week) %>% 
  mutate(year = epiyear(year_week), 
         week = isoweek(year_week))

###############################################################################
# shehia #################################################################
###############################################################################


all_sts_shehia_df <- data %>% 
  mutate(year_week = yearweek(date), 
         shehia_id = paste0(district_for_shehia, "-", shehia)) %>% 
  count(year_week, shehia_id, .drop = FALSE) %>%   
  complete(year_week, shehia_id, fill = list(n = 0)) %>% 
  tsibble(key = shehia_id, index = year_week) %>% 
  mutate(year = epiyear(year_week), 
         week = isoweek(year_week))

filtered_sts_shehia_df <- data_filtered %>% 
  mutate(year_week = yearweek(date_fix), 
         shehia_id = paste0(district_for_shehia, "-", shehia)) %>% 
  count(year_week, shehia_id, .drop = FALSE) %>%   
  complete(year_week, shehia_id, fill = list(n = 0)) %>% 
  tsibble(key = shehia_id, index = year_week) %>% 
  mutate(year = epiyear(year_week), 
         week = isoweek(year_week))

# local only -- same classifications used from above
all_sts_shehia_df_local <- data %>% 
  filter(!case_classification_categories %in% c("imported", "introduced")) %>% 
  mutate(year_week = yearweek(date), 
         shehia_id = paste0(district_for_shehia, "-", shehia)) %>% 
  count(year_week, shehia_id, .drop = FALSE) %>%   
  complete(year_week, shehia_id, fill = list(n = 0)) %>% 
  tsibble(key = shehia_id, index = year_week) %>% 
  mutate(year = epiyear(year_week), 
         week = isoweek(year_week))

filtered_sts_shehia_df_local <- data_filtered %>% 
  filter(!case_classification_categories %in% c("imported", "introduced")) %>% 
  mutate(year_week = yearweek(date_fix), 
         shehia_id = paste0(district_for_shehia, "-", shehia)) %>% 
  count(year_week, shehia_id, .drop = FALSE) %>%   
  complete(year_week, shehia_id, fill = list(n = 0)) %>% 
  tsibble(key = shehia_id, index = year_week) %>% 
  mutate(year = epiyear(year_week), 
         week = isoweek(year_week))


###############################################################################
##  all cases ###########################################################
###############################################################################

filtered_data <- map(list(filtered_sts_df, filtered_sts_shehia_df), 
                     function(x)map(list(2022, 2023, 2024), function(y)x %>% 
                                      filter(year <= y | (year <= y + 1 & week <= 4)) %>% 
                                      select(-c(year, week)) %>% 
                                      pivot_wider(names_from = 2,
                                                  values_from = n)
                                    # list_cbind()
                     )) %>% 
  set_names("filtered_sts_df", "filtered_sts_shehia_df")




filtered_sts <- map(filtered_data, function(x) map(x, 
                                                   function(y) sts(y[,-1],
                                                                   start = c(2019, 1),
                                                                   frequency = 52)
                                                   )
                    ) %>% 
  set_names("filtered_sts", "filtered_sts_shehia")



#### Test configs for issue with sliding date ###

filtered_data <- map(list(filtered_sts_df, filtered_sts_shehia_df), 
                     function(x)map(list(2022, 2023, 2024), function(y)x %>% 
                                      filter(year <= y | (year <= y + 1 & week <= 4)) %>% 
                                      select(-c(year, week)) %>% 
                                      pivot_wider(names_from = 2,
                                                  values_from = n) %>% 
                                      mutate(epoch = as.Date(year_week))
                     )) %>% 
  set_names("filtered_sts_df", "filtered_sts_shehia_df")

data_epoch <- filtered_data %>% 
  map(~.x %>% 
        map(~.x %>% ungroup() %>% 
              pull(epoch)))

filtered_data1 <- filtered_data %>% 
  map(~.x %>% 
        map(~.x %>% select(-epoch)))

filtered_sts <- map2(filtered_data1, data_epoch, 
                     function(x, y) map2(x,y,
                                         function(z, z1) sts(z[,-1],
                                                             epoch = z1)
                                         )
                     ) %>% 
  set_names("filtered_sts", "filtered_sts_shehia")



controls_w3 <- map(list(3,4,5), function(x) list(noPeriods=10,
                                              b=x, w=3, weightsThreshold=2.58,
                                              #  reweight = TRUE,
                                              pastWeeksNotIncluded=26,
                                              limit54=c(1,50), #limit54=c(cases,period) turns threshold to NA and alarm to false if less than -cases- in -period- weeks - effectively turned off for graphics
                                              # thresholdMethod = "nbPlugin",
                                              pThresholdTrend=1,
                                              alpha=0.1))

test_w3 <- map(filtered_sts, function(x) map2(x, controls_w3,
                                           function(y, z)farringtonFlexible(y, control = z)))


b <- list()
for (i in 1:length(test_w3)){
  a <- names(test_w3)[[i]]
  b[[i]] <- test_w3[[i]] %>% 
    set_names(map(list(2022, 2023, 2024), ~paste0(a, "_", .x)))
}

c <- unlist(b) %>% 
  map(~as.data.frame(.x, as.Date = TRUE))

filtered_district_w3_alpha_1 <- c[1:3] %>% 
  list_rbind() %>% 
  mutate(id = row_number()) %>% 
  arrange(desc(id)) %>% 
  distinct(epoch, .keep_all = TRUE) %>% 
  arrange(id) 



rio::export(filtered_district_w3_alpha_1,
            "filtered_district_alert_alpha_1_w3.xlsx") 



controls <- map(list(3,4,5), function(x) list(noPeriods=10,
                                              b=x, w=2, weightsThreshold=2.58,
                                              #  reweight = TRUE,
                                              pastWeeksNotIncluded=26,
                                              limit54=c(1,50), #limit54=c(cases,period) turns threshold to NA and alarm to false if less than -cases- in -period- weeks - effectively turned off for graphics
                                              # thresholdMethod = "nbPlugin",
                                              pThresholdTrend=1,
                                              alpha=0.1))

test <- map(filtered_sts, function(x) map2(x, controls,
                                              function(y, z)farringtonFlexible(y, control = z)))

#test <- map(filtered_sts, function(x) map2(x, controls,
#                                           function(y, z)farringtonFlexible(y, control = z)))


controls_test <- map(list(3, 4,5),
                     function(x) map(list(0.01, 0.1), 
                                     function(y) list(noPeriods=10,
                                              b=x, w=2, weightsThreshold=2.58,
                                              #  reweight = TRUE,
                                              pastWeeksNotIncluded=26,
                                              limit54=c(1,50), #limit54=c(cases,period) turns threshold to NA and alarm to false if less than -cases- in -period- weeks - effectively turned off for graphics
                                              # thresholdMethod = "nbPlugin",
                                              pThresholdTrend=1,
                                              alpha=y)))

test1 <- map(filtered_sts, function(x) map2(x, controls_test,
                                           function(y, z)map(z, function(a)farringtonFlexible(y, 
                                                                            control = a))))


b <- list()
for (i in 1:length(test1)){
  a <- names(test1)[[i]]
  b[[i]] <- test1[[i]] %>% 
    set_names(map(list(2022, 2023, 2024), ~paste0(a, "_", .x)))
}

c <- unlist(b) %>% 
  map(~as.data.frame(.x, as.Date = TRUE))


filtered_district_01 <- c[c(1,3,5)] %>% 
  list_rbind() %>% 
  mutate(id = row_number()) %>% 
  arrange(desc(id)) %>% 
  distinct(epoch, .keep_all = TRUE) %>% 
  arrange(id) 

filtered_district_1 <- c[c(2,4,6)] %>% 
  list_rbind() %>% 
  mutate(id = row_number()) %>% 
  arrange(desc(id)) %>% 
  distinct(epoch, .keep_all = TRUE) %>% 
  arrange(id) 

filtered_shehia_01 <- c[c(7,9,11)] %>% 
  list_rbind() %>% 
  mutate(id = row_number()) %>% 
  arrange(desc(id)) %>% 
  distinct(epoch, .keep_all = TRUE) %>% 
  arrange(id) 

filtered_shehia_1 <- c[c(8,10,12)] %>% 
  list_rbind() %>% 
  mutate(id = row_number()) %>% 
  arrange(desc(id)) %>% 
  distinct(epoch, .keep_all = TRUE) %>% 
  arrange(id) 

map2(list(filtered_district_01, 
          filtered_district_1, 
          filtered_shehia_01,
          filtered_shehia_1), 
     list("filtered_district_alert_alpha_01.xlsx", 
          "filtered_district_alert_alpha_1.xlsx",
          "filtered_shehia_alert_alpha_01.xlsx",
          "filtered_shehia_alert_alpha_1.xlsx"), 
     ~rio::export(.x, .y))


###############################################################################
##  local cases only #########################################################
###############################################################################

# this could be done iteratively 



filtered_data_local <- map(list(filtered_sts_df_local, 
                                filtered_sts_shehia_df_local), 
                     function(x)map(list(2022, 2023, 2024), function(y)x %>% 
                                      filter(year <= y | (year <= y + 1 & week <= 4)) %>% 
                                      select(-c(year, week)) %>% 
                                      pivot_wider(names_from = 2,
                                                  values_from = n) %>% 
                                      mutate(epoch = as.Date(year_week))
                                    # list_cbind()
                     )) %>% 
  set_names("filtered_sts_df", "filtered_sts_shehia_df")


data_epoch <- filtered_data_local %>% 
  map(~.x %>% 
        map(~.x %>% ungroup() %>% 
              pull(epoch)))

filtered_data1 <- filtered_data_local %>% 
  map(~.x %>% 
        map(~.x %>% select(-epoch)))

filtered_sts_local <- map2(filtered_data1, data_epoch, 
                     function(x, y) map2(x,y,
                                         function(z, z1) sts(z[,-1],
                                                             epoch = z1)
                     )
) %>% 
  set_names("filtered_sts", "filtered_sts_shehia")


#   filtered_sts_local <- map(filtered_data_local, function(x) map(x, 
#                                                      function(y) sts(y[,-1],
#                                                                      start = c(2019, 1),
#                                                                      frequency = 52)
#   )
#   ) %>% 
#     set_names("filtered_sts", "filtered_sts_shehia")
#   


controls_test <- map(list(3, 4,5),
                     function(x) map(list(0.01, 0.1), 
                                     function(y) list(noPeriods=10,
                                                      b=x, w=2, weightsThreshold=2.58,
                                                      #  reweight = TRUE,
                                                      pastWeeksNotIncluded=26,
                                                      limit54=c(1,50), #limit54=c(cases,period) turns threshold to NA and alarm to false if less than -cases- in -period- weeks - effectively turned off for graphics
                                                      # thresholdMethod = "nbPlugin",
                                                      pThresholdTrend=1,
                                                      alpha=y)))

testlocal <- map(filtered_sts_local, function(x) map2(x, controls_test,
                                            function(y, z)map(z, function(a)farringtonFlexible(y, 
                                                                                               control = a))))



# controls <- map(list(3,4,5), function(x) list(noPeriods=10,
#                                               b=x, w=2, weightsThreshold=2.58,
#                                               #  reweight = TRUE,
#                                               pastWeeksNotIncluded=26,
#                                               limit54=c(1,50), #limit54=c(cases,period) turns threshold to NA and alarm to false if less than -cases- in -period- weeks - effectively turned off for graphics
#                                               # thresholdMethod = "nbPlugin",
#                                               pThresholdTrend=1,
#                                               alpha=0.1))
# 
# 
# 
# test <- map(filtered_sts_local, function(x) map2(x, controls,
#                                            function(y, z)farringtonFlexible(y, control = z)))


b <- list()
for (i in 1:length(testlocal)){
  a <- names(testlocal)[[i]]
  b[[i]] <- testlocal[[i]] %>% 
    set_names(map(list(2022, 2023, 2024), ~paste0(a, "_", .x)))
}

c <- unlist(b) %>% 
  map(~as.data.frame(.x, as.Date = TRUE))


filtered_district_local_01 <- c[c(1,3,5)] %>% 
  list_rbind() %>% 
  mutate(id = row_number()) %>% 
  arrange(desc(id)) %>% 
  distinct(epoch, .keep_all = TRUE) %>% 
  arrange(id) 


filtered_district_local_1 <- c[c(2,4,6)] %>% 
  list_rbind() %>% 
  mutate(id = row_number()) %>% 
  arrange(desc(id)) %>% 
  distinct(epoch, .keep_all = TRUE) %>% 
  arrange(id) 

filtered_shehia_local_01 <- c[c(7,9,11)] %>% 
  list_rbind() %>% 
  mutate(id = row_number()) %>% 
  arrange(desc(id)) %>% 
  distinct(epoch, .keep_all = TRUE) %>% 
  arrange(id) 

filtered_shehia_local_1 <- c[c(8,10,12)] %>% 
  list_rbind() %>% 
  mutate(id = row_number()) %>% 
  arrange(desc(id)) %>% 
  distinct(epoch, .keep_all = TRUE) %>% 
  arrange(id) 

map2(list(filtered_district_local_01,
          filtered_district_local_1,
          filtered_shehia_local_01,
          filtered_shehia_local_1), 
     list("filtered_district_local_alert_01.xlsx",
          "filtered_district_local_alert_1.xlsx",
          "filtered_shehia_local_alert_01.xlsx",
          "filtered_shehia_local_alert_1.xlsx"), 
     ~rio::export(.x, .y))