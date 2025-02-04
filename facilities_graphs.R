
## This script produces graphs for each facility included in the gold standard
## subset to be included in the appendices

pacman::p_load(rio, 
               tsibble,      
               slider,
               feasts,   
               forecast,
               yardstick,
               surveillance,
               tidyverse)

pacman::p_load_gh("reconhub/linelist")
pacman::p_load_gh("reconverse/trending")


rm(list = ls())

data <- rio::import("data_filtered_for_tst.xlsx")


weeks_years <- tibble(weeks = rep(c(seq(1,53,1)), 6), 
                      year = factor(unlist(map(list(2019, 2020, 2021, 2022, 2023, 2024), 
                                               ~rep(.x, 53)))))
test <- data %>% 
  split(.$district_for_shehia) %>% 
  map(~.x %>% 
        split(.$shehia) %>% 
        map(~.x %>% 
              split(.$facility) %>% 
              map2(., names(.), function (a, b){if(nrow(a)>1){a %>% 
                  mutate(year_week = yearweek(date_fix), 
                         year = factor(as.character(epiyear(year_week))),
                         week = epiweek(year_week)) %>% 
                  count(year, week, .drop = FALSE) %>% 
                  right_join(weeks_years, by = c("week" = "weeks", 
                                                 "year" = "year")) %>%   
                  complete(year, week, fill = list(n = 0)) %>% 
                  ungroup() %>% 
                  mutate(color1 = case_when(year == "2019" ~ "#786060", 
                                            year == "2020" ~ "#486090", 
                                            year == "2021" ~ "#a890a8",
                                            year == "2022" ~ "#a84848", 
                                            year == "2023" ~ "#f0a860", 
                                            year == "2024" ~ "#d8c0a8", 
                                            .default = "black")) %>% 
                  rename(`Cases Reported` = n, 
                         Epiweek = week) %>% 
                  ggplot()+
                  geom_line(aes(x = Epiweek, 
                                y = `Cases Reported`, 
                                group = color1, 
                                colour = color1))+
                  scale_color_identity()+
                  coord_polar()+
                  theme_minimal()+
                  ggtitle(str_to_title(str_replace_all(b, "_", " ")))}else{NULL}}) %>% 
              keep(~!is.null(.))
        ))



temp <- list()
for (e in 1:length(test)){
  temp[[e]] <- list() 
  for (s in 1:length(test[[e]])){
    temp[[e]][[s]] <- list()
    a <- ceiling(length(test[[e]][[s]])/6)
    b <- length(test[[e]][[s]])
    for (i in 1:a){if(i<a){
      temp[[e]][[s]][[i]] <- cowplot::plot_grid(test[[e]][[s]][[6*(i-1)+1]], 
                                                test[[e]][[s]][[6*(i-1)+2]], 
                                                test[[e]][[s]][[6*(i-1)+3]],
                                                test[[e]][[s]][[6*(i-1)+4]],
                                                test[[e]][[s]][[6*(i-1)+5]],
                                                test[[e]][[s]][[6*(i-1)+6]], 
                                                ncol = 3)} else if (6*i-b == 1){temp[[e]][[s]][[i]] <- cowplot::plot_grid(test[[e]][[s]][[6*(i-1)+1]], 
                                                                                                                          test[[e]][[s]][[6*(i-1)+2]], 
                                                                                                                          test[[e]][[s]][[6*(i-1)+3]],
                                                                                                                          test[[e]][[s]][[6*(i-1)+4]],
                                                                                                                          test[[e]][[s]][[6*(i-1)+5]],NULL, ncol = 3)} else if (6*i-b == 2){temp[[e]][[s]][[i]] <- cowplot::plot_grid(test[[e]][[s]][[6*(i-1)+1]], 
                                                                                                                                                                                                                                      test[[e]][[s]][[6*(i-1)+2]], 
                                                                                                                                                                                                                                      test[[e]][[s]][[6*(i-1)+3]],
                                                                                                                                                                                                                                      test[[e]][[s]][[6*(i-1)+4]],NULL, NULL, ncol = 3)}else if (6*i-b == 3){temp[[e]][[s]][[i]] <- cowplot::plot_grid(test[[e]][[s]][[6*(i-1)+1]], 
                                                                                                                                                                                                                                                                                                                                                       test[[e]][[s]][[6*(i-1)+2]], 
                                                                                                                                                                                                                                                                                                                                                       test[[e]][[s]][[6*(i-1)+3]],NULL, NULL, NULL, 
                                                                                                                                                                                                                                                                                                                                                       ncol = 3)} else if (6*i-b == 4){temp[[e]][[s]][[i]] <- cowplot::plot_grid(test[[e]][[s]][[6*(i-1)+1]], 
                                                                                                                                                                                                                                                                                                                                                                                                                                 test[[e]][[s]][[6*(i-1)+2]],NULL, NULL, NULL, NULL, ncol = 3)} else if (6*i-b == 5){temp[[e]][[s]][[i]] <- cowplot::plot_grid(test[[e]][[s]][[6*(i-1)+1]],NULL, NULL, NULL, NULL,NULL, ncol = 3)}   
    }}
}


temp <- list()
for (e in 1:length(test)){
  temp[[e]] <- list() 
  for (s in 1:length(test[[e]])){
    temp[[e]][[s]] <- list()
    a <- ceiling(length(test[[e]][[s]])/6)
    b <- length(test[[e]][[s]])
    name <- str_to_title(str_replace_all(names(test[[e]])[[s]], "_", " "))
    
    for (i in 1:a){if(i<a){temp[[e]][[s]][[i]] <- cowplot::plot_grid(cowplot::ggdraw()+
                                                                       cowplot::draw_label(name, x = 0.2, fontface = "italic"),
                                                                     cowplot::plot_grid(test[[e]][[s]][[6*(i-1)+1]], 
                                                                                        test[[e]][[s]][[6*(i-1)+2]], 
                                                                                        test[[e]][[s]][[6*(i-1)+3]],
                                                                                        test[[e]][[s]][[6*(i-1)+4]],
                                                                                        test[[e]][[s]][[6*(i-1)+5]],
                                                                                        test[[e]][[s]][[6*(i-1)+6]], 
                                                                                        ncol = 3), ncol = 1, rel_heights = c(0.1,1))} 
      else if (6*i-b == 1){temp[[e]][[s]][[i]] <- cowplot::plot_grid(cowplot::ggdraw()+
                                                                       cowplot::draw_label(name, x = 0.2, fontface = "italic"),
                                                                     cowplot::plot_grid(test[[e]][[s]][[6*(i-1)+1]], 
                                                                                        test[[e]][[s]][[6*(i-1)+2]], 
                                                                                        test[[e]][[s]][[6*(i-1)+3]],
                                                                                        test[[e]][[s]][[6*(i-1)+4]],
                                                                                        test[[e]][[s]][[6*(i-1)+5]],
                                                                                        NULL, 
                                                                                        ncol = 3), ncol = 1, rel_heights = c(0.1,1))} 
      else if (6*i-b == 2){temp[[e]][[s]][[i]] <- cowplot::plot_grid(cowplot::ggdraw()+
                                                                       cowplot::draw_label(name, x = 0.2, fontface = "italic"),
                                                                     cowplot::plot_grid(test[[e]][[s]][[6*(i-1)+1]], 
                                                                                        test[[e]][[s]][[6*(i-1)+2]], 
                                                                                        test[[e]][[s]][[6*(i-1)+3]],
                                                                                        test[[e]][[s]][[6*(i-1)+4]],
                                                                                        NULL, 
                                                                                        NULL, 
                                                                                        ncol = 3), ncol = 1, rel_heights = c(0.1,1))}
      else if (6*i-b == 3){temp[[e]][[s]][[i]] <- cowplot::plot_grid(cowplot::ggdraw()+
                                                                       cowplot::draw_label(name, x = 0.2, fontface = "italic"),
                                                                     cowplot::plot_grid(test[[e]][[s]][[6*(i-1)+1]], 
                                                                                        test[[e]][[s]][[6*(i-1)+2]], 
                                                                                        test[[e]][[s]][[6*(i-1)+3]],
                                                                                        NULL, 
                                                                                        NULL, 
                                                                                        NULL, 
                                                                                        ncol = 3), ncol = 1, rel_heights = c(0.1,1))} 
      else if (6*i-b == 4){temp[[e]][[s]][[i]] <- cowplot::plot_grid(cowplot::ggdraw()+
                                                                       cowplot::draw_label(name, x = 0.2, fontface = "italic"),
                                                                     cowplot::plot_grid(test[[e]][[s]][[6*(i-1)+1]], 
                                                                                        test[[e]][[s]][[6*(i-1)+2]],
                                                                                        NULL, 
                                                                                        NULL, 
                                                                                        NULL, 
                                                                                        NULL, 
                                                                                        ncol = 3), ncol = 1, rel_heights = c(0.1,1))} 
      else if (6*i-b == 5){temp[[e]][[s]][[i]] <- cowplot::plot_grid(cowplot::ggdraw()+
                                                                       cowplot::draw_label(name, x = 0.2, fontface = "italic"),
                                                                     cowplot::plot_grid(test[[e]][[s]][[6*(i-1)+1]],
                                                                                        NULL, 
                                                                                        NULL, 
                                                                                        NULL, 
                                                                                        NULL,
                                                                                        NULL, 
                                                                                        ncol = 3), ncol = 1, rel_heights = c(0.1,1))}
    } 
  }}

temp1 <- temp %>% 
  set_names(names(test)) 

z <- map2(temp1, test, 
          function(a,b){set_names(a, names(b))}) %>% 
  map(., ~list_flatten(.x, name_spec = "{outer}_{inner}"))


folder <- names(z)
plot_names <- map(z, ~names(.x))

pmap(list(z, folder, plot_names),
     function(x, y, z)map2(x, z,
                           function(a,b){ggsave(filename = paste0(here::here("pdf_report",
                                                                             "facilities",
                                                                             y), 
                                                                  "/", b, ".png"), 
                                                plot = a, 
                                                width = 9, 
                                                height = 7.2, 
                                                units = "in")}
     )
)
