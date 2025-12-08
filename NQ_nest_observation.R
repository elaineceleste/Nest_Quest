# Nest Quest Nest Activity Data Start on 11/25/2025 SD

#load packages
library(tidyverse) #dplyr package
library(lubridate) #dealing with time and dates
library(readr) # creating .csv files
library(lme4) 

#load data 
activity_raw <- read.csv('nest_observation.csv')

#cleaning
activity_raw <- activity_raw %>% filter(., site != '')# %>% # removing empty rows
                #mutate(., count.pollen_size = paste(amt_enter, pollen_size, sep = ':')) 

activity_unite <- unite(activity_raw, new_enter, amt_enter, amt_enter.1, amt_enter.2, amt_enter.3, amt_enter.4, amt_enter.5, sep = ":")
activity_unite <- unite(activity_unite, new_pollen, pollen_size, pollen_size.1, pollen_size.2, pollen_size.3, pollen_size.4, pollen_size.5, sep = ":")

all_exit <- activity_unite %>% 
              select(., !c("new_enter", "new_pollen")) %>% 
              mutate(amt_exit = ifelse(is.na(amt_exit), 0, amt_exit),
                     activity = 'exit', .before = amt_exit,
                     year = as.character(year),
                     tempC = as.numeric(tempC),
                     tempF = as.numeric(tempF)) %>% 
              uncount(amt_exit)

activity_seperated <- activity_unite %>% 
                       select(., !c('amt_exit')) %>% 
                       separate_rows(., c(new_enter, new_pollen), sep = ":") %>% 
                       mutate(new_enter = na_if(new_enter, "NA"),
                              new_pollen = na_if(new_pollen, 'na')) %>% 
                       mutate(new_pollen = na_if(new_pollen, '')) %>% 
                       mutate(year = as.character(year),
                              tempC = as.numeric(tempC),
                              tempF = as.numeric(tempF),
                              new_enter = as.numeric(new_enter))

activity_seperated <- activity_seperated[!is.na(activity_seperated$new_enter), ]

all_enter <- activity_seperated %>% 
  uncount(new_enter) %>% 
  mutate(activity = 'enter', .before = new_pollen)

all_activity <- full_join(all_enter, all_exit)

all_activity <- all_activity %>% rename(., 
                                        pollen = new_pollen,
                                        role = staff.or.volunteer) %>% 
  mutate(species = case_when(grepl('imp', nest_ID) ~ "impatiens",
                   grepl('auri', nest_ID) ~ 'auricomus',
                   grepl('rufo', nest_ID) ~ 'rufocinctus',
                   grepl('gris', nest_ID) ~ 'griseocollis',
                   grepl('ferv', nest_ID) ~ 'fervidus'))

all_activity <- unite(all_activity, date, day, month, year, sep = '-', remove = FALSE)
all_activity <- all_activity %>% mutate(date = as.Date(date, "%d-%m-%Y"),
                                        week_true = as.factor(strftime(c(date), format = "%U")),
                                        floor_date = floor_date(date, 'week'),
                                        floor_date = format(floor_date, "%b-%d"),
                                        start_time = hm(start_time),
                                        end_time = hm(end_time))

                                        time_spent = difftime(end_time, start_time, units = "mins")



                                        start_time = as.POSIXct(start_time, format = "%H:%M"),
                                        end_time = hm(end_time),
                                        end_time = as.POSIXct(end_time, format = "%H:%M:%S"))

                                        start_time = format(start_time, "%R"))
                                        time_spent = difftime(end_time, start_time, units = "mins"))

                                        
                                        start_time = strftime(start_time, format = "%R"))
                                        end_time = hm(end_time),
                                        time_spent = difftime(end_time, start_time, units = "mins"))


 all_activity  %>% group_by(species, 
                            week_true, 
                            floor_date, 
                            nest_ID,
                            start_time, 
                            end_time) %>% 
                   summarize(in_total = sum(activity == "enter", na.rm = TRUE),
                   out_total = sum(activity == "exit", na.rm = TRUE),
                   total_activity = in_total + out_total)
   
   
  mutate(mins = as.numeric(difftime(end_time, start_time, unit = "mins")),
         bees_per_min = total_activity/mins) # main data frame to be used



         