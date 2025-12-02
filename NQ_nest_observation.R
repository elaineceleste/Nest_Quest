# Nest Quest Nest Activity Data Start on 11/25/2025 SD

#load packages
library(tidyverse) #dplyr package
library(lubridate) #dealing with time and dates
library(readr) # creating .csv files
library(lme4) 

#load data 
activity_raw <- read.csv('nest_observation.csv')

#cleaning
activity_raw <- activity_raw %>% filter(., site != '') # removing empty rows

pivot <- activity_raw %>%
  pivot_longer(., cols = starts_with('amt'), names_to = "activity", values_to = 'count') %>% 
  mutate(count = ifelse(is.na(count), 0, count)) 

sum(pivot$count) # 1202 total bees observed
  
pivot2 <- pivot %>% 
  pivot_longer(, cols = starts_with('pollen'), names_to = "pollen", values_to = "pollen_size")  %>% 
  mutate(pollen_size = na_if(pollen_size, 'na'),
         pollen_size = na_if(pollen_size, ''))


         