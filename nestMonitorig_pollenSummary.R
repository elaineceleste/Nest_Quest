#### Summarizing pollen counts from 2024-2025 Nest Quest nest monitoring data. 
### EB 13 August 2026. Decided it would be easier to do the pollen in its own script because I kept getting confused. 
## Other nest monitoring numbers are summarized in nestMonitoring_summary.R script. 

#load packages
library(tidyverse) #dplyr package
library(lubridate) #dealing with time and dates
library(readr) # creating .csv files
library(lme4) 
library(colorBlindness)
library(forcats)
library(gridExtra) # for creating plot grids
library(dplyr)

#load data 
activity_raw <- read.csv('nest_observation.csv')
activity_raw_survey123 <- read.csv('survey123_nest_traffic.csv')
sites <- read.csv("nest_survey_site_data.csv") 

#Some data cleaning. Using some of Sami's NQ_nest_observation.R code
#Starting with the main dataset

activity_raw <- activity_raw %>% mutate(., site = tolower(site)) 

activity_raw <- activity_raw %>% filter(., site != '')# %>% # removing empty rows
#mutate(., count.pollen_size = paste(amt_enter, pollen_size, sep = ':')) 

activity_unite <- unite(activity_raw, new_enter, amt_enter, amt_enter.1, amt_enter.2, amt_enter.3, amt_enter.4, amt_enter.5, sep = ":") #Creates new column with all of the entrance data
activity_unite <- unite(activity_unite, new_pollen, pollen_size, pollen_size.1, pollen_size.2, pollen_size.3, pollen_size.4, pollen_size.5, sep = ":") #Creates a new column with all of the pollen data

all_exit <- activity_unite %>% 
  select(., !c("new_enter", "new_pollen")) %>% 
  mutate(amt_exit = ifelse(is.na(amt_exit), 0, amt_exit),
         activity = 'exit', .before = amt_exit,
         year = as.character(year),
         tempC = as.numeric(tempC),
         tempF = as.numeric(tempF)) %>% 
  uncount(amt_exit) #Creates a new data frame with the exit activity

activity_separated <- activity_unite %>% 
  select(., !c('amt_exit')) %>% #Exclude amt_exit data
  separate_rows(., c(new_enter, new_pollen), sep = ":") %>% 
  mutate(new_enter = na_if(new_enter, "NA"),
         new_pollen = na_if(new_pollen, 'na')) %>% 
  mutate(new_pollen = na_if(new_pollen, '')) %>% 
  mutate(year = as.character(year),
         tempC = as.numeric(tempC),
         tempF = as.numeric(tempF),
         new_enter = as.numeric(new_enter)) #Creates a row for each pollen size and the number of bees with that pollen size per observation. This data frame is ONLY entering bees. 

activity_separated <- activity_separated[!is.na(activity_separated$new_enter), ] #Removes NA rows where the new_enter value is NA

all_enter <- activity_separated %>% 
  uncount(new_enter) %>% 
  mutate(activity = 'enter', .before = new_pollen) #Creates a new data frame and gives each entering bee its own row

all_activity <- full_join(all_enter, all_exit) #Creating new data frame, joining the enter and exit data

all_activity <- all_activity %>% rename(., 
                                        pollen_size = new_pollen,
                                        role = staff.or.volunteer) %>% 
  mutate(species = case_when(grepl('imp', nest_ID) ~ "impatiens",
                             grepl('auri', nest_ID) ~ 'auricomus',
                             grepl('rufo', nest_ID) ~ 'rufocinctus',
                             grepl('gris', nest_ID) ~ 'griseocollis',
                             grepl('ferv', nest_ID) ~ 'fervidus')) #Creating column for species name

table(all_activity$nest_ID) #Checking nest IDs are correct

all_activity <- all_activity %>%
  mutate(., nest_ID = tolower(nest_ID)) #Making all nest IDs lowercase

all_activity <- all_activity %>%
  mutate(year = as.numeric(year))

all_activity <- all_activity %>%
  mutate(month = as.numeric(month))

all_activity <- all_activity %>%
  mutate(day = as.numeric(day))

all_activity <- all_activity %>%
  mutate(tempF = as.character(tempF))

###NOW cleaning the Survey123 data to merge into the main dataset. 

activity_raw_survey123 <- activity_raw_survey123 %>% 
  rename(., 
         observer1 = Primary.observer,
         observer2 = Secondary.observer,
         nest_ID = Nest.ID,
         tempF = Temperature_F,
         wind = Wind.speed,
         ent_shade = How.much.shade.is.there.by.the.primary.entrance.,
         activity_pollen = Number.of.bees.entering.nest.with.evident.pollen.loads,
         S = Number.of.small.pollen.loads,
         M = Number.of.medium.pollen.loads,
         L = Number.of.large.pollen.loads,
         activity_no_pollen = Number.of.bees.entering.nest.with.NO.evident.pollen.loads,
         activity_exit = Number.of.bees.exiting.nest) #Renaming columns 

activity_raw_survey123 <- activity_raw_survey123 %>%
  unite(notes, Please.note.any.other.animals.you.saw.at.the.nest.entrance, Please.note.any.other.observations, sep = ";")

survey123 <- activity_raw_survey123 %>%
  select(Start.time, End.time, observer1, observer2, nest_ID, tempF, wind, ent_shade, activity_pollen, S, M, L, activity_no_pollen, activity_exit, notes)
#activity_pollen column is the total number of bees that entered with pollen. s = number with small pollen loads, etc. activity_no_pollen is bees that entered that did not have evident pollen loads. 

survey123$activity_pollen[13] <- 2 #Manually changing the NA value in the activity_pollen column row 13

survey123 <- survey123 %>%
  mutate(
    start_datetime = as.POSIXct(
      Start.time,
      format = "%m/%d/%Y %I:%M:%S %p"
    ),
    end_datetime = as.POSIXct(
      End.time,
      format = "%m/%d/%Y %I:%M:%S %p"
    ),
    
    date = as.Date(start_datetime),
    
    start_time = format(start_datetime, "%I:%M:%S %p"),
    end_time = format(end_datetime, "%I:%M:%S %p")) #Putting start and end time in their own columns

survey123 <- survey123 %>%
  mutate(
    year = format(date, "%Y"),
    month = format(date, "%m"),
    day = format(date, "%d")
  ) #Creating day, month, year columns 

all_survey123 <- all_survey123 %>%
  mutate(
    start_time = format(
      as.POSIXct(start_time, format = "%I:%M:%S %p"),
      "%H:%M:%S"
    ),
    end_time = format(
      as.POSIXct(end_time, format = "%I:%M:%S %p"),
      "%H:%M:%S"
    )
  ) #Convering to 24 hour time

enter_survey123 <- survey123 %>%
  select(-activity_pollen, -activity_exit) %>%
  pivot_longer(
    cols = c(S, M, L, activity_no_pollen),
    names_to = "pollen_size",
    values_to = "count"
  ) %>%
  filter(!is.na(count), count > 0) %>%
  uncount(count) %>%
  mutate(
    activity = "enter",
    pollen_size = recode(
      pollen_size,
      S = "small",
      M = "medium",
      L = "large",
      activity_no_pollen = "none"
    )
  ) #New data frame with one row per bee (pivot_longer)

exit_survey123 <- survey123 %>%
  select(-activity_pollen, -S, -M, -L, -activity_no_pollen) %>%
  filter(!is.na(activity_exit), activity_exit > 0) %>%
  uncount(activity_exit) %>%
  mutate(
    activity = "exit",
    pollen_size = NA_character_
  ) #Data frame for all of the exiting bees, each gets its own row

all_survey123 <- bind_rows(enter_survey123, exit_survey123) #Combine bees entering and exiting for Survey123 data

clean_survey123 <- all_survey123 %>%
  select(nest_ID, year, month, day, start_time, end_time, wind, ent_shade, tempF, activity, pollen_size, observer1, observer2, notes)

clean_survey123 <- clean_survey123 %>%
  mutate(nest_ID = trimws(as.character(nest_ID)),
         nest_ID = case_when(
           nest_ID == "Crow1_imp1" ~ "25Crow1_imp1",
           nest_ID == "AH_LU_imp3" ~ "25ah_lowerupper_imp3",
           nest_ID == "Crow Hassan Bug Out" ~ "24Crow_imp_gc",
           nest_ID == "Crow2_imp_1" ~ "24Crow2_imp_1",
           nest_ID == "CrowGC_imp1" ~ "24Crow_imp_gc",
           nest_ID == "HP_1_imp1" ~ "25HP1_imp1",
           nest_ID == "Hp2 imp1" ~ "25HP2_imp1",
           nest_ID == "LE_barn_ferv" ~ "25LE_barn_ferv",
           nest_ID == "LE_Barn_ferv" ~ "25LE_barn_ferv",
           nest_ID == "LH_schultz_gris2" ~ "25LH_Schultz_rufo1",
           nest_ID == "Schultz Lake" ~ "25LH_Schultz_gris1",
           TRUE ~ nest_ID
         ))

clean_survey123 <- clean_survey123 %>%
  mutate(species = case_when(grepl('imp', nest_ID) ~ "impatiens",
                             grepl('auri', nest_ID) ~ 'auricomus',
                             grepl('rufo', nest_ID) ~ 'rufocinctus',
                             grepl('gris', nest_ID) ~ 'griseocollis',
                             grepl('ferv', nest_ID) ~ 'fervidus'))

clean_survey123 <- clean_survey123 %>%
  mutate(year = as.numeric(year))

clean_survey123 <- clean_survey123 %>%
  mutate(month = as.numeric(month))

clean_survey123 <- clean_survey123 %>%
  mutate(day = as.numeric(day))

clean_survey123 <- clean_survey123 %>%
  mutate(tempF = as.character(tempF))

###NOW combining the main dataset and survey123 data! 
combined_activity <- bind_rows(all_activity, clean_survey123)

#Final cleaning
combined_activity <- combined_activity %>% mutate(., nest_ID = tolower(nest_ID))

combined_activity <- combined_activity %>%
  mutate(nest_ID = case_when(
    nest_ID == "crow1_imp1" ~ "25crow1_imp1",
    TRUE ~ nest_ID
  )) #Fixing names

combined_activity <- combined_activity %>%
  mutate(pollen_size = case_when(
    pollen_size == "small" ~ "S",
    pollen_size == "medium" ~ "M", 
    pollen_size == "large" ~ "L",
    TRUE ~ pollen_size
  )) #Could do this above with the pivot_longer function instead of having to rename things here

combined_activity <- unite(combined_activity, date, day, month, year, sep = '-', remove = FALSE) #Creates a column with the date like 01-01-2024 format
combined_activity <- combined_activity %>% mutate(date = as.Date(date, "%d-%m-%Y"),
                                        week_true = as.factor(strftime(c(date), format = "%W")),
                                        floor_date = floor_date(date, 'week'),
                                        floor_date = format(floor_date, "%b-%d"),
                                        duration = 20) #Creates new columns for week, floor date, and survey duration 

combined_activity <- unite(combined_activity, nest_ID, date, start_time, end_time, observer1, sep = ":", remove = FALSE, col = unique_ID) #Creates a unique ID for each survey

n_distinct(combined_activity$unique_ID)
#77 total observations where there were bees seen. This data set does not include observations that had 0 bees, since we are aiming to summarize pollen. 

n_distinct(combined_activity$nest_ID) 
#16 nests - other 2 excluded since their observations had no bees. I think? 

table(combined_activity$pollen_size)
#L    M none    S   XL   XS 
#10   30  502   52    8   32 

table(combined_activity$activity)
#enter  exit 
#652   714 

combined_activity <- combined_activity %>%
  mutate(weight_g = case_when(
    pollen_size == "none" ~ 0,
    pollen_size == "XS" ~ NA,
    pollen_size == "S" ~ 6,
    pollen_size == "M" ~ 13,
    pollen_size == "L" ~ 20,
    pollen_size == "XL" ~ 45,
    TRUE ~ NA_real_
  )) #creates a column for pollen weights 

#pollen = c('none', 'XS', 'S', 'M', 'L', 'XL'),
#weight_g = c(0, NA, 6, 13, 20, 45))

pollen_nestID <- combined_activity %>%  filter(., activity == 'enter') %>% group_by(nest_ID, pollen_size) %>% 
  summarize(., total_g_pollen = sum(weight_g)) %>% na.omit() #Creates a dataframe that shows the total amount of pollen per nest

unique_survey_pollen <- combined_activity %>%
  filter(., activity == 'enter') %>%
  group_by(unique_ID, nest_ID, pollen_size) %>%
  summarize(., total_g_pollen = sum(weight_g)) %>%
  na.omit() #Data frame with pollen amounts per survey, rather than per nest

pollen_range <- combined_activity %>%
  group_by(nest_ID) %>%
  summarise(
    min_pollen = min(weight_g, na.rm = TRUE),
    max_pollen = max(weight_g, na.rm = TRUE),
    avg_pollen = mean(weight_g, na.rm = TRUE),
    .groups = "drop"
  ) #Dataframe with ranges of pollen intake for each nest

ggplot(pollen_range, aes(x = nest_ID)) +
  geom_linerange(
    aes(ymin = min_pollen, ymax = max_pollen),
    linewidth = 1
  ) +
  geom_point(
    aes(y = avg_pollen),
    size = 3
  ) +
  labs(
    x = "Nest ID",
    y = "Pollen weights (g)",
    title = "Average pollen intake per nest per 20 minutes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
