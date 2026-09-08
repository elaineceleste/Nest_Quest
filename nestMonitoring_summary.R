##Fresh take on some analysis and summary data for the 2024-2025 Nest Quest nest monitoring data. 
#EB 10 August 2026
#Goal to create some summaries by nest -> Colony activity ranges (average of x to x bee per minute), estimates of pollen intake per 20 minute survey
#This script is JUST FOR THE BEES PER MINUTE. POLLEN summaries are done in nestMonitoring_pollensummary.R

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
nest_surveys <- read.csv("time_log_nestQuest.csv")

#Start with data cleaning. Survey123 data first to join with the main data set
#first, renaming
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
         activity_exit = Number.of.bees.exiting.nest) 

#Updating NAs to 0s in the entering and exiting columns 
activity_raw_survey123 <- activity_raw_survey123 %>%
  mutate(activity_pollen = replace_na(activity_pollen, 0),
         S = replace_na(S, 0),
         M = replace_na(M, 0),
         L = replace_na(L, 0),
         activity_no_pollen = replace_na(activity_no_pollen, 0),
         activity_exit = replace_na(activity_exit, 0))

activity_raw_survey123$activity_pollen[13] <- 2 #manually updating activity_pollen in this row because the total was entered as NA or 0 but it should be 2. 

activity_raw_survey123 <- activity_raw_survey123 %>%
  mutate(total_enter = activity_pollen + activity_no_pollen)

activity_raw_survey123 <- activity_raw_survey123 %>%
  rename(.,
         amt_exit = activity_exit)

activity_survey123_noPollen <- activity_raw_survey123 %>%
  select(Start.time, End.time, observer1, observer2, nest_ID, tempF, wind, ent_shade, total_enter, amt_exit, Please.note.any.other.observations)

activity_survey123_noPollen <- activity_survey123_noPollen %>%
  rename(amt_enter = total_enter)

activity_survey123_noPollen <- activity_survey123_noPollen %>%
  mutate(total_activity = amt_enter + amt_exit)

activity_survey123_noPollen <- activity_survey123_noPollen %>%
  rename(.,
         #start_time = Start.time,
         #end_time = End.time,
         notes = Please.note.any.other.observations)

#Making date and start/end time their own columns
activity_survey123_noPollen <- activity_survey123_noPollen %>%
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
    end_time = format(end_datetime, "%I:%M:%S %p"),
    
    survey_minutes = as.numeric(
      difftime(end_datetime, start_datetime, units = "mins")
    )
  )
 #This does what I wanted it to do, but it creates new columns and keeps the originals, so then there are just a lot of columns. I made a new, cleaner data frame next. 

#New, cleaner data frame with only what we need to calculate the number of bees per minute per nest. 
activity2_noPollen <- activity_survey123_noPollen %>%
  select(nest_ID, date, start_time, end_time, survey_minutes, wind, ent_shade, tempF, amt_enter, amt_exit, total_activity, observer1, observer2, notes)

activity2_noPollen <- activity2_noPollen %>%
  mutate(
    year = format(date, "%Y"),
    month = format(date, "%m"),
    day = format(date, "%d")
  )

#Converting times to 24-hour time to make the join easier
activity2_noPollen <- activity2_noPollen %>%
  mutate(
    start_time = format(
      as.POSIXct(start_time, format = "%I:%M:%S %p"),
      "%H:%M:%S"
    ),
    end_time = format(
      as.POSIXct(end_time, format = "%I:%M:%S %p"),
      "%H:%M:%S"
    )
  )

#Cleaning the rest of the activity data so we can join data sets together. 

activity_raw <- activity_raw %>% mutate(., site = tolower(site)) 

activity_raw <- activity_raw %>%
  mutate(amt_enter = replace_na(amt_enter, 0),
         amt_enter.1 = replace_na(amt_enter.1,0),
         amt_enter.2 = replace_na(amt_enter.2, 0),
         amt_enter.3 = replace_na(amt_enter.3, 0),
         amt_enter.4 = replace_na(amt_enter.4, 0),
         amt_enter.5 = replace_na(amt_enter.5, 0),
         amt_exit = replace_na(amt_exit, 0)) #Replaces NA values with 0s #Check this.... it might add weird values? 
    
activity_raw <- activity_raw %>%
  mutate(total_enter = amt_enter + amt_enter.1 + amt_enter.2 + amt_enter.3 + amt_enter.4 + amt_enter.5) #Creates new column for total number of bees entering

activity_noPollen <- activity_raw %>%
  select(site, nest_ID, year, month, day, start_time, end_time, wind, ent_shade, tempC, tempF, AQI, total_enter, amt_exit, staff.or.volunteer, observer1, observer2, notes)

activity_noPollen <- activity_noPollen %>%
  rename(amt_enter = total_enter)

activity_noPollen <- activity_noPollen %>%
  mutate(total_activity = amt_enter + amt_exit)

activity_noPollen <- activity_noPollen %>%
  mutate(
    survey_minutes = as.numeric(difftime(
      strptime(end_time, format = "%H:%M"),
      strptime(start_time, format = "%H:%M"),
      units = "mins"
    ))
  ) #Adding column with total survey minutes

#Combining Survey123 and entered data sets to start generating some summary statistics
#First, standardizing the class of the data. Each data frame needs to be the same (character, numeric, etc.)

activity2_noPollen <- activity2_noPollen %>%
  mutate(year = as.numeric(year))

activity2_noPollen <- activity2_noPollen %>%
  mutate(month = as.numeric(month))

activity2_noPollen <- activity2_noPollen %>%
  mutate(day = as.numeric(day))

#activity2_noPollen <- activity2_noPollen %>%
 # mutate(
   # start_time = as.POSIXct(start_time, format = "%H:%M"),
   # end_time = as.POSIXct(end_time, format = "%H:%M")
 # )

activity2_noPollen <- activity2_noPollen %>%
  mutate(tempF = as.character(tempF))

all_noPollen <- bind_rows(activity_noPollen, activity2_noPollen) #Creates data frame with all of the observations without the pollen data

all_noPollen <- all_noPollen %>%
  mutate(
    tempF = as.numeric(tempF),
    tempC = (tempF - 32) * 5/9
  ) #Fixing temperature columns to remove the weird NA rows

all_noPollen <- all_noPollen %>%
  filter(!is.na(nest_ID) & nest_ID != "") #Removes NA rows

all_noPollen <- all_noPollen %>%
  mutate(survey_minutes = replace_na(survey_minutes, 20)) #Assumes each survey was 20 minutes if a start or end time was not recorded. 

#Cleaning nest_ID names
table(all_noPollen$nest_ID) #Checking nest ID names

all_noPollen <- all_noPollen %>%
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

all_noPollen <- all_noPollen %>%
  mutate(nest_ID = tolower(nest_ID))

all_noPollen$survey_minutes[69] <- 20 #Manually adjusting Kevin's wonky survey that said -760 survey minutes 

all_noPollen <- all_noPollen %>%
  mutate(bees_per_min = total_activity / survey_minutes) #Creating a column for bees per minute per observation 

all_noPollen <- all_noPollen %>%
  mutate(date = if_else(
    is.na(date),
    make_date(year, month, day),
    date)) #Filling in dates in the date column to use for visualizing the data

#Creating new data frame to show the ranges for each nest  
nest_range <- all_noPollen %>%
  group_by(nest_ID) %>%
  summarise(
    n_obs = n(),
    min_bees = min(bees_per_min, na.rm = TRUE),
    max_bees = max(bees_per_min, na.rm = TRUE),
    avg_bees = mean(bees_per_min, na.rm = TRUE),
    .groups = "drop"
  ) 

#Creating some visuals 
ggplot(all_noPollen, 
       aes(x = month, y = bees_per_min, color = nest_ID)) +
  geom_jitter(
    width = 0.15,
    alpha = 0.4
  ) +
  #stat_summary(
    #aes(group = nest_ID),
    #fun.min = min,
    #fun.max = max,
   # geom = "linerange",
   # linewidth = 1
 # ) +
  labs(
    x = "Month",
    y = "Bees per minute",
    color = "Nest ID"
  ) +
  theme_minimal() 

ggplot(all_noPollen, aes(x = date, y = bees_per_min)) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  #stat_summary(
    #fun.min = min,
    #fun.max = max,
    #geom = "linerange",
    #linewidth = 1
  #) +
  facet_wrap(~ nest_ID) +
  labs(
    x = "Date",
    y = "Bees per minute",
    title = "Bees per minute per nest over time"
  ) +
  theme_minimal() #Shows each nest as its own plot with date on the x and bees per min on the y 

ggplot(nest_range, aes(x = nest_ID)) +
  geom_linerange(
    aes(ymin = min_bees, ymax = max_bees),
    linewidth = 1
  ) +
  geom_point(
    aes(y = avg_bees),
    size = 3
  ) +
  labs(
    x = "Nest ID",
    y = "Bees per minute",
    title = "Average bees per minute per nest"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) #Shows min bees, max bees, avg bees per minute per nest without date

ggplot(nest_range, aes(x = nest_ID)) +
  geom_point(
    aes(y = min_bees),
    shape = 17,      # triangle
    size = 3
  ) +
  geom_point(
    aes(y = max_bees),
    shape = 18,     # diamond
    size = 3
  ) +
  geom_point(
    aes(y = avg_bees),
    shape = 16,     # filled circle
    size = 3
  ) +
  labs(
    x = "Nest ID",
    y = "Bees per minute",
    title = "Average bees per minute per nest"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) #Different shape per mean, minimum, and maximum

ggplot(nest_range, aes(x = nest_ID)) +
  geom_point(
    aes(y = min_bees, shape = "Minimum"),
    size = 3
  ) +
  geom_point(
    aes(y = avg_bees, shape = "Average"),
    size = 3
  ) +
  geom_point(
    aes(y = max_bees, shape = "Maximum"),
    size = 3
  ) +
  scale_shape_manual(
    name = "Bee count",
    values = c(
      "Minimum" = 15,  # square
      "Average" = 16,  # filled circle
      "Maximum" = 17   # triangle
    )
  ) +
  labs(
    x = "Nest ID",
    y = "Bees per minute",
    title = "Average bees per minute per nest"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) #Different size per average, min, max, with a legend. This feels more jumbled to me. 


####Does nest size impact the number of surveys required to find a nest? 
###Creating a detection matrix of surveys vs survey plots. 0 = no nest detected, 1 = nest detected. 
##How many times was a plot surveyed before a nest was discovered? 

table(nest_surveys$task) #Checking to see how things are listed in the task column

#Some quick cleaning to get rid of things we don't need to create the detection matrix
nest_surveys <- nest_surveys %>%
  filter(task != "Drive_Time",
         task != "Planning",
         task != "Radio_Tracking",
         task != "Volunteer_Communications_Training")

nest_surveys <- nest_surveys %>%
  select(-volunteer_search_program, -staff_search_program, -radio_tracking_program, -park)

nest_surveys <- nest_surveys %>%
  rename(.,
         plot = site,
         survey_ID = Entry)

nest_surveys <- nest_surveys %>%
  mutate(nest_found_num = case_when(
    nest_found == "No" ~ "0",
    nest_found == "Yes" ~ "1",
    TRUE ~ nest_found
  ))

write.csv(nest_surveys, "quests2425.csv", row.names = FALSE) #Saving nest_surveys as a CSV to try creating the matrix in excel

#Creating detection matrix 

detection_matrix <- nest_surveys %>%
  filter(task == "Nest_Search") %>%
  mutate(
    detected = if_else(nest_found == "Yes", 1, 0)
  ) %>%
  select(plot, survey_ID, detected) %>%
  pivot_wider(
    names_from = survey_ID,
    values_from = detected
  )


#detection_matrix2 <- nest_surveys %>%
 # filter(task == "Nest_Search") %>%
  #mutate(
   # date = make_date(year, month, day),
    #detected = if_else(nest_found == "Yes", 1, 0)
  #) %>%
  #group_by(plot, date) %>%
  #summarise(
   # detected = max(detected, na.rm = TRUE),
    #.groups = "drop"
  #) %>%
  #pivot_wider(
   # names_from = date,
    #values_from = detected,
    #values_fill = 0
  #) #This creates the matrix by date



