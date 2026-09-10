## June 2, 2026
#Combining data for applied economist analysis
  #Combining survey data and staff effort data into one sheet formatted for Terry's analysis

#load packages
library(tidyverse)
library(lubridate)
library(hms)

#load data
surveys <- read.csv("staff_volunteer_surveys_2425cleaned.csv")
staff_effort <- read.csv("staff_effort2425.csv")

#Cleaning up surveys first. Start with filling in NAs
surveys <- surveys %>%
  mutate(
    time_spent = if_else(
      is.na(end_timeCST),
      "1:00:00",
      as.character(time_spent)
    )
  ) #adds in 1 hour survey time for surveys with "NA" end time 

surveys <- surveys %>%
  mutate(
    time_spent = if_else(
      is.na(time_spent),
      "1:00:00",
      time_spent
    )
  ) #adds in 1 hour survey time for surveys counted as 1 that had "NA" in time_spent column

#Changing the total time to display as decimal format, rather than hh:mm:ss format
surveys <- surveys %>%
  mutate(
    total_time = as.numeric(as_hms(time_spent)) / 3600
  )

#splitting date into columns for month, day, and year
surveys <- surveys %>%
  separate(
    date,
    into = c("month", "day", "year"),
    sep = "/",
    remove = FALSE)

#creating a new column to indicate the task
surveys <- surveys %>%
  mutate(
    task = case_when(
      Volunteer_or_staff == "staff" ~ "staff nest searching",
      Volunteer_or_staff == "volunteer" ~ "volunteer nest searching",
      TRUE ~ NA_character_
    )
  )

#Renaming observer1 column
surveys <- surveys %>%
  rename(name = observer1)

#Finalizing clean data frame with only the details we need for the final join
surveys_clean <- surveys %>%
  select(site, date, day, month, year, Volunteer_or_staff, name, total_time, task)

surveys_clean <- surveys_clean %>%
  rename(volunteer_or_staff = Volunteer_or_staff)

surveys_clean <- surveys_clean %>%
  mutate(name = case_when(
    name == "Mathew Zappa" ~ "Rose Zappa",
    .default = as.character(name)
  ))
  

#Onto staff_effort data cleaning
#Starting by making sure each row represents one individual
staff_effort <- staff_effort %>%
  mutate(row_id = row_number()) %>% #Creating row id for each row 
  separate_rows(staff_name, sep = ", ") %>% #separates row based on presence of , to give each staff member their own row
  group_by(row_id) %>%
  mutate(
    n_people = n(), 
    total_time = total_time / n() #divides total_time value by number of people originally represented in that row 
  ) %>%
  ungroup() %>%
  select(-row_id)

#Changing names from initials
staff_effort <- staff_effort %>%
  mutate(staff_name = case_when(
    staff_name == "EE" ~ "Elaine Evans",
    staff_name == "EB" ~ "Elise Bernstein",
    staff_name == "CC" ~ "Clara Costello",
    staff_name == "cc" ~ "Clara Costello",
    staff_name == "JP" ~ "Jayden Peterson",
    staff_name == "jp" ~ "Jayden Peterson",
    staff_name == "SD" ~ "Sami Dolan",
    staff_name == "FL" ~ "Fern LaBry",
    staff_name == "EA" ~ "Emma Ambrosi",
    staff_name == "ea" ~ "Emma Ambrosi",
    staff_name == "KM" ~ "Kevin McKinney", #need to mark as volunteer nest observation 
    staff_name == "MC" ~ "Maria Ciencias", #need to mark as volunteer nest observation 
    startsWith(staff_name, "Mathew") ~ "Rose Zappa",
    startsWith(staff_name, "Elise") ~ "Elise Bernstein",
    startsWith(staff_name, "Clara") ~ "Clara Costello",
    .default = as.character(staff_name)
  ))

table(staff_effort$staff_name) #Check to make sure all initials have been changed to full names 

#creating separate data frame that only has habitat assessment data
hab_assess <- staff_effort %>%
  select(-c(time_as_reported, corrected_time, people, corrected_total_time, X, X.1, X.2, X.3, X.4, n_people))

hab_assess <- hab_assess %>%
  filter(task == "Habitat assessment")

#Removing Nest searching rows from effort sheet. This effort is is accounted for in the "surveys" data frame which will be combined. 
#And removing habitat assessment, nest observation, nest excavation time. This time will not be included in the final cost analysis, since it does not count towards nest searching or radio tracking time. 
staff_effort_clean <- staff_effort %>%
  filter(task != "Nest searching",
         task != "Habitat assessment",
         task != "Nest observation",
         task != "Nest excavation")

#Removing unncessary columns from staff_effort_clean
staff_effort_clean <- staff_effort_clean %>%
  select(-c(time_as_reported, corrected_time, people, corrected_total_time, X, X.1, X.2, X.3, X.4, n_people))

#Fixing date values 
staff_effort_clean <- staff_effort_clean %>%
  rename(day = date.1)

staff_effort_clean <- staff_effort_clean %>%
  mutate(year = as.character(year)) #making year a character so I can update from 24 to 2024/25 to 2025

staff_effort_clean <- staff_effort_clean %>%
  mutate(
    year = case_when(
      year == "24" ~ "2024",
      year == "25" ~ "2025",
      TRUE ~ year
    )
  )

staff_effort_clean <- staff_effort_clean %>%
  mutate(day = as.character(day)) #making day a character so data frames can be joined

#Renaming "staff_name" to "name"
staff_effort_clean <- staff_effort_clean %>%
  rename(name = staff_name)

#Joining data frames together 
combined_effort <- full_join(surveys_clean, staff_effort_clean)

#Tidying up new combined_effort before adding in additional columns
#Start by filling in volunteer or staff

#Create list of staff members 
staff_list <- c("Sarah Henderson", "???", "Clara Costello", "Elaine Evans", "Elise Bernstein", "Emma Ambrosi", "Fern LaBry", "Jayden Peterson", "Jessica Schatz", "Renee Rademacher", "Rose Zappa", "Sami Dolan")

#Make blanks true NA values 
combined_effort <- combined_effort %>%
  mutate(
    name = trimws(name),
    name = na_if(name, "")
  )

#Now fill in values for staff, else volunteer. Includes blanks as staff  
combined_effort <- combined_effort %>%
  mutate(
    volunteer_or_staff = case_when(
      is.na(volunteer_or_staff) &
        (name %in% staff_list | is.na(name)) ~ "staff",
      is.na(volunteer_or_staff) ~ "volunteer",
      TRUE ~ volunteer_or_staff
    )
  )
  

#Create list of volunteer search program tasks, staff search program tasks, and radio tracking tasks 
#RUN THIS TWICE???? 
volunteer_prog_tasks <- c("Training", "Website building", "Volunteer communications", "Site selection", "volunteer nest searching")
staff_prog_tasks <- c("staff nest searching", "Scheduling/planning", "Data entry", "Data analysis", "Staff travel")
radio_tasks <- c("Radio tracking")

#Creating three new columns: volunteer search program, nest search program, and radio tracking
##RUN THIS TWICE????
combined_effort <- combined_effort %>%
  mutate(
    volunteer_search_program = if_else(task %in% volunteer_prog_tasks, "yes", "no"),
    staff_search_program = if_else(task %in% staff_prog_tasks, "yes", "no"),
    radio_tracking_program = if_else(task %in% radio_tasks, "yes", "no")
  )
  

#Updating some rows 
#Starting with drive time values. If "Radio" is indicated in the drive time column, indicate yes in radio_tracking_program column
#combined_effort <- combined_effort %>%
 # mutate(
  # radio_tracking_program = case_when(
   #  task == "Drive time" & grepl("Radio", notes %||% "") ~ "yes",
   #  TRUE ~ "no"
   #)) #This voids radio_tracking_program above, so need to figure something else out

#Update site for radio tracking
combined_effort <- combined_effort %>%
  mutate(
    site = if_else(task == "Radio tracking", "Arden Hills", site)
  )



  