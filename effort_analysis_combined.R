## June 2, 2026
#Combining data for applied economist analysis
  #Combining survey data and staff effort data into one sheet formatted for Terry's analysis

#load packages
library(tidyverse)
library(lubridate)

#load data
surveys <- read.csv(staff_volunteer_surveys_2425cleaned.csv)
staff_effort <- read.csv(staff_effort2425.csv)
