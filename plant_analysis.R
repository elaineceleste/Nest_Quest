# Nest Quest Plant Preliminary Data # SD 24/4/2025

#load packages
library(tidyverse)
library(lubridate)

#load data
plants <- read.csv("NQ_plants_data.csv")

#classifying variables
plants$site <- as.factor(plants$site)
plants$nest_ID <- as.factor(plants$nest_ID)
plants$plant_name <- as.factor(plants$plant_name)
plants$date <- as.Date(plants$date, format = "%m_%d_%Y") #convert to date
#plants = separate(data = plants, col = "date", into = c("day","month","year"),  sep = "_", remove = FALSE)
plants$stem_count <- as.integer(plants$stem_count) #error where "no count" is converted to NA
plants$stem_count <- plants$stem_count %>% replace(is.na(.), 0) #turning those NA's into 0's


#removing unessecary columns for simplicity
plants <- plants %>% select(-c('field_name', 'needs_id', 'Identifer'))


plants$plant_name[plants$plant_name==""] <- NA #convert blank values for to NA. These are plants with unknown identity. 


summary(plants) #check


ggplot(plants, aes(y = plant_name, x = stem_count)) +
  geom_bar(stat = "identity", position = "dodge") # just looking at all plants frequency


# Looking at plant communities by nest
 nest_plants_summary <- plants %>% group_by(nest_ID) %>% summarize(total_species = n_distinct(plant_name),
                                            total_stems = sum(stem_count)) 
  