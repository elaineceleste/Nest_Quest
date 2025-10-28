# Nest Quest Survey123 Survey Data # SD start Oct 28 2025


#load packages
library(tidyverse) #dplyr package
library(lubridate)

#load data
surveys <- read.csv('quest_survey123_data.csv', header = T, na.strings = c("","NA"))
sites <- read.csv('nest_survey_site_data.csv')
all_nests <- read.csv('nest_discovery.csv')

# a little cleaning
surveys_no_flower <- surveys %>% # removing rows with flower data for now. Just to focus on bees mainly
  select(., -c(Currently.blooming.plant..incl..trees.and.shrubs..and...of.area.covered.:Currently.blooming.plant..incl..trees.and.shrubs..and...of.area.covered..9))


surveys_no_flower <- surveys_no_flower %>% # changing names of giant column names
  rename(bloom_area = How.much.of.the.area.is.covered.in.currently.blooming.flowers.,
         bombus_obs = How.many.times.did.you.see.bumble.bees.during.your.survey.,
         AQI = What.is.air.quality.index..Visit.https...www.airnow.gov.,
         volunteer_hours = How.many.hours.did.you.spend.volunteering..including.preparation.and.driving.time..Enter.as.a.decimal.number.to.the.closest.15.minutes..i.e..1.75.for.one.hour.and.45.minutes)

table(surveys_no_flower$site) # check sites
surveys_no_flower <- surveys_no_flower %>% # removing invalid rows based on site
  filter(., site != c("Crow hassan", "Hyand queen search", "Queen searching Hyland")) # for some reason this needs to run twice to work?

# replacing site values so all match
surveys_no_flower$site[surveys_no_flower$site == "Hp1"] <- 'hp1' 
surveys_no_flower$site[surveys_no_flower$site == 'LH_eq2'] <- 'lh_eq2'
surveys_no_flower$site[surveys_no_flower$site == 'HP_sled'] <- 'hp_sled'
table(surveys_no_flower$site) #check
  
table(surveys_no_flower$bombus_obs) # first look at bumble bee observation data

# How many surveys for each site/plot?

site_summary <- surveys_no_flower %>% 
  group_by(site, Volunteer_or_staff, year) %>% 
  summarize(total_survey_sum = sum(survey_count, na.rm = TRUE),
            total_unique_observers = n_distinct(Primary.observer, na.rm = TRUE)) # NOTE TO SAMI, OBSERVER COLUMN NEEDS CLEANED

# How many nests at each site/plot?
all_nests <- all_nests %>% 
  filter(., site != c(""))

nests <- all_nests %>% 
  group_by(site) %>% 
  count(., .drop = FALSE) %>% 
  rename(total_nests = n) 

 total_nests <- sum(nests$total_nests)

site_summary2 <- full_join(nests, site_summary)

site_summary %>% 
  ggplot(aes(y = total_survey_sum, x = -desc(site))) + # defining x and y values. 'fill' is how to get the groupings based on a column. 
  geom_bar(stat = "identity") 
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5), #rotate x-axis text
        panel.grid.major = element_blank()) + 
  facet_wrap(vars(year), ncol = 2, scale = "free") + # separate data by year
  ylim(0,5)+ # set scale manually
  labs(x = "nest_ID", y = "mean bare ground cover") + #labels
  scale_fill_manual(values = c("purple", "pink", "blue")) # changing color


                    