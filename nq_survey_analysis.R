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
  
### Bumble bee observations, catergorical data 
table(surveys_no_flower$bombus_obs) # first look at bumble bee observation data

bombus_summary <- surveys_no_flower %>% select(.,c('site',
                                                   'year',
                                                   'bombus_obs')) %>%
  drop_na(bombus_obs)

bombus_counts <- bombus_summary %>% 
  group_by(year, bombus_obs) %>% 
  tally()
  

#
ggplot(bombus_counts, aes(x = bombus_obs, y = n, fill = year )) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5, size = 12, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 14)) +
  ylim(0,40)+
  labs(x = "Bombus seen",
       y = "n") +
  scale_x_discrete(limits = c('0',
                              'less than 5',
                              '5 to 10',
                              '10 to 19',
                              '20 to 39',
                              'more than 40'))+ 
  facet_wrap(vars(year), ncol = 2, strip.position = 'top') 


### How many surveys for each site/plot?

site_summary <- surveys_no_flower %>% 
  group_by(site, Volunteer_or_staff, year) %>% 
  summarize(total_survey_sum = sum(survey_count, na.rm = TRUE),
            total_unique_observers = n_distinct(Primary.observer, na.rm = TRUE), .groups ="drop") # NOTE TO SAMI, OBSERVER COLUMN NEEDS CLEANED

#site_summary <- site_summary %>% 
 # complete(., site, Volunteer_or_staff, year, fill = list(Volunteer_or_staff = 'volunteer')) %>% 
#  mutate(., total_survey_sum = ifelse(is.na(total_survey_sum), 0, total_survey_sum))
  #drop_na(total_unique_observers)

site_summary2 <- full_join(nests, site_summary)

site_summary %>% 
  ggplot(aes(y = -desc(total_survey_sum), x = site, fill = Volunteer_or_staff)) + # defining x and y values. 'fill' is how to get the groupings based on a column. 
  geom_bar(stat = "identity", colour = 'black') +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.8, size = 12, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 14)) + 
  facet_wrap(vars(year), ncol = 2, scale = "free",
             strip.position = 'top')+  # separate data by year
  ylim(0,15) + # set scale manually
  labs(x = "Site", y = "Total Surveys") + #labels
  scale_fill_manual(values = c("#FFA500", "#56B4E9")) # changing color


# How many nests at each site/plot?
all_nests <- all_nests %>% 
  filter(., site != c(""))

nests <- all_nests %>% 
  group_by(site) %>% 
  count(., .drop = FALSE) %>% 
  rename(total_nests = n) 

total_nests <- sum(nests$total_nests)


                    