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


# How many nests at each site/plot?
all_nests <- all_nests %>% 
  filter(., site != c(""))

nests <- all_nests %>% 
  group_by(site) %>% 
  count(., .drop = FALSE) %>% 
  rename(total_nests = n) 

sites <- right_join(nests, sites) 
sites <- sites %>%  mutate(total_nests = ifelse(is.na(total_nests), 0, total_nests))

#total_nests <- sum(nests$total_nests)


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
site_summary2 <- site_summary2 %>% filter(., site != "crow_gc")


plot_labels <- site_summary2 %>% distinct(site) 

plot_labels %>% mutate(nest = if_else(site_summary2$total_nests > 1, 'yes', 'no'))

df <- data.frame(site = distinct(site_summary2, site),
                 nest = c('yes', 'yes', 'yes','yes', 'yes', 'yes', 'yes', 'yes' ,'no', 'no', 'no', 'no', 'no', 'no', 'no', 'no', 'no'),
                 x = c())

site_summary2

png("total_NQ_surveys_bar.png", units="in", width=12, height=10, res=1200)
site_summary2 %>% 
  ggplot(aes(y = -desc(total_survey_sum), x = site, fill = Volunteer_or_staff, label = total_nests)) + # defining x and y values. 'fill' is how to get the groupings based on a column. 
  geom_bar(stat = "identity", colour = 'black') +
  theme(axis.text.x = element_blank(),
    #axis.text.x = element_text(angle = 50, vjust = 0.8, size = 16, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 18, color = 'black'),
        axis.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = 'seashell2', color = '#4D4D4D'),
        legend.text = element_text(size = 18),
        legend.key.size = unit(1, 'cm')) + 
  facet_wrap(vars(year), ncol = 2, scale = "free",
             strip.position = 'bottom') +  # separate data by year
  scale_y_continuous(breaks = seq(0, 16, by = 2)) + # set scale manually
  labs(x = "", y = "Total Surveys") + #labels
  scale_fill_manual(values = c("#FFA500", "#56B4E9"), 
                    labels = c("Staff", "Volunteer"),
                    guide = guide_legend(title = NULL)) + # changing color
  geom_hline(yintercept = 5, linetype ="dashed", size = 1) 
  #annotate("text", x = 2, y = 17, label = "!!", size = unit(8, "pt")) +
  #geom_text(aes(label = ifelse(total_nests > 0, "!!", "" & Volunteer_or_staff == 'Staff'), group = site))
  #geom_text(aes(label = total_nests))
dev.off()






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



                    