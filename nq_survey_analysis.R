# Nest Quest Survey123 Survey Data # SD start Oct 28 2025


#load packages
library(tidyverse) #dplyr package
library(lubridate) #dealing with time and dates
library(readr) # creating .csv files
library(lme4) # GLM models
library(gridExtra) # for creating plot grids
library(grid)

##### Cleaning and loading in data #####
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
         volunteer_hours = How.many.hours.did.you.spend.volunteering..including.preparation.and.driving.time..Enter.as.a.decimal.number.to.the.closest.15.minutes..i.e..1.75.for.one.hour.and.45.minutes,
         observer1 = Primary.observer,
         observer2 = Secondary.observer)

table(surveys_no_flower$site) # check sites

### RUN THIS TWICE (IDK WHY BUT THATS WHAT YOU HAVE TO DO)
surveys_no_flower <- surveys_no_flower %>% # removing invalid rows based on site
  filter(., site != c("Crow hassan", "Hyand queen search", "Queen searching Hyland")) # for some reason this needs to run twice to work?

surveys_no_flower$year <-as.character(surveys_no_flower$year)  # making year a character 
surveys_no_flower$survey_count <-as.numeric(surveys_no_flower$survey_count) # making survey count numeric

# replacing site values so all match
surveys_no_flower$site[surveys_no_flower$site == "Hp1"] <- 'hp1' 
surveys_no_flower$site[surveys_no_flower$site == 'LH_eq2'] <- 'lh_eq2'
surveys_no_flower$site[surveys_no_flower$site == 'HP_sled'] <- 'hp_sled'
table(surveys_no_flower$site) #check

# cleaning  primary observer names. They are all over the place!
table(surveys_no_flower$observer1)
surveys_no_flower <- surveys_no_flower %>% 
  mutate(observer1 = case_when(startsWith(observer1, "Clara") ~ "Clara Costello",
                               startsWith(observer1, "Elaine") ~ "Elaine Evans",
                               startsWith(observer1, 'Fern') ~ "Fern LaBry",
                               startsWith(observer1, 'Alex') ~ "Alex Swartz",
                               startsWith(observer1, 'Dawn') ~ "Dawn Peterson",
                               startsWith(observer1, 'Hanych') ~ "Emily Hanych",
                               startsWith(observer1, 'Janet') ~ "Janet Orange",
                               startsWith(observer1, 'Paul') ~ "Paul Richtman", #two pauls, one without a last name. assuming it's the same person.
                               startsWith(observer1, 'Renee') ~ "Renee Rademacher",
                               grepl('Tam', observer1) ~ "Tamara Smith",
                               grepl('ayden', observer1) ~ 'Jayden Peterson',
                               startsWith(observer1, "Elise") ~ "Elise Bernstein",
                               startsWith(observer1, "Tom") ~ "Tom Patterson",
                               .default = as.character(observer1)))

table(surveys_no_flower$observer1) #check. looks good to me

# cleaning survey_count data. 
table(surveys_no_flower$survey_count) # there are inconsistent values for repeating 0.3.
surveys_no_flower <- surveys_no_flower %>% 
  mutate(survey_count = case_when(startsWith(as.character(survey_count), "0.3") ~ "0.3", 
                                  .default = as.character(survey_count)))
table(surveys_no_flower$survey_count)

surveys_no_flower <- surveys_no_flower %>%  filter(!row_number() %in% c(149)) # removing a row where data is missing. This row seems to a survey that was split up, but the second part of the survey happened the next day??

#cleaning time spent column
surveys_no_flower <- surveys_no_flower %>% mutate(time_spent = ifelse(is.na(time_spent), '1:00:00', time_spent)) # assuming NA values in this column are just an hour of surveying. 

##### First look summary stats and exploration ####
all_nests <- all_nests %>% filter(., site != c("")) %>% 
              mutate(separate(., col = "date", into = c("month","day","year"),  sep = "/")) %>% 
              rename(found_on_survey = Did.you.discover.this.nest.during.your.one.hour.survey.time.,
                     position = where.was.the.nest.located.,
                     total_entrances = how.many.entrances.do.you.see.being.used.,
                     nest_material = what.nest.materials.were.being.used.) 

all_nests_selected <- all_nests %>% 
  select(., nest_ID, found_on_survey, species, position, date, day, month, year, time, site) %>% 
  mutate(., position = ifelse(position == 'on the surface of the ground', 'surface', 'underground'))
#write.csv(all_nests_selected, "C:\\Users\\chand\\OneDrive\\Documents\\data\\nestquest\\nestquest\\nest_species_summary.csv") 

nest_species <- all_nests_selected %>%group_by(species, position) %>%  summarize(count = n_distinct(nest_ID))


nest_species_survey_only <- all_nests_selected %>% filter(., found_on_survey == 'yes') %>% group_by(species, position) %>% summarize(count = n_distinct(nest_ID))
 

ggplot(nest_species, aes(x = factor(species, levels = c('impatiens',
                                                        'griseocollis',
                                                        'rufocinctus',
                                                        'auricomus',
                                                        'fervidus')), y = count, fill = position )) +
  geom_bar(stat = 'identity', color = 'black') + 
  theme(
        axis.text.x = element_text(angle = 0, vjust = 0.8, size =12, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.title = element_text(size =16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 12), 
        legend.position = c(0.8, 0.6)) +
  scale_fill_manual(values = c( '#9ACD32', '#CDAA7D'),
                    guide = guide_legend(title = NULL)) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  labs(x = '', y = 'Number of nests')


ggplot(nest_species_survey_only, aes(x = factor(species, levels = c('impatiens',
                                                        'griseocollis',
                                                        'rufocinctus',
                                                        'auricomus',
                                                        'fervidus')), y = count, fill = position )) +
  geom_bar(stat = 'identity', color = 'black') + 
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.8, size =12, color = 'black'),#rotate x-axis text
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.title = element_text(size =16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks.x = element_blank(),
    legend.text = element_text(size = 12)) +
  scale_fill_manual(values = c( '#9ACD32', '#CDAA7D'),
                    guide = guide_legend(title = NULL)) +
  scale_y_continuous(breaks = seq(0, 8, by = 2)) +
  ylim(0,8) +
  labs(x = 'Bombus species', y = 'Number of nests')


# How many nests at each site/plot?
nests <- all_nests %>% 
  group_by(site) %>% 
  count(., .drop = FALSE) %>% 
  rename(total_nests = n) # counting the number of nests at each site.
nests

sites <- right_join(nests, sites) 
sites <- sites %>%  mutate(total_nests = ifelse(is.na(total_nests), 0, total_nests)) # joining nest data to match sites. This is a nice summary table

### How many surveys for each site/plot?

site_summary <- surveys_no_flower %>% 
  group_by(site, 
          Volunteer_or_staff, 
           year) %>% # organize surveys by site, year, and volunteer/staff
  summarize(total_survey_sum = format(round(sum(as.numeric(survey_count), na.rm = TRUE), digits = 4)), # survey totals
            total_unique_observers = n_distinct(observer1, na.rm = TRUE), .groups ="drop") # total unique surveyors for each plot. 
site_summary$total_survey_sum <- as.numeric(site_summary$total_survey_sum)
site_summary

#site_summary <- site_summary %>% 
 # complete(., site, Volunteer_or_staff, year, fill = list(Volunteer_or_staff = 'volunteer')) %>% 
#  mutate(., total_survey_sum = ifelse(is.na(total_survey_sum), 0, total_survey_sum))
  #drop_na(total_unique_observers)

site_summary2 <- full_join(nests, site_summary) # note that total_nests column does not represent nests differentiated by surveyor type
site_summary2 <- site_summary2 %>% mutate(total_nests = ifelse(is.na(total_nests), 0, total_nests)) #NA's in nest column to 0
site_summary2 <- site_summary2 %>% filter(., site != "crow_gc")
site_summary2

##### Plotting Survey amounts by year, and filled with volunteer or staff surveys. #####
#png("total_NQ_surveys_bar_no_hline.png", units="in", width=12, height=10, res=1200) #uncomment this to create a png
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
                    guide = guide_legend(title = NULL))  # changing color
  # geom_hline(yintercept = 5, linetype ="dashed", size = 1) 
  #annotate("text", x = 2, y = 17, label = "!!", size = unit(8, "pt")) +
  #geom_text(aes(label = ifelse(total_nests > 0, "!!", "" & Volunteer_or_staff == 'Staff'), group = site))
  #geom_text(aes(label = total_nests))
#dev.off() #uncomment this to create a png

surveys_2024 <- site_summary2 %>% filter(., year == '2024')%>% 
  ggplot(aes(y = -desc(total_survey_sum), x = site, fill = Volunteer_or_staff, label = total_nests)) + # defining x and y values. 'fill' is how to get the groupings based on a column. 
  geom_bar(stat = "identity", colour = 'black') +
  theme(#axis.text.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust =  1.25, vjust = 0, size = 12, color = 'black'),
        axis.text.y = element_text(size = 18, color = 'black'),
        axis.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = 'seashell2', color = '#4D4D4D'),
        legend.text = element_text(size = 18),
        legend.key.size = unit(1, 'cm'), 
        legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 16, by = 2)) + # set scale manually
  labs(x = "2024", y = "Total Surveys") + #labels
  scale_fill_manual(values = c("#FFA500", "#56B4E9"), 
                    labels = c("Staff", "Volunteer"),
                    guide = guide_legend(title = NULL)) + # changing color
  annotate('text', x = c(2, 3, 5, 8), y = c(16.5, 13.5, 7.5, 10.5), label = '!!', size = 8)

 surveys_2025 <- site_summary2 %>% filter(., year == '2025')%>% 
  ggplot(aes(y = -desc(total_survey_sum), x = site, fill = Volunteer_or_staff, label = total_nests)) + # defining x and y values. 'fill' is how to get the groupings based on a column. 
  geom_bar(stat = "identity", colour = 'black') +
  theme(#axis.text.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1  , vjust = 0, size = 8.5, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 18, color = 'black'),
        axis.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = 'seashell2', color = '#4D4D4D'),
        legend.text = element_text(size = 18),
        legend.key.size = unit(1, 'cm'),
        legend.position = 'none') + 
  scale_y_continuous(breaks = seq(0, 16, by = 2)) + # set scale manually
  labs(x = "2025", y = "") + #labels
  scale_fill_manual(values = c("#FFA500", "#56B4E9"),
                    labels = c("Staff", "Volunteer"),
                    guide = guide_legend(title = NULL)) + # changing color
  annotate('text', x = c(2, 5, 6, 9, 10, 11, 14, 15), y = c(8, 6.5, 7.75, 7.75, 4.5, 7.75, 12.5, 16.5), label = '!!', size = 8)

grid.arrange(surveys_2024, surveys_2025, ncol = 2)


##### Bumble bee observations, categorical data #####
table(surveys_no_flower$bombus_obs)# first look at bumble bee observation data

bombus_summary <- surveys_no_flower %>%
  select(.,c('site',
             'year',
             'bombus_obs')) %>%
  drop_na(bombus_obs)

bombus_counts <- bombus_summary %>% 
  group_by(year, site, bombus_obs) %>% 
  tally()

## Bar chart of total bumble observations by category
ggplot(bombus_counts, aes(x = bombus_obs, y = n)) +
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
                              'more than 40')) +
  scale_y_continuous() +
  facet_wrap(vars(year), ncol = 2, strip.position = 'top') 


## bar chart of each plots bumble bee observatoins
ggplot(bombus_counts, aes(x = site, fill = bombus_obs, y = n)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5, size = 12, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 14)) +
  ylim(0,8)+
  labs(x = "Bombus seen",
       y = "n") 
  scale_x_discrete(limits = c('0',
                              'less than 5',
                              '5 to 10',
                              '10 to 19',
                              '20 to 39',
                              'more than 40')) +
  scale_y_continuous() +
  facet_wrap(vars(year), ncol = 2, strip.position = 'top') 


##### Looking at how various variables affect nest detection #####
# Mainly interested in how the categorical variable of bumble bee abundance affects nest detection
# Does the amount of bumble bees we saw on a survey affect our detection probability? Or occurrence probability? 

#each row in this data frame represents ONE survey
survey_nest <- surveys_no_flower %>% select(., "year", 
                                            'date', 
                                            'site', 
                                            'time_spent',
                                            'survey_count',
                                            'Volunteer_or_staff', 
                                            'observer1',
                                            'bombus_obs',
                                            'bloom_area',
                                            'GlobalID') # picking variables of interest for analysis. 

all_nests <- all_nests %>% rename(observer1 = primary_observer) # changing 


# I want to create a binary column in survey_nest that signifies whether a nest was found during that survey or not.
# I think I can do this by taking shared columns in survey_nest and all_nests, and combining them to make a unique ID of sorts.
# date:site:observer1 SHOULD result in unique enough values to compare.
# when there is a match between the two columns, R will say TRUE. If TRUE, a nest was found. If FALSE, a nest was not found. 

#creating unique values to be matched between data frames
survey_nest$date_site_observer1 <- as_factor(paste(survey_nest$date,":", survey_nest$site,":", survey_nest$observer1))
all_nests$date_site_observer1 <- as_factor(paste(all_nests$date,":", all_nests$site,":", all_nests$observer1))

all_survey_nests <- all_nests %>% filter(., found_on_survey == 'yes') # only selecting rows where nest was found during survey

survey_nest$nest_found <- match(survey_nest$date_site_observer1, all_survey_nests$date_site_observer1,  nomatch = 0) # match data frames, giving a 0 when there is no match meaning no nest was found.
survey_nest <- survey_nest %>% mutate(., nest_found = ifelse(nest_found > 0, 1, 0)) # make values a binary 0:1 
     
survey_nest[survey_nest$nest_found == '1', ]
survey_nest[191, 12] = 0 # Changing one specific value based on position. There is one incident of Jayden having two surveys (8/5/2025 and hp1) with the same date and location. 
               # she found a nest in the first half of her survey, then stopped and started a new survey. R thinks a nest was found in both of these, when really it was just the first one
                  # PSA: code as I suggest, not as I do. I'm crossing my fingers this little code won't cause problems later lol
table(survey_nest$nest_found) # 0 = nest not found during survey, 1 = nest found during survey (There should be 12 nests found).     
survey_nest$survey_count <-as.numeric(surveys_no_flower$survey_count) 
survey_nest$nest_found <- as.factor(survey_nest$nest_found)

survey_nest$location <- sites$location[match(survey_nest$site, sites$site)] # adding location to each row. 

##### Some summary analysis using this survey_nests. Lots of this is used for economist summary ##### #####
# nest finding success rate of volunteers

volunteer_nests <- survey_nest %>% 
  filter(., Volunteer_or_staff == "volunteer") # 110 total volunteer surveys

tally(volunteer_nests, nest_found == '1') / 110 # volunteer survey success rate is 0.018 nests/survey

volunteer_count <- survey_nest %>% filter(., Volunteer_or_staff == 'volunteer') %>% count(observer1, Volunteer_or_staff)
table(volunteer_count$n)
ggplot(volunteer_count, aes(x=n)) + 
  geom_histogram(binwidth = 1, fill = 'slateblue', color = 'black') +
  scale_y_continuous(breaks = c(1:12)) +
  scale_x_continuous(breaks = c(1:13)) + 
  annotate('text', x = 13, y = 1.5, label = 'Dawn Peterson', angle = 0, size = 3) +
  annotate('text', x = 10, y = 1.5, label = 'Kevin McKinney', size = 3) +
  annotate('text', x = 8, y = 2.5, label = 'Maria Cicenas and Tom Patterson', size = 3) +
  ylab('n') + xlab('Number of Surveys') +
  theme(#panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
        #panel.background = element_blank())

# nest finding success rate of staff members
staff_nests <- survey_nest %>% 
  filter(., Volunteer_or_staff == "staff") # 154 total staff surveys

tally(staff_nests, nest_found == '1') / 154 # staff survey success rate is 0.058 nest/survey

survey_nest %>% group_by(site, year) %>% summarize(sum = sum(survey_count, na.rm = TRUE))

survey_success_summary <- survey_nest %>% count(year, nest_found, Volunteer_or_staff) # summary of success by year and observer type
survey_nest %>% count(year,
                      site,
                      nest_found) %>% filter(.,nest_found == 1) # how many nests found per site/year combo? I entered this into the economist spreadsheet. 

## Total time spent surveying as recorded by survey123
survey_nest$time_spent_hms <- hms(survey_nest$time_spent) # make column hms format
survey_nest$time_spent_seconds <- period_to_seconds(survey_nest$time_spent_hms) # convert to seconds
survey_nest$time_spent_hours <- round(survey_nest$time_spent_seconds/3600, digits = 2)

survey_nest %>% 
  mutate_at(vars(time_spent_seconds), ~replace_na(., 3600)) %>% 
  group_by(., year, Volunteer_or_staff) %>% 
  summarize(., total_time_seconds = sum(time_spent_seconds),    # summing seconds spent surveying
            total_hours = total_time_seconds/3600,              # convert seconds to hours
            survey_count_sum = sum(survey_count, na.rm = TRUE)) # totals from survey_count column

time_per_site <- survey_nest %>% 
  mutate_at(vars(time_spent_seconds), ~replace_na(., 3600)) %>% 
  group_by(., site,year) %>% 
  summarize(., total_time_seconds = sum(time_spent_seconds),
            total_hours = total_time_seconds/3600) 

time_per_site <- time_per_site %>% pivot_wider(names_from = year, 
                              values_from = c(total_hours), 
                              values_fill = 0)

time_summary <- survey_nest %>% 
  group_by(year,Volunteer_or_staff) %>% 
  summarize(., hours = sum(time_spent_hours),
               total_unique_observers = n_distinct(observer1, na.rm = TRUE), .groups ="drop")

ggplot(time_summary, aes(x = year, y = hours, fill = Volunteer_or_staff)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  annotate('text', x = 0.77, y = 66, label = '8', angle = 0, size = 5.5, color = 'white') +
  annotate('text', x = 1.22, y = 42, label = '11', angle = 0, size = 5.5, color = 'white') +
  annotate('text', x = 1.77, y = 53, label = '7', angle = 0, size = 5.5, color = 'white') +
  annotate('text', x = 2.23, y = 73, label = '27', angle = 0, size = 5.5, color = 'white') +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 12, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 14)) +
  labs(x = "Year",
       y = "Number of hours") +
  scale_y_continuous(
    limits = c(0, 80),
   breaks = seq(0, 80, by = 15)
    ) +
  scale_fill_manual(values = c("#FFA500", "#56B4E9"), 
                      labels = c("Staff", "Volunteer"),
                      guide = guide_legend(title = NULL)) 

#write.csv(time_per_site, "C:\\Users\\chand\\Documents\\data\\nestquest\\nestquest\\time_per_site2.csv")

# calculating pace of surveys
survey123_tracks <- read.csv('cleaned_track_lengths.csv')
survey123_tracks <- survey123_tracks %>% select(.,
                            'globalid',
                            'length') %>% 
  rename(GlobalID = globalid,
         length_meters = length) %>% na.omit() #removing lines that don't have a track recorded before joining.

survey_nest <- left_join(survey_nest, survey123_tracks) %>% # join track length to main analysis dataframe
  mutate(length_meters = if_else(duplicated(GlobalID), NA, length_meters))  #removing duplicated values in length_meters upon matching. Some lengths were duplicated upon joining, despite no track being recorded

survey_nest$length_meters <- round(survey_nest$length_meters, digits = 2) # make values a bit more readable. 

survey_nest <- survey_nest %>% 
  mutate(., pace_meters_per_hour = length_meters / time_spent_hours, # create meters/hour pace
        pace_meters_per_hour = replace(pace_meters_per_hour, which(pace_meters_per_hour > 3500), NA)) # Removing a single outlier of Elise data where it seems unrealistic. 
# some other outliers are contained here^^^

pace_data <- survey_nest %>% drop_na(length_meters)

pace_summary <- survey_nest %>% 
  group_by(., year, Volunteer_or_staff) %>% 
  summarize(mean_pace = mean(pace_meters_per_hour, na.rm = TRUE,),
            mean_length = mean(length_meters, na.rm = TRUE)) 



ggplot(survey_nest, aes(x = Volunteer_or_staff, y = pace_meters_per_hour)) + 
  geom_boxplot(fill= "slateblue", alpha= 0.7) +
  xlab('') +
  ylab('Pace (meters/hour)') +
  scale_x_discrete(labels = c("Staff", "Volunteer")) +
  theme(axis.text.x = element_text(vjust = 0.5, size = 12, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 14)) +
  facet_wrap(vars(year), ncol = 2, strip.position = 'top') 


#
observer_surveys <- survey_nest %>% count(observer1, sort = TRUE)
observer_summary <- survey_nest %>% 
  group_by(., 
           observer1,
           Volunteer_or_staff) %>% 
  summarize(.,
            total_hours = sum(time_spent_hours),
            total_survey_sum = sum(survey_count, na.rm = TRUE))
            
observer_summary <- left_join(observer_summary, observer_surveys) 
observer_summary <- observer_summary %>% rename(total_unique_surveys = n)

observer_nests <- survey_nest %>% 
  count(observer1, nest_found) %>% 
  filter(nest_found == 1) %>%
  select(., 'observer1', 'n') 

observer_summary <- left_join(observer_summary, observer_nests) %>% 
  rename(total_nests = n) %>% 
  mutate(total_nests = ifelse(is.na(total_nests), 0, total_nests))

#write.csv(observer_summary, "C:\\Users\\chand\\OneDrive\\Documents\\data\\nestquest\\nestquest\\observer_summary.csv")


site_unique_surveys<- survey_nest %>% count(site, year, sort = TRUE)
#write.csv(site_unique_surveys, "C:\\Users\\chand\\OneDrive\\Documents\\data\\nestquest\\nestquest\\site_unique_surveys.csv")
  
##### Survey success analysis using the binary variable 0 and 1 of nest found #####

# NULL: Volunteer_or_staff DOES NOT affect if a nest is found during a survey.
# ALT: Volunteer_or_staff DOES not affect if a nest is found during a survey.
observer_cont_table <- print(table(survey_nest$nest_found, survey_nest$Volunteer_or_staff)) # create contingency table
#chi_observer_nest_found <- chisq.test(observer_cont_table, simulate.p.value = TRUE) # p = 0.09095. Fail to reject NULL
#chi_observer_nest_found

# fisher's exact test : better suited for small datasets like ours. 
fisher_nest_found_obs <- fisher.test(observer_cont_table)
fisher_nest_found_obs # p = 0.081 fail to reject NULL

# NULL: bloom_area DOES NOT affect if a nest is found during a survey.
# ALT: bloom_area DOES not affect if a nest is found during a survey.
bloom_area_cont_table <- print(table(survey_nest$nest_found, survey_nest$bloom_area)) # create contingency table
# chi_bloom_area_nest_found <- chisq.test(bloom_area_cont_table, simulate.p.value = TRUE) # p = 1. Fail to reject NULL
# chi_bloom_area_nest_found

# fisher's exact test : better suited for small datasets like ours. 
fisher_nest_found_bloom <- fisher.test(bloom_area_cont_table)
fisher_nest_found_bloom # p = 1. fail to reject NULL

# Chisquare tests for categorical variables
  # NULL: bombus_obs DOES NOT affect if a nest is found during a survey.
  # ALT: bombus obs DOES affect if a nest is found during a survey.
 bombus_cont_table <- print(table(survey_nest$nest_found, survey_nest$bombus_obs, useNA = 'no')) # create contingency table
#  chi_bombus_nest_found <- chisq.test(bombus_cont_table, simulate.p.value = FALSE) # p = 0.02849. REJECT NULL
 # chi_bombus_nest_found
  
# fisher's exact test : better suited for small datasets like ours. 
  fisher_nest_found_bombus <- fisher.test(bombus_cont_table)
  fisher_nest_found_bombus # p = 0.03321* REJECT NULL

# Plot of nest_found vs bombus category. Sites are included
ggplot(data = subset(survey_nest, !is.na(bombus_obs)), aes(x = factor(bombus_obs, 
                                                          levels = c('0',
                                                          'less than 5',
                                                          '5 to 10', 
                                                          '10 to 19',
                                                          '20 to 39', 
                                                          'more than 40')), 
                                                          y = nest_found,
                                                          color = location)) + 
  geom_point(position = position_jitterdodge(jitter.height = 0.2, jitter.width = 0),
             size = 2) +
  ylab('Nest found?') +
  xlab('Number of BB observed') +
  theme(axis.text.x = element_text(vjust = 0.7, angle = 0, size = 12, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 14),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 14)) +
  scale_y_discrete(labels = c("No", "Yes"))

                                              
# attempting a generalized linear mixed effect model
  
# glmer(nest_found ~ bombus_obs + (1|location),
#       data = survey_nest,
#      family = binomial(link = 'logit')) # It turns out this doesn't work because of "complete separation" of the dataset. 

# An alternative to this would be to use Firth's Logistic Regression, which accounts for this
library(logistf) # loading package

lrf1 <- logistf(nest_found ~ bombus_obs, 
        data = survey_nest,
        firth = TRUE)
summary(lrf1) # ran this test, appears that results are non-significant

ggplot(data = subset(survey_nest, !is.na(bombus_obs)), aes(x = factor(bombus_obs, 
                                                                      levels = c('0',
                                                                                 'less than 5',
                                                                                 '5 to 10', 
                                                                                 '10 to 19',
                                                                                 '20 to 39', 
                                                                                 'more than 40')), 
                                                           y = nest_found,
                                                           color = location)) + 
  geom_point(position = position_jitterdodge(jitter.height = 0.2, jitter.width = 0),
             size = 2) +
  geom_point(aes(y = lrf1$predict), position = 'identity') +
  ylab('Nest found?') +
  xlab('Number of BB observed') +
  theme(axis.text.x = element_text(vjust = 0.7, angle = 0, size = 12, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 14),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 14)) 
 
  