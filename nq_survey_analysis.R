# Nest Quest Survey123 Survey Data # SD start Oct 28 2025


#load packages
library(tidyverse) #dplyr package
library(lubridate) #dealing with time and dates
library(readr) # creating .csv files

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

# How many nests at each site/plot?
all_nests <- all_nests %>% 
  filter(., site != c("")) %>% 
  mutate( separate(., col = "date", into = c("month","day","year"),  sep = "/")) %>%  # separated date values so they all have a unique column. 
  rename(found_on_survey = Did.you.discover.this.nest.during.your.one.hour.survey.time.)

nests <- all_nests %>% 
  group_by(site) %>% 
  count(., .drop = FALSE) %>% 
  rename(total_nests = n) # counting the number of nests at each site.

sites <- right_join(nests, sites) 
sites <- sites %>%  mutate(total_nests = ifelse(is.na(total_nests), 0, total_nests)) # joining nest data to match sites. This is a nice summary table

#total_nests <- sum(nests$total_nests)


### How many surveys for each site/plot?

site_summary <- surveys_no_flower %>% 
  group_by(site, 
          Volunteer_or_staff, 
           year) %>% # organize surveys by site, year, and volunteer/staff
  summarize(total_survey_sum = format(round(sum(as.numeric(survey_count), na.rm = TRUE), digits = 4)), # survey totals
            total_unique_observers = n_distinct(observer1, na.rm = TRUE), .groups ="drop") # total unique surveyors for each plot. 
site_summary$total_survey_sum <- as.numeric(site_summary$total_survey_sum)

#site_summary <- site_summary %>% 
 # complete(., site, Volunteer_or_staff, year, fill = list(Volunteer_or_staff = 'volunteer')) %>% 
#  mutate(., total_survey_sum = ifelse(is.na(total_survey_sum), 0, total_survey_sum))
  #drop_na(total_unique_observers)

site_summary2 <- full_join(nests, site_summary) # note that total_nests column does not represent nests differentiated by surveyor type
site_summary2 <- site_summary2 %>% mutate(total_nests = ifelse(is.na(total_nests), 0, total_nests)) #NA's in nest column to 0
site_summary2 <- site_summary2 %>% filter(., site != "crow_gc")

## Code I was trying to do to get labels above bars. Did not work. 
#plot_labels <- site_summary2 %>% distinct(site) 

#plot_labels %>% mutate(nest = if_else(site_summary2$total_nests > 1, 'yes', 'no'))

#df <- data.frame(site = distinct(site_summary2, site),
            #     nest = c('yes', 'yes', 'yes','yes', ' yes','yes', 'yes', 'yes' ,'no', 'no', 'no', 'no', 'no', 'no', 'no', 'no', 'no'),
             #    x = c())

site_summary2

##### Plotting Survey amounts by year, and filled with volunteer or staff surveys.
#png("total_NQ_surveys_bar_no_hline.png", units="in", width=12, height=10, res=1200)
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
#dev.off()




### Bumble bee observations, categorical data 
table(surveys_no_flower$bombus_obs)# first look at bumble bee observation data



bombus_summary <- surveys_no_flower %>%
  select(.,c('site',
             'year',
             'bombus_obs')) %>%
  drop_na(bombus_obs)

#bombus_summary$nest <- ifelse(employees$Salary > 60000, "Yes", "No")



bombus_counts <- bombus_summary %>% 
  group_by(year, bombus_obs) %>% 
  tally()


#
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



#### Looking at how various variables affect nest detection ####
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
survey_nest <- survey_nest %>% mutate(., nest_found = as.character(ifelse(nest_found > 0, 1, 0))) # make values a binary 0:1 
     
survey_nest[survey_nest$nest_found == '1', ]
survey_nest[192, 11] = 0 # Changing one specific value based on position. There is one incident of Jayden having two surveys (8/5/2025 and hp1) with the same date and location. 
               # she found a nest in the first half of her survey, then stopped and started a new survey. R thinks a nest was found in both of these, when really it was just the first one
                  # PSA: code as I suggest, not as I do. I'm crossing my fingers this little code won't cause problems later lol
table(survey_nest$nest_found) # 0 = nest not found during survey, 1 = nest found during survey (There should be 12 nests found).     
survey_nest$survey_count <-as.numeric(surveys_no_flower$survey_count) 


##### Some summary analysis using this new dataframe. Lots of this is used for econoist summary #####
# nest finding success rate of volunteers
volunteer_nests <- survey_nest %>% 
  filter(., Volunteer_or_staff == "volunteer") # 110 total volunteer surveys

tally(volunteer_nests, nest_found == '1') / 110 # volunteer survey success rate is 0.018 nests/survey

# nest finding success rate of staff members
staff_nests <- survey_nest %>% 
  filter(., Volunteer_or_staff == "staff") # 155 total staff surveys

tally(staff_nests, nest_found == '1') / 155 # staff survey success rate is 0.058 nest/survey

survey_nest %>% group_by(site, year) %>% summarize(sum = sum(survey_count, na.rm = TRUE))

survey_success_summary <- survey_nest %>% count(year, nest_found, Volunteer_or_staff) # summary of success by year and observer type
survey_nest %>% count(year,
                      site,
                      nest_found,) # how many nests found per site/year combo? I entered this into the economist spreadsheet. 

## Total time spent surveying as recorded by survey123
survey_nest$time_spent_hms <- hms(survey_nest$time_spent) # make column hms format
survey_nest$time_spent_seconds <- period_to_seconds(survey_nest$time_spent_hms) # convert to seconds

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
#write.csv(time_per_site, "C:\\Users\\chand\\Documents\\data\\nestquest\\nestquest\\time_per_site2.csv")

# calculating pace of surveys
survey123_tracks <- read.csv('survey123_tracks_meters.csv')
survey123_tracks <- survey123_tracks %>% select(.,
                            'globalid',
                            'length') %>% 
  rename(GlobalID = globalid,
         length_meters = length) %>% na.omit()

survey_nest <- left_join(survey_nest, survey123_tracks)


