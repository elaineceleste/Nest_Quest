# Nest Quest Nest Activity Data Start on 11/25/2025 SD

#load packages
library(tidyverse) #dplyr package
library(lubridate) #dealing with time and dates
library(readr) # creating .csv files
library(lme4) 
library(colorBlindness)

#load data 
activity_raw <- read.csv('nest_observation.csv')
activity_raw_survey123 <- read.csv('survey123_nest_traffic.csv')

# cleaning survery123 data to be joined with main dataset

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


#cleaning
activity_raw <- activity_raw %>% filter(., site != '')# %>% # removing empty rows
                #mutate(., count.pollen_size = paste(amt_enter, pollen_size, sep = ':')) 

activity_unite <- unite(activity_raw, new_enter, amt_enter, amt_enter.1, amt_enter.2, amt_enter.3, amt_enter.4, amt_enter.5, sep = ":")
activity_unite <- unite(activity_unite, new_pollen, pollen_size, pollen_size.1, pollen_size.2, pollen_size.3, pollen_size.4, pollen_size.5, sep = ":")

all_exit <- activity_unite %>% 
              select(., !c("new_enter", "new_pollen")) %>% 
              mutate(amt_exit = ifelse(is.na(amt_exit), 0, amt_exit),
                     activity = 'exit', .before = amt_exit,
                     year = as.character(year),
                     tempC = as.numeric(tempC),
                     tempF = as.numeric(tempF)) %>% 
              uncount(amt_exit)

activity_seperated <- activity_unite %>% 
                       select(., !c('amt_exit')) %>% 
                       separate_rows(., c(new_enter, new_pollen), sep = ":") %>% 
                       mutate(new_enter = na_if(new_enter, "NA"),
                              new_pollen = na_if(new_pollen, 'na')) %>% 
                       mutate(new_pollen = na_if(new_pollen, '')) %>% 
                       mutate(year = as.character(year),
                              tempC = as.numeric(tempC),
                              tempF = as.numeric(tempF),
                              new_enter = as.numeric(new_enter))

activity_seperated <- activity_seperated[!is.na(activity_seperated$new_enter), ]

all_enter <- activity_seperated %>% 
  uncount(new_enter) %>% 
  mutate(activity = 'enter', .before = new_pollen)

all_activity <- full_join(all_enter, all_exit)

all_activity <- all_activity %>% rename(., 
                                        pollen = new_pollen,
                                        role = staff.or.volunteer) %>% 
  mutate(species = case_when(grepl('imp', nest_ID) ~ "impatiens",
                   grepl('auri', nest_ID) ~ 'auricomus',
                   grepl('rufo', nest_ID) ~ 'rufocinctus',
                   grepl('gris', nest_ID) ~ 'griseocollis',
                   grepl('ferv', nest_ID) ~ 'fervidus'))



all_activity <- unite(all_activity, date, day, month, year, sep = '-', remove = FALSE)
all_activity <- all_activity %>% mutate(date = as.Date(date, "%d-%m-%Y"),
                                        week_true = as.factor(strftime(c(date), format = "%U")),
                                        floor_date = floor_date(date, 'week'),
                                        floor_date = format(floor_date, "%b-%d"),
                                        duration = 20)
all_activity <- unite(all_activity, site, nest_ID, date, start_time, end_time, observer1, sep = ":", remove = FALSE, col = unique_ID)
n_distinct(all_activity$unique_ID)
# 66 monitoring ocassions 
# 66 * 20 = 1320 minutes spent monitoring
# 1320/60 = 22 hours 
n_distinct(all_activity$nest_ID) 
# 17 total nests monitored

table(all_activity$pollen)
# 9+26+46+8+32
# 121
# 121/451
# 0.2682927

table(all_activity$activity)


activity_summary <- all_activity  %>% group_by(
                                               unique_ID,
                                               species, 
                                               year,
                                               week_true, 
                                               floor_date, 
                                               nest_ID,
                                               start_time, 
                                               end_time
                                               ) %>% 
                   summarize(in_total = sum(activity == "enter", na.rm = TRUE),
                             out_total = sum(activity == "exit", na.rm = TRUE),
                             total_activity = in_total + out_total) %>% 
                   mutate(duration = 20,
                          bees_per_min = total_activity/duration) # mean bee activity per minute per week

mean_bees_min_week_group <- aggregate(activity_summary$bees_per_min, FUN=mean,# calculate mean bees per minute per week
           by=list(species=activity_summary$species, year=activity_summary$year, week_true=activity_summary$week_true))
mean_bees_min_week_group <- mean_bees_min_week_group %>% rename(., mean_bees_min_week_group = x)


mean_bees_min_week_individual<- aggregate(activity_summary$bees_per_min, FUN=mean,# calculate mean bees per minute per week
                                by=list(nest_ID=activity_summary$nest_ID, species=activity_summary$species, year=activity_summary$year, week_true=activity_summary$week_true))
mean_bees_min_week_individual <- mean_bees_min_week_individual %>% rename(., mean_bees_min_week_indv = x)


activity_summary <- left_join(activity_summary, mean_bees_min_week_group) # join with summary
activity_summary <- left_join(activity_summary, mean_bees_min_week_individual)



ggplot(data = activity_summary, aes(x = week_true, y = bees_per_min)) +
  geom_boxplot()

ggplot(data = activity_summary, aes(x = week_true, y = bees_per_min, color = species, label = nest_ID )) +
  geom_point() +
  geom_text()

ggplot(data = activity_summary, aes(x = week_true, y = mean_bees_min_week, color = species, shape = year)) +
  geom_line() +
  geom_point(size = 3) +
  scale_shape_manual(values = c('2024' = 15, '2025' = 17)) 
 
ggplot(data = activity_summary, aes(x = week_true, 
                                    y = mean_bees_min_week_indv, 
                                    group = nest_ID, 
                                    shape = year,
                                    color = nest_ID)) +
  geom_line(stat = 'identity') +
  geom_point(size = 2) +
  scale_color_manual(values = c("#000000", "#004949", 
                                "#009292","#ff6db6",
                                "#ffb6db", "#490092",
                                "#006ddb", "#b66dff", 
                                "#6db6ff","#b6dbff",
                                "#920000","#924900",
                                "#db6d00", "#24ff24",
                                "#ffff6d", "#ff0000", "#00ffb4")) +
  
  ylab('Mean bees/minute/week') +
  xlab('Week') +
  facet_wrap(vars(species, year), ncol = 2, strip.position = 'top') 

ggplot(data = activity_summary, aes(x = week_true, 
                                    y = mean_bees_min_week_group, 
                                    color = species, 
                                    group = species
                                    )) +
  geom_line(stat = 'identity') +
  geom_point(size = 2) +
  ylab('Mean bees/minute/week') +
  xlab('Week') +
  facet_wrap(vars(year), ncol = 1, strip.position = 'top') 
  


ggplot(data = all_activity, aes(x = week_true, y = tempF)) +
  geom_boxplot() +
  geom_point()

   
   




         