# Nest Quest Nest Activity Data Start on 11/25/2025 SD

#load packages
library(tidyverse) #dplyr package
library(lubridate) #dealing with time and dates
library(readr) # creating .csv files
library(lme4) 
library(colorBlindness)
library(forcats)
library(gridExtra) # for creating plot grids

#load data 
activity_raw <- read.csv('nest_observation.csv')
activity_raw_survey123 <- read.csv('survey123_nest_traffic.csv')
sites <- read.csv("nest_survey_site_data.csv") 

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

activity_raw <- activity_raw %>% mutate(., site = tolower(site)) 



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
                                        week_true = as.factor(strftime(c(date), format = "%W")),
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
# 121/(121+451)
# 0.2115385 about 21% of bees had pollen when entering, for ones we could tell. 

table(all_activity$activity)

# Pollen plots
table(all_enter$new_pollen)
#pollen_data <- full_join(sites, all_enter)
#pollen_data <- pollen_data %>% rename(., pollen = new_pollen)
#pollen_data <- left_join(pollen_data, pollen_weights)

pollen_weights <- data.frame(pollen = c('none', 'XS', 'S', 'M', 'L', 'XL'),
                             weight_g = c(0, NA, 6, 13, 20, 45))
all_activity <- left_join(all_activity, pollen_weights)


#
pollen_nestID <- all_activity %>%  filter(., activity == 'enter') %>% group_by(nest_ID, pollen) %>% 
  summarize(., total_g_pollen = sum(weight_g)) %>% na.omit()
               #pollen_size_count = count(pollen))
pollen_nestID %>% 
  mutate(nest_ID = fct_reorder(nest_ID, total_g_pollen))

pollen_grams_figure <- ggplot(pollen_nestID, aes(x = nest_ID, y = total_g_pollen, fill = factor(pollen, levels = c( 'XL', 'L', 'M', 'S')))) +
  geom_bar(stat = 'identity', position = 'stack', color = 'black') +
  scale_fill_brewer(palette = 'Greens', guide = guide_legend(title = NULL)) +
  theme(#axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0, size = 12, color = 'black'),#rotate x-axis text
    axis.text.y = element_text(size = 10, color = 'black'),
    axis.title = element_text(size = 14),
    #panel.grid.major = element_blank()
   panel.grid.minor = element_blank()
   # panel.background = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0, 350, by = 50)) +
  labs(x = "", y = 'Total pollen (grams)') +
  coord_flip()


# Pollen size counts for each nest
pollen_counts <- all_activity %>%  
  filter(., activity == 'enter') %>% 
  group_by(nest_ID) %>% 
  count(pollen) %>% 
  rename(count = n)

pollen_count_figure <- ggplot(pollen_counts, aes(x = nest_ID , y = count, fill = factor(pollen, levels = c('NA', 'XL', 'L', 'M', 'S', 'XS', 'none')))) +
  geom_bar(stat = 'identity',
           position = 'stack' ,
           color = 'black') +
  scale_fill_brewer(palette = 'Purples', guide = guide_legend(title = NULL)) +
  theme(#axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0, size = 12, color = 'black'),#rotate x-axis text
    axis.text.y = element_text(size = 10, color = 'black'),
    axis.title = element_text(size = 14),
   # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.background = element_blank()
) +
  labs(x = "", y = "Total entering bees") + #labels
  scale_y_continuous(breaks = seq(0, 90, by = 10)) +
  coord_flip()
pollen_count_figure 

grid.arrange(pollen_count_figure, pollen_grams_figure, ncol = 2)

#
pollen_species <- all_activity %>% 
  filter(., activity == 'enter') %>% 
  group_by(species, pollen) %>% 
  summarize(., total_g_pollen = sum(weight_g))

ggplot(pollen_species, aes(x = species, y = total_g_pollen, fill = pollen)) +
  geom_bar(stat = 'identity', position = 'stack')


# Creating activity summary dataset
#all_activity <- merge(pollen_weights,all_activity, by.x="pollen", by.y="pollen", all.x = FALSE, all.y = TRUE)
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
            total_activity = in_total + out_total,
            total_g_pollen= sum(weight_g, na.rm = TRUE)) %>% 
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


# Activity plots
ggplot(data = activity_summary, aes(x = week_true, y = bees_per_min)) +
  geom_boxplot()

ggplot(data = activity_summary, aes(x = week_true, y = bees_per_min, color = species, label = nest_ID )) +
  geom_point() +
  geom_text()

ggplot(data = activity_summary, aes(x = week_true, y = mean_bees_min_week_indv, color = species, shape = year)) +
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
  #scale_x_discrete(labels = c("Jun-04", "Jun-11", "Jun-18", "Jun-25", "Jul-02", "Jul-09", "Jul-16", "Jul-23", "Jul-30", "Aug-06")) 
  facet_wrap(vars(year), ncol = 1, strip.position = 'top') 
  

#temperature and nest activity
ggplot(data = all_activity, aes(x = week_true, y = tempF)) +
  geom_boxplot() +
  geom_point()


# Pollen plots
ggplot(data = activity_summary, aes(x = week_true, y = total_g_pollen)) +
  geom_boxplot()

ggplot(data = activity_summary, aes(x = week_true, y = total_g_pollen, fill = species)) +
  geom_boxplot() 

mean_pollen_weekly <- aggregate(activity_summary$total_g_pollen, FUN=mean,# calculate mean bees per minute per week
          by=list(species=activity_summary$species, year=activity_summary$year, week_true=activity_summary$week_true))
mean_pollen_weekly <- mean_pollen_weekly %>% rename(., mean_pollen_weekly = x)

activity_summary <- left_join(activity_summary, mean_pollen_weekly)



# pollen over time
ggplot(data = activity_summary, aes(x = week_true, y = mean_pollen_weekly, group = species, color = species)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(year), ncol = 1, strip.position = 'top') 

ggplot(data = activity_summary, aes(x = week_true, y = mean_pollen_weekly, group = nest_ID, color = nest_ID)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(year), ncol = 1, strip.position = 'top') 
         
imp1_24hp<- filter(all_activity, nest_ID == '24hp_imp1') # this nest had one really good day of pollen. Or one really good day of the observer being able to see pollen

