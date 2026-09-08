##Scrap code from no nest analysis
#all_habitat is dataframe in nonest_analysis.R or noNest_plot_analysis.R. This script will not run without the data frames created in that script. 
#This script is meant to store unused code from analyses. Mostly models that aren't a good fit. 

##### Are habitat measures different at the plot level (plots with vs without a nest)? But determined that we don't have enough data to look at this data at the plot level. 

#Create data frame with one row per nest or one row per plot -> Does this need to be simplified even further? Averaging or summarizing all of the nests within one plot? 
plot_habitat_summarized <- all_habitat %>% 
  group_by(nest_ID, survey_plot, site, year) %>% 
  summarize(
    avg_bare  = mean(mean_litter_depth, na.rm = TRUE),
    avg_forb  = mean(per_forb_midpoint, na.rm = TRUE),
    avg_grass = mean(per_grass_midpoint, na.rm = TRUE),
    compaction = mean(psi_avg, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    site = as.factor(site),
    year = as.factor(year),
    survey_plot = as.factor(survey_plot)
  )

plot_habitat_summarized <- plot_habitat_summarized %>%
  mutate(nest_present = if_else(nest_ID == "no_nest", "no", "yes")) %>%
  mutate(nest_present = as.factor(nest_present))  #adds column to identify whether row represents a nest or not


plot_habitat_summarized <- plot_habitat_summarized %>% 
  mutate(., avg_grass_prop = avg_grass / 100, 
         avg_bare_prop = avg_bare / 100, 
         avg_forb_prop = avg_forb/ 100) # creating proportion columns (0-1) to meet assumptions for beta tests

plot_habitat_summarized2025 <- plot_habitat_summarized %>% 
  filter(., year == 2025) 

#Modeling all habitat measures together -> Model is asking: how do these habitat variables influence nest presence? 
plot_model <- glmmTMB(
  nest_present ~ avg_grass + avg_forb + avg_bare + compaction + (1 | survey_plot), #This code will run the model until I add "+ (1 | survey_plot). This prevents pseudoreplication. 
  data = plot_habitat_summarized,
  family = binomial()
)

summary(plot_model)

##### Creating models for each habitat cover measure, still at plot level - is this necessary ? 
## Soil Compaction Model 
## Testing normality - data NOT normal 
shapiro.test(plot_habitat_summarized$compaction)
shapiro.test(log(plot_habitat_summarized$compaction))
hist(plot_habitat_summarized$compaction)

#Creating models 
compaction_model2 <- glmmTMB(log(compaction) ~ nest_present + (1|site), 
                             data = plot_habitat_summarized, 
                             family = Gamma(link = "log"))  
summary(compaction_model2) # AIC = 
summary(aov(compaction_model2)) 

compaction_model3 <- glmmTMB(compaction ~ nest_present + (1 + survey_plot|site), 
                             data = plot_habitat_summarized, 
                             family = Gamma(link = "log")) # using means of each quadrat at each nest
summary(compaction_model3) #
summary(aov(compaction_model3)) #

#Grass model

#Forb model

#Bare ground model 

#Creating dataframe with necessary categories to compare nest and no nest habitat
all_habitat2 <- all_habitat %>% 
  select(survey_plot, year, site, nest_ID, leaf_lit_cm_1, leaf_lit_cm_2, leaf_lit_cm_3, psi_avg, mean_litter_depth, quadrat_distance, proximity, per_bare, per_forb, per_grass) %>%
  mutate(
    site = as.factor(site),
    year = as.factor(year),
    survey_plot = as.factor(survey_plot)
  ) %>%
  mutate(nest_present = if_else(nest_ID == "no_nest", "no", "yes")) %>%
  mutate(nest_present = as.factor(nest_present))

## Creating models 
## Testing normality
#Grass first 
shapiro.test(all_habitat2$prop_grass)
shapiro.test(log(all_habitat2$prop_grass))
hist(all_habitat2$prop_grass)

grass_model2 <- glmmTMB(prop_grass ~ nest_present + (1|survey_plot), 
                        data = all_habitat2, 
                        family = beta_family) 


##Then, can use both nest_present and quadrat_distance to dest the differences between nest and non-nest plots and if distance from the nest impacts that relationship

full_model <- glmmTMB(
  prop_forb ~ nest_present * quadrat_distance + (1 | survey_plot),
  data = all_habitat2,
  family = beta_family()
)


#Unused models. These models use all data from 2024 and 2025 as well as random effects that may not have been chosen for the final analysis.  
#Fitting a logistic regression for percent cover data (grass, forbs, bare ground)

###GRASS 

##Grass model including year as an effect

grass_model <- glmmTMB(
  nest_present_num ~ avg_grass + year + (1 | site:survey_plot),
  data = plot_habitat_summarized,
  family = binomial()
)

summary(grass_model)
drop1(grass_model, test = "Chisq")# Asking what happens if predictor variables are removed from the model #No clear effect of grass cover on nest presence probability, no evidence that nest probability differs from 2024 to 2025
summary(aov(grass_model))
plot(grass_model)
plot_model(grass_model, type = "pred", terms = c("nest_present_num"))

#Grass predicted values

preds_grass <- expand.grid(
  avg_grass = seq(min(plot_habitat_summarized$avg_grass),
                  max(plot_habitat_summarized$avg_grass),
                  length.out = 50),
  year = levels(plot_habitat_summarized$year))

preds_grass$prob <- predict(
  grass_model,
  preds_grass,
  type = "response",
  re.form = NA)

#plotting it

ggplot(preds_grass, aes(x = avg_grass, y = prob, color = year)) +
  geom_jitter(data = plot_habitat_summarized,
              aes(x = avg_grass, y = nest_present_num),
              alpha = 0.4, height = 0.1) +
  geom_line(data = preds_grass,
            aes(x = avg_grass, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Grass cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()

#Visualizing the data to try and find a better way of plotting it
ggplot(plot_habitat_summarized, aes(x = nest_present, y = avg_grass, fill = nest_present)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~year) +  
  labs(x = "Nest Present", y = "Average Grass (%)", fill = "Nest Present") +
  theme_minimal()

#Grass model without year as an effect
grass_model2 <- glmmTMB(
  nest_present_num ~ avg_grass + (1 | site:survey_plot),
  data = plot_habitat_summarized,
  family = binomial()
)

summary(grass_model2)
drop1(grass_model2, test = "Chisq") #After removing year, grass becomes significant. 

preds_grass2 <- data.frame(
  avg_grass = seq(
    min(plot_habitat_summarized$avg_grass, na.rm = TRUE),
    max(plot_habitat_summarized$avg_grass, na.rm = TRUE),
    length.out = 100
  )
)

preds_grass2$prob <- predict(grass_model2, preds_grass2, type = "response", re.form = NA)

ggplot() +
  geom_jitter(data = plot_habitat_summarized,
              aes(x = avg_grass, y = nest_present_num),
              alpha = 0.4, height = 0.1) +
  geom_line(data = preds_grass2,
            aes(x = avg_grass, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Grass cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()



#Forb model

forb_model <- glmmTMB(
  nest_present_num ~ avg_forb + year + (1 | site:survey_plot),
  data = plot_habitat_summarized,
  family = binomial()
)

summary(forb_model)
drop1(forb_model, test = "Chisq")

#Forb predicted values

preds_forb <- expand.grid(
  avg_forb = seq(min(plot_habitat_summarized$avg_forb),
                 max(plot_habitat_summarized$avg_forb),
                 length.out = 50),
  year = levels(plot_habitat_summarized$year))

preds_forb$prob <- predict(
  forb_model,
  preds_forb,
  type = "response",
  re.form = NA)

#plotting it

ggplot(preds_forb, aes(x = avg_forb, y = prob, color = year)) +
  geom_jitter(data = plot_habitat_summarized,
              aes(x = avg_forb, y = nest_present_num),
              alpha = 0.4, height = 0.1) +
  geom_line(data = preds_forb,
            aes(x = avg_forb, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Forb cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()

#Bare ground model

bare_model <- glmmTMB(
  nest_present_num ~ avg_bare + year + (1 | site:survey_plot),
  data = plot_habitat_summarized,
  family = binomial()
)

summary(bare_model)
drop1(bare_model, test = "Chisq")

#Bare predicted values

preds_bare <- expand.grid(
  avg_bare = seq(min(plot_habitat_summarized$avg_bare),
                 max(plot_habitat_summarized$avg_bare),
                 length.out = 50),
  year = levels(plot_habitat_summarized$year))

preds_bare$prob <- predict(
  bare_model,
  preds_bare,
  type = "response",
  re.form = NA)

#plotting it

ggplot(preds_bare, aes(x = avg_bare, y = prob, color = year)) +
  geom_jitter(data = plot_habitat_summarized,
              aes(x = avg_bare, y = nest_present_num),
              alpha = 0.4, height = 0.1) +
  geom_line(data = preds_bare,
            aes(x = avg_bare, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Bare ground cover (%)",
    y = "Probability of nest presence"
  ) +
  theme_minimal()

#Bare ground model without year as an effect

bare_model2 <- glmmTMB(
  nest_present_num ~ avg_bare + (1 | site:survey_plot),
  data = plot_habitat_summarized,
  family = binomial()
)

summary(bare_model2)
drop1(bare_model2, test = "Chisq") #After removing year, grass becomes significant. 

preds_bare2 <- data.frame(
  avg_bare = seq(
    min(plot_habitat_summarized$avg_bare, na.rm = TRUE),
    max(plot_habitat_summarized$avg_bare, na.rm = TRUE),
    length.out = 100
  )
)

preds_bare2$prob <- predict(bare_model2, preds_bare2, type = "response", re.form = NA)

ggplot(plot_habitat_summarized, aes(x = nest_present, y = avg_bare, fill = nest_present)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  #geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.8) +
  labs(
    x = "Nest Presence",
    y = "Average Bare Ground (%)"
  ) +
  facet_wrap(~year) +
  theme_minimal()

#Soil compaction model

compaction_model <- glmmTMB(
  nest_present_num ~ compaction + (1 | site:survey_plot), #had to remove year as an effect or got all NaN values
  data = plot_habitat_summarized,
  family = binomial()
)

summary(compaction_model)
drop1(compaction_model, test = "Chisq")

#Grass predicted values

preds_compaction <- data.frame(
  compaction = seq(
    min(plot_habitat_summarized$compaction, na.rm = TRUE),
    max(plot_habitat_summarized$compaction, na.rm = TRUE),
    length.out = 100
  )
)

preds_compaction$prob <- predict(compaction_model, preds_compaction, type = "response", re.form = NA)

#plotting it

ggplot() +
  geom_jitter(data = plot_habitat_summarized,
              aes(x = compaction, y = nest_present_num),
              alpha = 0.4) +
  geom_line(data = preds_compaction,
            aes(x = compaction, y = prob),
            linewidth = 1.2) +
  ylim(0, 1) +
  labs(
    x = "Soil compaction (psi)",
    y = "Probability of nest presence"
  ) +
  theme_minimal() 

#Grass model diagnostics (not sure if this is right)

preds_grass25$logit <- log(
  preds_grass25$prob /
    (1 - preds_grass25$prob)
)

ggplot(preds_grass25, aes(x = avg_grass, y = logit)) +
  geom_jitter(size = 3, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(
    x = "Average Grass (%)",
    y = "Logit (log-odds of nest presence)"
  ) +
  theme_minimal()

##Logistic regression diagnostics 
#Checking that model meets assumptions of linearity. Don't need to do this.....

#Calculate logits. Add values to preds_grass25 data frame created above
preds_grass25 <- preds_grass25 %>%
  mutate(logit = log(prob / (1 - prob)))

ggplot(preds_grass25, aes(x = avg_grass, y = logit)) +
  geom_point(size = 2) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "red",
    linewidth = 2) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue",
    linetype = "dashed", #Diff line type makes it easier to tell if the red and blue line are exactly overlapping each other 
    linewidth = 1) +
  theme_classic()
#Red line and blue line (red = loess curve, blue = linear fit) lie exactly on top of each other, meaning assumptions of linearity are met.

#Quadratic model for comparision to use in model fit testing 
grass_model25_quad <- glmmTMB(
  nest_present_num ~ avg_grass + I(avg_grass^2),
  data = plot_habitat25,
  family = binomial()
)

anova(grass_model25, grass_model25_quad)
summary(grass_model25_quad)

#Combining variables 
combined_model25 <- glmmTMB(
  nest_present_num ~ avg_grass + avg_bare + avg_forb + compaction + (1 | site:survey_plot),
  data = plot_habitat25,
  family = binomial()
)

summary(combined_model25)