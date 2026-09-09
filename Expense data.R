library(tidyverse)

expense <- rep(c("Communications/Training", 
             "Equipment and supplies",
             "Vehical rental & mileage",
             "Staff salary"), 3)

project <- rep(c("Volunteer Visual Surveys",
             "Staff Visual Surveys",
             "Radio Tracking"), 4)

costs_df <- data.frame(expense = expense,
           project = project)
costs_df <- costs_df %>% arrange(., project)
costs_df
costs <- c(1178, 3773, 0, 5809,
           46, 0, 6579, 5342,
           210, 3912, 205, 23)
costs_df$cost <- costs
costs_df

costs_df$expense <- factor(costs_df$expense, levels = c("Communications/Training", 
                                                        "Equipment and supplies",
                                                        "Vehical rental & mileage",
                                                        "Staff salary"))
costs_df$project <- factor(costs_df$project, levels = c("Volunteer Visual Surveys",
                                                        "Staff Visual Surveys",
                                                        "Radio Tracking"))

totals <- costs_df %>% group_by(project) %>% summarize(total = sum(cost))

ggplot(data = costs_df, aes(x = project, y = cost, fill = expense)) + 
  geom_bar(stat = "identity", position = 'stack', color = 'black') +
  geom_text(data = totals, aes(x = project, y = total, label= scales::dollar(total), fill = NULL), 
            size = 5,
            nudge_y = 1000) +
  ylim(0,17000) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12, color = 'black'),#rotate x-axis text
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 11),
        legend.position = "bottom") + 
  #xlab("") +
  ylab("Total Expenses (USD)") +
  scale_fill_manual(values = c("gray", "#8968CD", "#9A32CD", '#FF8C69' ),
                    guide = guide_legend(title = NULL)) +
  scale_y_continuous(labels = scales::dollar_format())
