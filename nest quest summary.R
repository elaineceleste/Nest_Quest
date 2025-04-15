library(ggplot2)

soil=read.csv("nq_soil.csv")

soil$distance <- as.factor(soil$distance)
p <- ggplot(soil, aes(x=distance, y=perc_pebbles))+geom_violin()
  
p <- ggplot(soil, aes(x=distance, y=perc_sand))+geom_violin()




