require(tidyverse)
require(scales)

stackingorder = c("closed","green","gold")
color <- read_csv("Data/OA-Labeled.csv")

color <- color %>% filter(!(year == "2002")) %>% filter(!(year == "2007")) %>% filter(!(year =="no year")) %>% filter(!(year=="not found"))
color$color <- factor(color$color, levels = stackingorder)
color <- color %>% group_by(year) 
#%>% group_by(color)   
#data <- summarise(color,n())

  plot <- ggplot(color, aes(year)) +
  geom_bar(aes(fill=color), position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("red", "green", "gold"))

# Barplot
print(plot)

