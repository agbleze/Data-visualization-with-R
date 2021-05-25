####violin plot to see the distribution of acres burned by state
library(readr)
library(dplyr)
library(ggplot2)
df <- read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
df%>%
  select(ORGANIZATI, STATE, YR = YEAR_, ACRES = TOTALACRES, CAUSE) %>%
  filter(ACRES >= 1000) %>%
  group_by(STATE) %>%
  ggplot(mapping = aes(x = STATE, y = log(ACRES))) + geom_violin() + geom_boxplot(width = 0.1) +
  ggtitle("Violin plot with boxplot") + xlab("states") + ylab("log of total acres") +
  theme_dark() + theme(legend.position = "bottom") 
