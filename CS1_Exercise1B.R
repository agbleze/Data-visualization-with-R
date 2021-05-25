###Exercise 1: Have the number of wildfires increased or decreased in the past few decades?
##### facet warping by state
#Load libraries
library(readr)
library(dplyr)
library(ggplot2)
##read data
df <- read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
df%>%
  select(STATE, YR = YEAR_, ACRES = TOTALACRES, CAUSE) %>%
  filter(ACRES >= 1000) %>%
  group_by(STATE, YR) %>%
  summarize(cnt = n()) %>%
  ggplot(mapping = aes(x = YR, y = cnt)) + geom_point() + facet_wrap(~STATE) + 
  geom_smooth(method = lm, se = TRUE) + ggtitle("Number of Fires by State and Year") + xlab("Year") + ylab("Number of Fires")
