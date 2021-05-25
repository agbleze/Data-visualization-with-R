library(readr)
library(dplyr)
library(ggplot2)
#Exercise 1: Have the number of wildfires increased or decreased in the past few decades?
reData <- read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
nrow(reData)
reData%>%
  select(STATE, YR = YEAR_, ACRES = TOTALACRES, CAUSE)%>%
  filter(ACRES >= 1000)%>%
  group_by(YR)%>%
  summarize(count = n())%>%
  ggplot(mapping = aes(x = YR, y = count)) + geom_point() + geom_smooth(method = loess, se = TRUE) +
  ggtitle("Large Fires are becoming more") + xlab("Year") + ylab("Number of Wildfires") + theme_dark()
  
  
  