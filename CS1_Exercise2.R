##### Exercise 2: Has the acreage burned increased over time?
library(readr)
library(dplyr)
library(ggplot2)
df <- read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
df%>%
  select(STATE, ACRES = TOTALACRES, CAUSE, YR = YEAR_)%>%
  filter(ACRES >= 1000)%>%
  group_by(YR)%>%
  summarize(totalacres = sum(ACRES)) %>%
  ggplot(mapping = aes(x = YR, y = log(totalacres))) + geom_point() + geom_smooth(method = lm, se = TRUE) + 
  ggtitle("total acres burned") + xlab("Year") + ylab("Log of total acres burned")
