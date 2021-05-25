###Exercise 2B
## Load library
library(readr)
library(dplyr)
library(ggplot2)

#read data file
df <- read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
df%>%
  select(STATE, YR = YEAR_, ACRES = TOTALACRES, CAUSE) %>%
  filter(ACRES >= 1000)%>%
  group_by(STATE, YR)%>%
  summarize(totalacres = sum(ACRES))%>%
  ggplot(mapping = aes(x = YR, y = log(totalacres))) + geom_point(aes(color = STATE)) + facet_wrap(~STATE)+
  geom_smooth(method = loess, se = TRUE) + ggtitle("ACres burnt by state") + xlab("YR")+
  ylab("total acres") + theme_dark()
  
  