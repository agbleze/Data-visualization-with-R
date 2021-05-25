## if there is a difference in the size of wildfires that were caused naturally as opposed to human induced
library(readr)
library(dplyr)
library(ggplot2)
##read data file csv
df <- read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
df%>%
  select(STATE, YR = YEAR_, ACRES = TOTALACRES, CAUSE) %>%
  filter(ACRES >= 1000 & CAUSE %in% c("Natural", "Human")) %>%
  group_by(YR, CAUSE)%>%
  summarize(totalacres = sum(ACRES))%>%
  ggplot(mapping = aes(x = YR, y = log(totalacres), colour = CAUSE)) +geom_point() +
  geom_smooth(method = lm, se = TRUE) + ggtitle("Total Acres Burned") + xlab("Year") +
  ylab("Log of total acres burned") + theme_dark()
  
  