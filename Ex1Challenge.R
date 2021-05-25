####Exercise1 challenge
##load libraries
library(readr)
library(dplyr)
library(ggplot2)
##read file
datald = read_csv("StudyArea.csv", col_types = cols(.default = "c"), col_names = TRUE)
View(datald)
data5000 = filter(datald, TOTALACRES >= 5000)
data5000grp = group_by(data5000, YEAR_)
dat5000sum = summarize(data5000grp, mean(YEAR_), count = n())
ggplot(data = dat5000sum, mapping = aes(x = YEAR_, y = count)) + geom_point() +
  geom_smooth(method = lm, se = TRUE) + ggtitle("Fires greater than 5000") +
  xlab("YEAR") + ylab("Count") + theme_gray()

###### TOTALACRES gretater than 25000
data25 = filter(datald, TOTALACRES >= 25000)
data25grp = group_by(data25, STATE, YEAR_)
data25grpSum = summarize(data25grp, count = n())
ggplot(data = data25grpSum, mapping = aes(x = YEAR_, y = count)) + geom_point(aes(color = STATE)) +
geom_smooth(method = lm, se = TRUE)

#######  >= 100000
datald%>%
  select(STATE, YR = YEAR_, ACRES = TOTALACRES, CAUSE) %>%
  filter(ACRES >= 100000) %>%
  group_by(STATE, YR) %>%
  summarize(cnt = n()) %>%
  ggplot(mapping = aes(x = YR, y = cnt)) + geom_point() + facet_wrap(~STATE) + 
  geom_smooth(method = loess, se = TRUE) + ggtitle("Number of Fires by State and Year") + xlab("Year") + ylab("Number of Fires")
