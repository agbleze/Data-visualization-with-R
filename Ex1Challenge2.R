#####EX1 challeneg2
dat = read_csv("StudyArea.csv", col_types = cols(.default = "c"), col_names = TRUE)
dat%>%
  select(YR = YEAR_, ACRES = TOTALACRES, STATE, CAUSE)%>%
  group_by(YR, CAUSE == "Natural")%>%
  summarize(newcount = n())%>%
  ggplot(mapping = aes(x = YR, y = newcount)) + geom_point() + geom_smooth(method = loess, se = TRUE)
