##crating scatterplot
datald = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
datald
datald = select(datald, ORGANIZATI, STATE, YEAR_, TOTALACRES, CAUSE)
datald = group_by(datald, YEAR_)
datald = summarize(datald, totalacres = sum(TOTALACRES))
  ggplot(data = datald) + geom_point(mapping = aes(x=YEAR_, y = totalacres))

  ##Use logarithmin scale to respond to skewness towards large values
  ## log() for that
  ggplot(data = datald) + geom_point(mapping = aes(x = YEAR_, y = log(totalacres)))
  
  ##ADD REGRESSION LINE
ggplot(data = datald, aes(x = YEAR_, y = log(totalacres))) + geom_point() + geom_smooth(method = lm, se=FALSE)
## SET METHOD TO LOESS
ggplot(data = datald, aes(x=YEAR_, y=log(totalacres))) + geom_point() + geom_smooth(method=loess, se=FALSE)
## add confidence interval by setting se = true
ggplot(data = datald, aes(x=YEAR_, y=log(totalacres))) + geom_point() + geom_smooth(metho = loess, se = TRUE)

#######Plotting by catagories######
newData = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
grpData = group_by(newData, STATE, YEAR_)
View(grpData)
summarizeData = summarize(grpData, totalacres = sum(TOTALACRES))
##plot scatterplot with regresson line coloured
ggplot(data = summarizeData, aes(x = YEAR_, y=(totalacres), colour = STATE)) + geom_point(aes(colour = STATE)) + stat_smooth(method=lm, se=FALSE)

###labeling graph
ggplot(data = summarizeData, aes(x=YEAR_, y=log(totalacres))) + geom_point(aes(colour = STATE)) + 
  geom_smooth(method = loess, se = TRUE) + geom_text(aes(label=STATE, colour = STATE), size = 3, check_overlap = TRUE, nudge_x = 1) + 
  labs(title = paste("Acreage Burned by Wildfires has increased in the past decades"), subtitle = paste("1980-2016"), caption = "Data from USGS") +
  scale_y_continuous(name = "Log of total area burn") + scale_x_continuous(name = "Burn year") + theme(legend.position = "bottom") + 
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 4)))

### creating facet to display each set of data
ggplot(data = summarizeData, aes(x = YEAR_, y = log(totalacres))) + geom_point() + 
  facet_wrap(~STATE) + geom_smooth(method = loess, se = TRUE) + theme_dark()
 
