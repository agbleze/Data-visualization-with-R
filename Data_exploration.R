# Exploration of categorical data
#read data
rawData = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
filtData = filter(rawData, TOTALACRES >= 1000, YEAR_ %in% c(2010, 2011,2012,2013,2014,2015,2016))
#creat bar chart
ggplot(data = filtData) + geom_bar(mapping = aes(x = YEAR_))
View(count(filtData, YEAR_))

####Measuring continuous variation with histogram#######
#read data
raw = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE) 
  raw %>%
  select(TOTALACRES) %>%
  filter(TOTALACRES >= 1000) %>%
ggplot() + geom_histogram(mapping = aes(x = TOTALACRES), binwidth = 500)  
# count number of fires in each bin
raw%>%
  count(cut_width(TOTALACRES, 500))
########Histogram with bin size of 5000############
df = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(),OUTDATED = col_character()), col_names = TRUE)
df%>%
  select(TOTALACRES)%>%
  filter(TOTALACRES >= 1000)%>%
  ggplot() + geom_histogram(mapping=(aes(x=TOTALACRES)), binwidth = 5000)

#### Measuring covariation with boxplot ####
daf = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
daf%>%
  select(TOTALACRES, ORGANIZATI)%>%
  filter(TOTALACRES >= 1000 & TOTALACRES <= 5000)%>%
  group_by(ORGANIZATI)%>%
  ggplot(mapping = aes(x = ORGANIZATI, y = TOTALACRES)) + geom_boxplot()
###Covariation of CAUSE and TOTALACRES###
datald = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
datald%>%
  select(CAUSE, TOTALACRES)%>%
  group_by(CAUSE)%>%
  ggplot(mapping = aes(x = CAUSE, y = TOTALACRES)) + geom_boxplot()

### Measuring covariation with symbol size -graduated symbol ###
dfFiresd = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
dfFiresd%>%
  filter(CAUSE %in% c("Natural", "Human"))%>%
  group_by(ORGANIZATI)%>%
  ggplot() + geom_count(mapping = (aes(x = ORGANIZATI, y = CAUSE)))
dfFiresd%>% 
count(ORGANIZATI,CAUSE) 
 
###Create 2D bin charts to visualize co-variation
Reload = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
#create 2D bin 
ggplot(data = Reload) + geom_bin2d(mapping = aes(x = YEAR_, y = TOTALACRES))
#creat 2D hex
ggplot(data = Reload) + geom_hex(mapping = aes(y = TOTALACRES, x = YEAR_))

####Generating summary statistics#####
##select columns ###
Reload = select(Reload, ORGANIZATI, STATE, YEAR_, TOTALACRES, CAUSE)
Reload = filter(Reload, TOTALACRES >= 1000)
mean(Reload$TOTALACRES)
median(Reload$TOTALACRES)
summary(Reload$TOTALACRES)
