library(readr)
library(ggplot2)
library(dplyr)
#Load data with read_csv
dfFires = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
#check number of records
nrow(dfFires)
# use filter() to subset dataset that meet a condition
df25k = filter(dfFires, TOTALACRES >= 25000)
nrow(df25k)
View(df25k)

#filtering with multiple conditions
df1k = filter(dfFires, TOTALACRES >= 1000, YEAR_ == 2016)
nrow(df1k)
# you can also use & for filterig multiple conditions
df2k = filter(dfFires, TOTALACRES >=2000 & YEAR_ == 2010)
# To include a list of potential values in the filter
dfYear = filter(dfFires,YEAR_%in% c(2010,2011,2012))
View(dfYear)

###################select()######################
dfFires2 = select(dfFires, FIRENAME, TOTALACRES, YEAR_)
head(dfFires2)
#Rename column names
dfFires2 = select(dfFires, "FIRE"="FIRENAME", "ACRES" = "TOTALACRES", "YR" = "YEAR_")
head(dfFires2)
#Returns any column taht contains the specified parameter
dfFires3 = select(dfFires, contains("DATE"))
head(dfFires3)
#Run multiple conditions
dfFires3 = select(dfFires, contains("DATE"), starts_with("TOTAL"))
head(dfFires3)

###########Arranging rows############
dfFires = read_csv("StudyArea.csv", col_types=list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
View(dfFires)
df1k = filter(dfFires, TOTALACRES > 1000 & YEAR_ == 2016)
df1k = select(df1k, "NAME" = "FIRENAME", "ACRES" = "TOTALACRES", "YR" = "YEAR_")
#Sort values in ascending order
arrange(df1k, ACRES)
#Sort in descending order
arrange(df1k, desc(ACRES))

############### Adding column
library(lubridate)
dfFires = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_names = TRUE)
df = select(dfFires, ORGANIZATI, STATE, YEAR_, TOTALACRES, CAUSE, STARTDATED)
df = filter(dfFires, TOTALACRES > 1000 & CAUSE %in% c('Human', 'Natural'))
View(df)
## Add a new day column which stores the day the fire started
df = mutate(df, DOY=yday(as.Date(df$STARTDATED, format = '%m/%d/%y%H:%M')))
View(df)

##Summarise data for each group
dfFires = read_csv("StudyArea.csv", col_types = list(UNIT = col_character(), OUTDATED = col_character()), col_name = TRUE)
df = select(dfFires, ORGANIZATI, STATE, YEAR_, TOTALACRES, CAUSE)
df = filter(df, TOTALACRES >= 1000)
# create a new column DECADE and fill it with the ifelse conditions
df = mutate(df, DECADE = ifelse(YEAR_ %in% 1980:1989, "1980-1989", ifelse(YEAR_ %in% 1990:1999, "1990-1999", ifelse(YEAR_%in% 2000:2009, "2000-2009",
                                                                                ifelse(YEAR_ %in% 2010:2016, "2010-2016", "-99")))))
View(df)
#groups the data by DECADE column
grp = group_by(df, DECADE)
sm = summarize(grp, mean(TOTALACRES))
View(sm)
#Rename new summarize columns
names(sm) <- c("DECADE", "MEAN_ACRES_BURNED")
#CREATE BARCHART
ggplot(data = sm) + geom_col(mapping = aes(x = DECADE, y = MEAN_ACRES_BURNED), fill = "red")
