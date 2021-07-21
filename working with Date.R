library(readr)
library(lubridate)
library(ggplot2)


GA_Dataset <- read_csv("Downloads/R_shiny_app_GoogleTrends/Users\\lin\\Downloads\\R_shiny_app_GoogleTrends\\GA_clean_Dataset.csv", 
                                                                          col_types = cols(`Avg. Session Duration` = col_time(format = "%H:%M:%S"), 
                                                                                           Date = col_date(format = "%m/%d/%Y")))
View(GA_Dataset)


### create date sequence
seq(from = as.Date("2010-1-1"), to = as.Date("2020-01-01"), by = "years")
seq(as.Date("2012/01/01"), as.Date("2016-08-31"), by = "7 days" )

## using lubridate
seq(ymd("2020/09/10"), ymd("2021/09/09"), by = "quarter")

x <- max(GA_Dataset$Date)
y <- min(GA_Dataset$Date)
y- x

timemax <- max(GA_Dataset$`Avg. Session Duration`)
timemin <- min(GA_Dataset$`Avg. Session Duration`)
timemin - timemax

## time zones
timex <- as.POSIXct("2015-09-22 01:00:00", tz = "US/Eastern")
timey <- as.POSIXct("2015-09-22 01:00:00", tz = "GMT")
timex == timey
timex - timey

now <- now()
today = ymd("2003-10-20")
now - today
new_duration(40)
dur <- lubridate::dseconds(GA_Dataset$`Avg. Session Duration`)
