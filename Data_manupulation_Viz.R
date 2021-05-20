library(ggplot2)
library(readr)
lirary(lubridate)

dfCrime = read_csv("Crime_Data.csv", col_names = TRUE)

#Selects and rename columns
dfCrime = select(dfCrime, 'CrimeDate' = 'Reported Date', 'Category' = 'Crime Subcategory', 'Description'='Primary Offense Description', 'Precinct', 'Sector', 'Beat', 'Neighborhood')
#filters the data and returns what satisfy the filter condition
dfCrime2 = filter(dfCrime, Neighborhood == 'QUEEN ANNE', Category == 'BURGLARY-RESIDENTIAL')
#retunrs number of records in the data
nrow(dfCrime2)
nrow(dfCrime)
#Views the data
View(dfCrime2)
# Adds a new column Year to the data and fill it with extract of year from previous data
dfCrime3 = mutate(dfCrime2, YEAR = year(as.Date(dfCrime2$CrimeDate, format='%m/%d/%Y')))
View(dfCrime3)
#group the data by year
dfCrime4 = group_by(dfCrime3, YEAR)
# create a suumary with new column n showing number of count
dfCrime4 = summarise(dfCrime4, n = n())
View(dfCrime4)
#plot the data
ggplot(data = dfCrime4) + geom_col(mapping = aes(x=YEAR, y=n), fill ="red")

################Create bar chart of crime by month  #############
dfCrime5 = mutate(dfCrime2, MONTH = month(as.Date(dfCrime2$CrimeDate, format = '%m/%d/%Y')))
#Group the data by month
dfCrime5 = group_by(dfCrime5, MONTH)
# create summary with number of crime for each month
dfCrime5 = summarise(dfCrime5, Crime_number = n())
#Plot bar chart
ggplot(data = dfCrime5) + geom_col(mapping = aes(x=MONTH, y=Crime_number), fill='green')
