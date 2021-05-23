####Tuttorial on tidying data by gathering and spreading###
#Read data
actData = read_csv("CountryPopulation.csv", col_names = TRUE)
#View the loaded data
View(actData)
# tidy the data by gathering
tidyData = gather(actData, "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", key = "YEAR", value="POPULATION")
View(tidyData)

######spreading data######
install.packages("devtools")
devtools::install_github("garrettgman/DSR")
View(table2)
## spreading
table2spd = spread(table2, key = key, value = value)
View(table2spd)
##Separate a column with the values of two variables in a single cell 
View(table3)
# Use separate function
sepTab = separate(table3, rate, into = c("cases", "population"))
View(sepTab)
#load data
loadData = read_csv("usco2005.csv", col_names = TRUE)
View(loadData)
sepData = separate(loadData, "State-County Name", into = c("StateAbbrev", 'CountyName', 'Type'))
View(sepData)
####Unite combines different column (variables) values into 1 column as the opposite of separate
uniData = unite(sepData, State_county_Name, StateAbbrev, CountyName)
View(uniData)
