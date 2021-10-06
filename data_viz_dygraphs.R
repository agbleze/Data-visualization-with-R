#### LOAD LIBRARIES
library(gdata)  # read csv from online
library(XML)  # identify links
library(RCurl)
library(stringr)  # string manipulation
library(rvest)
library(magrittr)
library(rjson)
library(rtimes) # NY times api
library(httr)  ## working with urls and api
library(jsonlite)
library(xts)

library(dygraphs)  ## dygraphs

####### httr for api
####### climate change data
#### Global Warming live graphs and API
### Retrive methane data
## define endpoint url
endpoint_url <- "https://global-warming.org/api/methane-api"
# request the data
methane_data <- GET(endpoint_url)

# retrive content of data
content(methane_data)
methane_data_contx <- httr::text_content(methane_data)
methane_text <- content(methane_data, as = 'text') # retrieve content as text
methane_raw <- content(methane_data, as = 'raw') #retrieve data as raw
methane_parsed <- content(methane_data, as = 'parsed')

## convert from json to r object
methane_fromJson <- fromJSON(methane_text) 

# convert to  dataframe
(methane_dataframe <- as.data.frame(methane_fromJson))

### retrive temperature anomaly data
temp_url <- "https://global-warming.org/api/temperature-api"

# retrives data from API and convert from JSON to R object
temp_anomaly_data <- fromJSON(temp_url)
temp_anomaly_dataframe <- as.data.frame(temp_anomaly_data$result)

# retrieve CO2 data from API
co2_url <- "https://global-warming.org/api/co2-api"
co2_from_json <- fromJSON(co2_url)
co2_dataframe <- as.data.frame(co2_from_json)

## retrieve NO2 data from API
no2_url <- "https://global-warming.org/api/nitrous-oxide-api"
no2_json <- fromJSON(no2_url)
no2_dataframe <- as.data.frame(no2_json)

## retrive average monthly arctic sea ice extent
arctic_sea_extent <- "https://global-warming.org/api/arctic-api"
arctic_sea_extent_json <- fromJSON(arctic_sea_extent)
arctic_sea_dataframe <- as.data.frame(arctic_sea_extent_json$result)
df_arctic_sea_dataframe <- data.frame(arctic_sea_dataframe)


try.ts<- ts(df_arctic_sea_dataframe)
dygraph(try.ts[,3])
dygraph(as.xts(try.ts[,3])) %>%
  dyLimit(label = "horizontal line", color = "red", limit = 4)

## as.Date(df_arctic_sea_dataframe$year)

