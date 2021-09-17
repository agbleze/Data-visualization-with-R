#########################   Basics of web scraping  ############################
# load libraries
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
# set url for online csv file
url <- "https://s3.amazonaws.com/bsp-ocsit-prod-east-appdata/datagov/wordpress/agency-participation.csv"

# use read.csv to import
data_gov <- read.csv(url, stringsAsFactors = FALSE)
head(data_gov)

##### reading excel from online
# the url for the online Excel file
url <- "http://www.huduser.org/portal/datasets/fmr/fmr2015f/FY2015F_4050_Final.xls"

# use read.xls to import
rents <- read.xls(url)
head(rents)


### downloading zip file
#set url 
url <- "http://www.bls.gov/cex/pumd/data/comma/diary14.zip"
download.file(url, destfile = "data_zip", mode = "wb")
unzip_file <- unzip("data_zip")
list.files("./diary14")
unzip_file <- read.csv(unz("data_zip", "diary14/expd141.csv"))
unzip_file

### Extract only needed file from .zip and discard the rest
# create .temp file
temp <- tempfile()

# fetch file into temp file
download.file("http://www.bls.gov/cex/pumd/data/comma/diary14.zip", temp)

# extract target file from temp
extract_data <- read.csv(unz(temp, "diary14/expd141.csv"))
unlink(temp)  ## delete temp file

### download multiple dataset from different links
url <- "http://www.elections.state.md.us/elections/2012/election_data/index.html"
links <- getHTMLLinks(url)

url_new <- "https://elections.maryland.gov/elections/2012/election_data/index.html"

access_link <- getURL(url_new)
get_links <- getHTMLLinks(access_link)
head(get_links)

# detect csv file links
link_data <- get_links[str_detect(get_links, ".csv")]
length(link_data)
## identify first 6 csv files and download
link_data <- head(link_data)
## create loop to download data
for(i in seq_along(link_data)){
  # paste .csv portion of link into base url
  url <- paste0("https://elections.maryland.gov/elections/2012/election_data/", link_data[i])
  
  # download .csv file and save as df
  df <- read.csv(url)
  
  # step 3: rename df
  assign(paste0("df", i), df)
}

sapply(paste0("df", 1:6), exists) ## checks to see if the downloaded files exist
head(df1)

#### scarping html page 
scarping_wiki <- read_html("https://en.wikipedia.org/wiki/Web_scraping")
scarping_wiki %>%
  html_nodes("h1")

#### scarping html text
scarping_wiki %>%
  html_nodes("h2") %>%
  html_text()

## extract paragraphs
p_nodes <- scarping_wiki %>%
  html_nodes("p")
length(p_nodes)
p_nodes[1:6]

## scraping list
ul_text <- scarping_wiki %>%
  html_nodes("ul") %>%
  html_text()
length(ul_text)
ul_text[1]

# read first 200 characters of 3rd lst
substr(ul_text[2], start = 1, stop = 200)

### extratc li nodes
li_text <- scarping_wiki %>%
  html_nodes("li") %>%
  html_text()
length(li_text)
li_text[1:8]
li_text[104:136]

## scraping all text on page can be usually done using the div node
all_text <- scarping_wiki%>%
  html_nodes("div") %>%
  html_text()
length(all_text)

body_text <- scarping_wiki %>%
  html_nodes("#mw-content-text") %>%
  html_text()

### read last 100 characters
last_100characters <- substr(body_text, start = nchar(body_text) - 100, stop = nchar(body_text))
length(last_100characters)

# scarping specific heading
scarping_wiki %>%
  html_nodes("#Techniques") %>%
  html_text()

# scraping specific div
scarping_wiki %>%
  html_nodes("#mw-content-text > div:nth-child(1)") %>%
  html_text()

## cleaning scarped data
body_text %>%
  str_replace_all(pattern = "\n", replacement = " ") %>%
  str_replace_all(pattern = "[\\^]", replacement = " ") %>%
  str_replace_all(pattern = "\"", replacement = " ") %>%
  str_replace_all(pattern = "\\s+", replacement = " ") %>%
  str_trim(side = "both") %>%
  substr(start = nchar(body_text) - 700, stop = nchar(body_text))

##### retrieving html table
webpage <- read_html("http://www.bls.gov/web/empsit/cesbmart.htm")
list_tbls <- html_nodes(webpage, "table") %>%
  .[3:4] %>%
  html_table()
str(list_tbls)

## method 2 to retrive text from table on web
# create empty list
tbls2_ls <- list()

# scrape table 2 and add to list
(tbls2_ls$Table1 <- webpage%>%
  html_nodes("#Table2") %>%
  html_table() %>%
    .[[1]]
  )

## scrape and add another table
(
  tbls2_ls$Table2 <- webpage %>%
    html_nodes("#Table3") %>%
    html_table() %>%
    .[[1]]
)

head(tbls2_ls[[1]], 4)
(tbls2_ls[[1]] <- tbls2_ls[[1]][-1,])

## rename table headings
colnames(tbls2_ls[[1]]) <- c("CES_code", "ind_title", "benchmark", "estimate", "amt_diff", "pct_diff")
head(tbls2_ls[[1]], 4)

##### scraping HTML tables with XML
url <- "https://www.bls.gov/web/empsit/cesbmart.htm"

# parse url
url_parsed<- htmlParse(getURL(url), asText = TRUE)

#select table nodes of interest
tablenodes <- getNodeSet(url_parsed, c('//*[@id="Table2"]','//*[@id="Table3"]', '//*[@id="Table9"]'))

# convert HTML tables to data frames
bls_table2<- readHTMLTable(tablenodes[[1]])
bls_table3 <- readHTMLTable(tablenodes[[2]])
bls_table9 <- readHTMLTable(tablenodes[[3]])
#View(bls_table2)

# specify names of columns when importing readHTMLTable
bls_table2<- readHTMLTable(tablenodes[[1]], header = c("CES_Code", "Ind_Title", "Benchmark", "Estimate", "Amt_Diff", "Pct_diff"))
head(bls_table2)

# specify column classes when importing readHTMLTable
bls_table9 <- readHTMLTable(tablenodes[[3]], colClasses = c("character", "character", rep("integer", 10)))



######## Scraping data using API
# rtimes API
api_key <- "tUIXCud365uQWHHqkNBYy4cwWhWLWMVM"
articles <- as_search(q = "elections", key = api_key)
articles$meta
articles$data$web_url
articles$data$keywords
articles$data[[3]]


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
methane_raw <- content(methane_data, as = 'raw') #retrieve data as ras
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

################################
terra_viva <- read_html("https://terravivagrants.org/funding-news/")
terra_viva %>%
  html_elements("h1")%>%
  html_text()

p_terra <- terra_viva %>%
  html_nodes("p") 
length(p_terra)

paragraph_text_terra <- p_terra %>%
  html_text()
paragraph_text_terra[1]

(list_item_terra <- terra_viva %>%
  html_nodes("ul") %>%
  html_text())
length(list_item_terra)

li_text_terra <- 
  terra_viva %>%
  html_nodes("li") %>%
  html_text()
