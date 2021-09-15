#
# set url
url <- "https://s3.amazonaws.com/bsp-ocsit-prod-east-appdata/datagov/wordpress/agency-participation.csv"

# use read.csv to import
read.csv(url, stringsAsFactors = FALSE)
