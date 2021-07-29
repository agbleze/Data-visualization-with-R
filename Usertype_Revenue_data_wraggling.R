#######  task   ############
## data cleaning and exploration
## wilcox test

library(readr)
library(ggplot2)
library(tidyverse)
library(rcompanion)

usertype_revenue <- read_csv("Downloads/R_shiny_app_GoogleTrends/Seleted dataset/Data_Usertype_Revenue.csv", 
                                  col_types = cols(Revenue = col_character()))
View(usertype_revenue)


######## clean the data and remove symbols from the rows and leave only numbers and characters
usertype_revenue$Revenue <- substring(usertype_revenue$Revenue, first = 2) # subset characters beginning from 1nd string to take out $ sign
View(usertype_revenue)

# Dataset$`Ecommerce Conversion Rate` <- substr(Dataset$`Ecommerce Conversion Rate`, 1, nchar(Dataset$`Ecommerce Conversion Rate`) - 1) # retains 1st to 4th characters and leaveout % sign
# Dataset$`Bounce Rate` <- substr(Dataset$`Bounce Rate`, 1, nchar(Dataset$`Bounce Rate`) - 1) # retains 1st to 4th characters and leaveout % sign
# View(Dataset)
########

#extract first 5 chars
char_first5 <- substr(usertype_revenue$`Day Index`, 1, 5)  ## use for Day Index col chracters of length 7

# extarct first 6 chars
char_first6 <- substr(usertype_revenue$`Day Index`, 1, 6)  ## use for Day index col characters of length 8

#extract last 2 chars
char_last2 <- substring(usertype_revenue$`Day Index`, first = nchar(usertype_revenue$`Day Index`) - 1) ## concatenate at the end of values in Day Index colume
char_last2

################ Formating Day Index col with 7 characters into Date format ####################
filter_char7 <- filter(usertype_revenue, nchar(usertype_revenue$`Day Index`) == 7) ## filter Day Index col with 7 strings
filter_char7_first5 <- substr(filter_char7$`Day Index`, 1, 5)  ## Extract 1st 5 strings from col 
filter_char7_last2 <- substring(filter_char7$`Day Index`, first = nchar(filter_char7$`Day Index`) - 1) #Extract last 2 strings of dataset in Day Index colume

filter_char7Re <- mutate(filter_char7, fill_char7Date = paste0("0", filter_char7_first5, "20", filter_char7_last2)) #create col and concatenate strings into it to form Date
colnames(filter_char7Re)[5] <- "Date" ## rename col as Date
View(filter_char7Re)

View(filter_char7)
View(filter_char7_last2)

################ Formating Day Index col with 8 characters into Date format ####################
filter_char8 <- filter(usertype_revenue, nchar(usertype_revenue$`Day Index`) == 8) ## filter Day Index col with 8 strings
filter_char8_first6 <- substr(filter_char8$`Day Index`, 1, 6)  ## Extract 1st 6 strings from col 
filter_char8_last2 <- substring(filter_char8$`Day Index`, first = nchar(filter_char8$`Day Index`) - 1) #Extract last 2 strings of dataset in Day Index colume

filter_char8Re <- mutate(filter_char8, fill_char8Date = paste0(filter_char8_first6, "20", filter_char8_last2)) #create col and concatenate strings into it to form Date
colnames(filter_char8Re)[5] <- "Date" ## rename col as Date

View(filter_char8)
View(filter_char8Re)
colnames(filter_char)

################ Formating Day Index col with 10 characters into Date format ####################
filter_char10 <- filter(usertype_revenue, nchar(usertype_revenue$`Day Index`) == 10) ## filter Day Index col with 10 strings
filter_char10["Date"] <- filter_char10$`Day Index` ## create Date col and assign the values of Day Index col to it

########binding rows of datasets  ##############################################
GA_userType_Revenue_Dataset <- bind_rows(filter_char7Re, filter_char8Re, filter_char10) ## bind rows of all datasets filtered
View(GA_userType_Revenue_Dataset)

#### Exporting dataset as csv
write.csv(GA_userType_Revenue_Dataset, file = "Users\\lin\\Downloads\\R_shiny_app_GoogleTrends\\GA_userType_Revenue_Dataset.csv")


################## data exploration ####################
View(GA_userType_Revenue_Dataset)

GA_userType_Revenue_Dataset <- read_csv("GA_userType_Revenue_Dataset.csv", 
                                        col_types = cols(Date = col_date(format = "%m/%d/%Y")))
#View(GA_userType_Revenue_Dataset)

#ggplot(data = GA_userType_Revenue_Dataset, aes(x = Segment, y = "Revenue")) + geom_histogram()
table(GA_userType_Revenue_Dataset$Segment)
data_usertype <- GA_userType_Revenue_Dataset
ggplot(data = GA_userType_Revenue_Dataset, aes(x = Segment)) + geom_bar()
summary(data_usertype$Revenue)
quantile(data_usertype$Transactions)
summary(data_usertype$Transactions)
var(data_usertype$Transactions)
View(data_usertype)

ggplot(data_usertype, aes(x = Segment,y = Revenue)) + geom_boxplot()
ggplot(data_usertype, aes(x = Segment, y = Transactions)) + geom_boxplot() + coord_flip()
ggplot(data_usertype, aes(Transactions)) + geom_histogram()
ggplot(data_usertype, aes(Revenue)) + geom_histogram()

(transaction_ttest <- t.test(Transactions ~ Segment, data = data_usertype)) ## T test for mean of transactions between 
(revenue_ttest <- t.test(Revenue ~ Segment, data = data_usertype))

## wilcon test for testing mean difference for non-normal distribution
(transaction_wilcox <- wilcox.test(Transactions ~ Segment, data = data_usertype))

(revenue_wilcox <- wilcox.test(Revenue ~ Segment, data = data_usertype))

plotNormalDensity(data_usertype$Revenue)
transformTukey(data_usertype$Revenue)

outliers::outlier(data_usertype$Transactions)
outliers::outlier(data_usertype$Revenue)
outliers::outlier(data_usertype$Transactions, opposite = TRUE)

# shapiro.test(data_usertype$Transactions)
# shapiro.test(transactions_transform)
# leveneTest(y = data_usertype$Transactions, group = data_usertype$Segment, data = data_usertype)
# fligner.test(x = data_usertype$Transactions, group = data_usertype$Segment, data = data_usertype )

plotNormalHistogram(data_usertype$Transactions)

(transactions_transform <- rcompanion::transformTukey(data_usertype$Transactions))

(transboxplot <- boxplot(data_usertype$Transactions))
(outlierrem <- rm.outlier(data_usertype[,5]))
View(outlierrem)
boxplot(outlierrem$Revenue)
plotDensityHistogram(outlierrem$Revenue)
