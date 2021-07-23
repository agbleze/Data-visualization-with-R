### time series forecasting
# load libraries
library(forecast)
library(fpp2)
library(tidyverse)

AirPassengers
str(AirPassengers)
glimpse(AirPassengers)
summary(AirPassengers)
pass.ts <- AirPassengers
autoplot(pass.ts)

arrivals
autoplot(arrivals)
autoplot(arrivals, facet = T)

## getting stats
us <- arrivals[,"US"]
summary(us)
frequency(us)

autoplot(qcement)

## seasonal plots
ggseasonplot(qcement, year.labels = FALSE, continuous = T)

ggseasonplot(a10, year.labels.left = F, continuous = T)
ggseasonplot(a10, year.labels = F, continuous = T, polar = T)
ggsubseriesplot(qcement)

#### autoplot beer data
autoplot(ausbeer)
gglagplot(ausbeer) ## lag plot
ggAcf(ausbeer) ## ACF Autocorrelation Function plot
gglagplot(ausbeer, set.lags = 1:16)

acf(ausbeer, plot = F)
autoplot(AirPassengers)
ggAcf(AirPassengers)
autoplot(USAccDeaths)
ggAcf(USAccDeaths)

### a random and identical distribution without autocorrelation of lags
set.seed(3)
wn <- ts(rnorm(36))
autoplot(wn)
ggAcf(wn)

### Acf plots test if individual lag autocorrelation is different than 0
pigs
pigs.ts <- ts(pigs[121:188], start = c(1990, 1), frequency = 12)
autoplot(pigs.ts)
ggAcf(pigs.ts)

## Ljung-Box test to determine if group of autocorrelations of time series are different from zero
Box.test(pigs, lag = 24, fitdf = 0, type = "Lj")

autoplot(goog)
View(goog)
(goog_day <- diff(goog))
autoplot(goog_day)
Acf(goog_day)
ggAcf(goog_day)
Box.test(goog_day, lag = 10, fitdf = 0, type = "Lj")

#############forecasting with simple methods (Benchmarking) ##########
## set training data 1992 -2007
beer2 <- window(ausbeer, start = 1992, end = c(2007, 4))
## simple forecasting methods
autoplot(beer2) + autolayer(meanf(beer2, h = 11), series = "Mean", PI = F) + ## forecast with average method
  autolayer(naive(beer2, h = 11), series = "Naive", PI = F) +  # forecast with naive method
  autolayer(snaive(beer2, h = 11), series = "seasonal naive", PI = F) + ## forecast with seasonal naive
  ggtitle("forecast for quarterly beer production") + xlab("year") + ylab("megalitres") +
  guides(colour = guide_legend(title = "forecast"))

### Forecast with drift method included 
autoplot(goog200) + autolayer(meanf(goog200, h = 40), series = "Mean", PI = F) +
  autolayer(rwf(goog200, h = 40), series = "Naive", PI = F) +
  autolayer(rwf(goog200, h = 40, drift = T), series = "Drift", PI = F) + 
  ggtitle("Google stock daily ending") + guides(colour = guide_legend(title = "Forecast", title.position = "left"))+
  xlab("day") + ylab("closing price (US$)")

############Transformation and adjustment ####################
## calendar adjustment
dframe <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))
autoplot(dframe, facet = T) + 
  xlab("years") + ylab("Pounds") + ggtitle("Milk production per cow")

## adjusting for bias dring back transformation
fc <- rwf(eggs, drift = T, lambda = 0, H = 50, level = 80) 
fc2 <- rwf(eggs, drift = T, lambda = 0, H = 50, level = 80, biasadj = T)
autoplot(eggs) +
  autolayer(fc, series = "simple back transformation") +
  autolayer(fc2, series = "bias adjusted", PI = F)

### ggogle stock price 
autoplot(goog200) + xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("google stock (daily ending 6 dec 2013")

## residuals for naive mean forcasting method####
res <- residuals(naive(goog200))
summary(res)

#tidy(res)
## plot of residuals
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naive method")

## residuals histogram
gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")

### residuals for Average mean forecasting method
# mean_res <- residuals(meanf(goog200))
# ggAcf(mean_res) + ggtitle("ACF of residuals of Mean forecats")
# summary(mean_res)
# 
# ### residuals for drift forecasting method
# drift_res <- residuals(rwf(goog200, drift = T))
# (drift_res_plot <- autoplot(drift_res))
# ggAcf(drift_res) + ggtitle("ACF of residuals of Drift forecats")
# summary(drift_res)

## portmanteau test for autocorrelation
# lag = h and fitdf = K
Box.test(res, lag = 10, fitdf = 0) # Box-Pierce test
Box.test(res, lag = 10, fitdf = 0, type = "Lj") # Box-Ljung test

# use checkresiduals() for portmanteau test
checkresiduals(naive(goog200)) # plot time plot, ACF, histograme and Ljung-box test

### subsetting time series for training and test set
trial <- window(ausbeer, start = 1995) ## extract all data from 1995 onwards
#extrcat last 5 years
use_subset <- subset(ausbeer, start = length(ausbeer)-4*5)
ex_quarter <- subset(ausbeer, quarter = 1)

####### evaluate forecast accuracy  ###################
beer2 <- window(ausbeer, start = 1992, end = c(2007, 4))
beerfit1 <- meanf(beer2, h = 10)
beerfit2 <- rwf(beer2, h = 10)
beer3 <- snaive(beer2, h = 10)
autoplot(window(ausbeer, start = 1992)) +
  autolayer(beerfit1, series = "Mean", PI = F) +
  autolayer(beerfit2, series = "Naive", PI = F) +
  autolayer(beer3, series = "Seasonal naive", PI = F) +
  xlab("Year") + ylab("Megalitres") + ggtitle("Forecasts for quarterly beer production") +
  guides(colour = guide_legend(title = "Forecast"))

beer2008 <- window(ausbeer, start = 2008)
accuracy(beerfit1, beer2008)
accuracy(beerfit2, beer2008)
accuracy(beer3, beer2008)

####### google stock forecast
googfc1 <- meanf(goog200, h = 40)
googfc2 <- rwf(goog200, h = 40)
googfc3 <- rwf(goog200, h = 40, drift = T)
autoplot(subset(goog, end = 240)) + 
  autolayer(googfc1, PI = F, series = "Mean") +
  autolayer(googfc2, PI = F, series = "Naive") +
  autolayer(googfc3, PI = F, series = "Drift") +
  xlab("Day") +ylab("closing price (US$)") +
  guides(colour = guide_legend(title = "Forecast")) +
  ggtitle("google stock price")
### evaluate forecast
googtest <- window(goog, start = 201, end = 240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)

##### time series cross validation  ########
e <- tsCV(goog200, rwf, drift = TRUE, h = 1)
sqrt(mean(e^2, na.rm = T)) #RMSE from time series cross-validation
sqrt(mean(residuals(rwf(goog200, drift = T))^2, na.rm = T)) #RMSE

#### time series cross validation
e <- tsCV(goog200, forecastfunction = naive, h = 8)
#compute MSE
mse <- colMeans(e^2, na.rm = T)
#plot MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse)%>%
  ggplot(aes(x = h, y = MSE)) + geom_point()

### bootsrap prediction interval
naive(goog200, bootstrap = TRUE)
