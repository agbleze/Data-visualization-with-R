######### exponential smoothing  ####################
library(tidyverse)
library(fpp2)
library(gridExtra)

##### create training and testing data
goog.train <- window(goog, end = 900)
goog.test <- window(goog, start = 901)

#### training and tesing data for cement data
qcement.train <- window(qcement, end = c(2012, 4))
qcement.test <- window(qcement, start = c(2013, 1))

## simple exponential smoothing (ses)
## use ses() for data with no trend and no season
## create differenced data to remove trend from trended data
goog.diff <- diff(goog.train)
autoplot(goog.diff)

# crrate ses forecast model
ses.goog.dif <- ses(goog.diff, alpha = .2, h = 100)
autoplot(ses.goog.dif)

### testing the model on validation dataseet
# crate differrenced validation dataste
goog.diff.test <- diff(goog.test)
accuracy(ses.goog.dif, goog.diff.test) ## test training model on test data

### identify optimal parameter
alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)){
  fit <- ses(goog.diff, alpha = alpha[i], h = 100)
  RMSE[i] <- accuracy(fit, goog.diff.test)[2,2]
}

## convert to dataframe and identify min apha val
alpha.fit <- data.frame(alpha, RMSE)
alpha.min <- filter(alpha.fit, RMSE == min(RMSE))

## plot RMSE vs. alpha
ggplot(alpha.fit, aes(alpha, RMSE)) + geom_line() +
  geom_point(data = alpha.min, aes(alpha, RMSE), size = 2, color = "blue")

## refit model with alpha = 0.05
ses.goog.opt <- ses(goog.diff, alpha = .05, h = 100)

## perofrmance level
accuracy(ses.goog.opt, goog.diff.test)

## plot results
p1 <- autoplot(ses.goog.opt) + theme(legend.position = "bottom")
p2 <- autoplot(goog.diff.test) + 
  autolayer(ses.goog.opt, alpha = .5) +
  ggtitle("Predicted vs. actuals for the test data set")

gridExtra::grid.arrange(p1, p2, nrow = 1)

## Holt method of exponential smoothing for trended data
holt.goog <- holt(goog.train, h = 100)
autoplot(holt.goog, level = T)
holt.goog$model
## check predictive accuracy of model
accuracy(holt.goog, goog.test)

### identify optimal alpha valua
beta <- seq(.0001, .5, by = .001)
RMSE <- NA
for(i in seq_along(beta)){
  fit <- holt(goog.train, beta = beta[i], h = 100)
  RMSE[i] <- accuracy(fit, goog.test)[2,2]
}

# convert to df and identify min alpha value
beta.fit <- data_frame(beta, RMSE)
beta.min <- filter(beta.fit, RMSE == min(RMSE))

## plot RMSE vs. alpha
ggplot(beta.fit, aes(beta,RMSE)) + 
  geom_line() +
  geom_point(data = beta.min, aes(beta, RMSE), size = 2, color = "blue")

##### new omdel with optimal beta
holt.goog.opt <- (holt(goog.train, h = 100, beta = 0.0231))

# accuracy of the first model
accuracy(holt.goog, goog.test)

## accuracy of optimal model
accuracy(holt.goog.opt, goog.test)

p1 <- autoplot(holt.goog) +
  ggtitle("Original Holt's Model") +
  coord_cartesian(ylim = c(400, 1000))

p2 <- autoplot(holt.goog.opt) +
  ggtitle("Optimal Holt's Model") +
  coord_cartesian(ylim = c(400, 1000))

gridExtra::grid.arrange(p1, p2, nrow = 1)


##### Holt-Winters method for forecasting on seasonal data
autoplot(decompose(qcement))

## fit holt-winters using ets(), Error, trend, seasonality
qcement.hw <- ets(qcement.train, model = "AAA") # model = addictive error, addictive trend, addictive seasonality
autoplot(forecast(qcement.hw))
summary(qcement.hw)
# check residuals
checkresiduals(qcement.hw)

## forecast next 5 quaters
qcement.f1 <- forecast(qcement.hw, h = 5)
#check accuracy
accuracy(qcement.f1, qcement.test)

######## multiplicative 
qcement.hw2 <- ets(qcement.train, model = "MAM")
checkresiduals(qcement.hw2)

### compare different models 
# addictive error, trend and seasonality
qcement.hw1 <- ets(qcement.train, model = "AAA")
qcement.f1 <- forecast(qcement.hw1, h = 5)
accuracy(qcement.f1, qcement.test)

# multiplicative error, addicitve trend and seasonality
qcement.hw2 <- ets(qcement.train, model = "MAA")
qcement.f2 <- forecast(qcement.hw2, h = 5)
accuracy(qcement.f2, qcement.test)

## addicitove error and trend and multiplicative seasonality
qcement.hw3 <- ets(qcement.train, model = "AAM", restrict = FALSE )
qcement.f3 <- forecast(qcement.hw3, h = 5)
accuracy(qcement.f3, qcement.test)

## multiplicative error, additive trend, and multiplicative
qcement.hw4 <- ets(qcement.train, model = "MAM")
qcement.f4 <- forecast(qcement.hw4, h = 5)
accuracy(qcement.f4, qcement.test)

qcement.hw5 <- ets(qcement.train, model = "ZZZ")
summary(qcement.hw5)

## finding the optimal gamma value that minimizes forecast error
gamma <- seq(0.01, 0.85, 0.01)
RMSE <- NA

for(i in seq_along(gamma)){
  hw.expo <- ets(qcement.train, "AAA", gamma = gamma[i])
  future <- forecast(hw.expo, h = 5)
  RMSE[i] = accuracy(future, qcement.test)[2,2]
}

error <- data_frame(gamma, RMSE)
minimum <- filter(error, RMSE == min(RMSE))
ggplot(error, aes(gamma, RMSE)) +
  geom_line() +
  geom_point(data = minimum, color = "blue", size = 2) +
  ggtitle("gamma's impact on forecast errors", subtitle = "gamma = 0.21 minimizes RMSE")


## previous model with additive error, trend and seasonality
accuracy(qcement.f1, qcement.test)

# new model with optimal gamma parameter
qcement.hw6 <- ets(qcement.train, model = "AAA", gamma = 0.21)
qcement.f6 <- forecast(qcement.hw6, h = 5)
accuracy(qcement.f6, qcement.test)
qcement.f6
autoplot(qcement.f6)


#### damping when forecast trend are expected to flatten out and conservative values
## holt linear (additive) model
fit1 <- ets(ausair, model = "ZAN", alpha = 0.8, beta = 0.2)
pred1 <- forecast(fit1, h = 5)

## holt's linear (additive) model
fit2 <- ets(ausair, model = "ZAN", damped = TRUE, alpha = 0.8, beta = 0.2, phi = 0.85)
pred2 <- forecast(fit2, h = 5)

# holt's exponential (multiplicative) model
fit3 <- ets(ausair, model = "ZMN", alpha = 0.8, beta = 0.2)
pred3 <- forecast(fit3, h = 5)

# holt's exponential (multiplicative) model damped
fit4 <- ets(ausair, model = "ZMN", damped = TRUE, alpha = 0.8, beta = 0.2, phi = 0.85)
pred4 <- forecast(fit4, h = 5)

autoplot(ausair) +
  autolayer(pred1$mean, color = "blue") +
  autolayer(pred2$mean, color = "blue", linetype = "dashed") +
  autolayer(pred3$mean, color = "red") +
  autolayer(pred4$mean, color = "red", linetype = "dashed")




