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
