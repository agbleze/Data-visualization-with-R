library(forecast)
library(fpp2)


############ linear regression of time series  #########
## scatter plot of data
uschange%>%
  as.data.frame()%>%
  ggplot(aes(x = Income, y = Consumption)) +
  geom_point() + geom_smooth(method = "lm") +
  ylab("Consumption (quarterly)") + xlab("income (quarterly)")

## simple linear regression
tslm(Consumption ~ Income, data = uschange)

## scatter plot matrix
uschange%>%
  as.data.frame()%>%
  ggpairs() + theme_grey()

## multiple linear regression
(fit.consMR <- tslm(
  Consumption ~ Income + Production + Unemployment + Savings,
  data = uschange
))
summary(fit.consMR)

# plot fitted model over data
autoplot(uschange[, 'Consumption'], series = 'Data') +
  autolayer(fitted(fit.consMR), series = "Fitted") +
  xlab("Year") + ylab("") + 
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour = guide_legend(title = ""))

## add fitted model to data columns 
cbind(Data = uschange[, "Consumption"],
      Fitted = fitted(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted)) + geom_point() +
  ylab("Fitted (predicted values)") + 
  xlab("Data (actual values)") +
  ggtitle("Percent change in US consumption expenditure") +
 geom_abline(intercept = 0, slope = 1)

### checkresiduals() to crosscheck residuals meet assumptions
checkresiduals(fit.consMR)


##### residual plots on predictors
df <- as.data.frame(uschange)
df[,"Residuals"]  <- as.numeric(residuals(fit.consMR))
p1 <- ggplot(df, aes(x=Income, y=Residuals)) +
  geom_point()
p2 <- ggplot(df, aes(x=Production, y=Residuals)) +
  geom_point()
p3 <- ggplot(df, aes(x=Savings, y=Residuals)) +
  geom_point()
p4 <- ggplot(df, aes(x=Unemployment, y=Residuals)) +
  geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

### Residuals plot agains fitted values
cbind(Fitted = fitted(fit.consMR),
      Residuals = residuals(fit.consMR)) %>%
  as.data.frame()%>%
  ggplot(aes(x = Fitted, y = Residuals)) + geom_point()

### spurious regresion
aussies <- window(ausair, end = 2011)
fit <- tslm(aussies ~ guinearice)
summary(fit)
checkresiduals(fit)


#### dummy var
beer2 <- window(ausbeer, start = 1992)
autoplot(beer2) + xlab("Y") + ylab("m")
fit.beer <- tslm(beer2 ~ trend + season)
summary(fit.beer)

autoplot(beer2, series = "Data") +
  autolayer(fitted(fit.beer), series = "Fitted") +
  xlab("y") + ylab("m") + ggtitle("Quarterly")

cbind(Data = beer2, Fitted = fitted(fit.beer)) %>%
  as.data.frame()%>%
  ggplot(aes(x = Data, y = Fitted, colour = as.factor(cycle(beer2)))) +
  geom_point() + ylab("Fitted") + xlab("Actual values") +
  scale_colour_brewer(palette = "Dark2", name = "Quarter") +
  geom_abline(intercept = 0, slope = 1)

## using fourier() to compute dummy for season period in data
fourier.beer <- tslm(beer2 ~ trend + fourier(beer2, K = 2))
summary(fourier.beer)

##########selecting predictors  ###############
## measure of predictive accuracy  ########
CV(fit.consMR)


###ex-ante forecast
beers <- window(ausbeer, start = 1992)
fit.beers <- tslm(beers ~ trend + season)
fcast <- forecast(fit.beers)
autoplot(fcast) 

#### scenario-based forecast
## fit regression
fit.scenario <- tslm(
  Consumption ~ Income + Savings + Unemployment, data = uschange
)  

# time step to forecast
h <- 4

# scenario of constaint ncrease in Income and savivings rate and no increase on unempl
newdata <- data.frame(
  Income = c(1,1,1,1),
  Savings = c(0.5,0.5,0.5,0.5),
  Unemployment = c(0,0,0,0)
)

# generate forecast with scenario 1
fcast.up <- forecast(fit.scenario, newdata = newdata)

# scenario 2 data with decrease in income rate and savings for same time step forecast
newdata <- data.frame(
  Income = rep(-1, h),
  Savings = rep(-0.5, h),
  Unemployment = rep(0, h)
)
# forecast for scenario 2
fcast.down <- forecast(fit.scenario, newdata = newdata)

##plot 
autoplot(uschange[,1]) + ylab("% change in US consumption") +
  autolayer(fcast.up, PI = TRUE, series = "increase") +
  autolayer(fcast.down, PI = TRUE, series = "decrease") +
  guides(colour = guide_legend(title = "scenario"))


#### estimating the prediction interval when income increase by historical mean of 0.72 vs 5% increase
fit.cons <- tslm(Consumption ~ Income, data = uschange)
h <- 4
fcast.ave <- forecast(fit.cons,
                      newdata = data.frame(
                        Income = rep(mean(uschange[, "Income"]), h)
                      ))
fcast_cons.up <- forecast(fit.cons, newdata = data.frame(Income = rep(5, h)))

autoplot(uschange[, "Consumption"]) + ylab("% change in US consumption") +
  autolayer(fcast.ave, series = "Avergae Increae", PI = TRUE) +
  autolayer(fcast.up, series = "Extreme increase", PI = TRUE) +
  guides(colour = guide_legend(title = "Scenario"))



######## forecasts from linear, exponential, piecewise, linear trends and cubic spline
boston_men <- window(marathon, start = 1924)  ## data
h <- 10  ## time step to forecast
fit.lin <- tslm(boston_men ~ trend) ## linear reg
fcasts.lin <- forecast(fit.lin, h = h) ## forecast with linear reg
fit.exp <- tslm(boston_men ~ trend, lambda = 0)  ## exponential reg
fcasts.exp <- forecast(fit.exp, h = h)  ## forecast with exponential ref

## parameters for estimating piecewise and spline regression
t <- time(boston_men)
t.break1 <- 1950
t.break2 <- 1980
tb1 <- ts(pmax(0, t - t.break1), start = 1924)
tb2 <- ts(pmax(0, t - t.break2), start = 1924)

fit.pw <- tslm(boston_men ~ t + tb1 + tb2) ## piecewise linear reg
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h)
tb2.new <- tb2[length(tb2)] + seq(h)

newdata <- cbind(t = t.new, tb1 = tb1.new, tb2 = tb2.new) %>%
  as.data.frame()

fcasts.pw <- forecast(fit.pw, newdata = newdata)  ## forecast with piecewise reg

fit.spline <- tslm(boston_men ~ t + I(t^2) + I(t^3) + 
                     I(tb1^3) + I(tb2^3)) ### cubic spline reg

fcasts.spl <- forecast(fit.spline, newdata = newdata)  ## forecast with cubic spline regression

autoplot(boston_men) + 
  autolayer(fitted(fit.lin), series = "Linear") +
  autolayer(fitted(fit.exp), series = "Expotential") +
  autolayer(fitted(fit.pw), series = "Piecewise") +
  autolayer(fitted(fit.spline), series = "Cubic Spline") +
  autolayer(fcasts.pw, series = "Piecewise") +
  autolayer(fcasts.lin, series = "Linear", PI = FALSE) +
  autolayer(fcasts.exp, series = "Expotential", PI = FALSE) +
  autolayer(fcasts.spl, series = "Cubic Spline", PI = FALSE) +
  xlab("Year") + ylab("Winning times in minutes") +
  ggtitle("Boston Marathon") +
  guides(colour = guide_legend(title = ""))

### splinef() for cubic spline forecasts
## natual cubic smoothing splines regre and forecast
boston_men %>%
  splinef(lambda = 0) %>% #log transformation to handle hetereoscedasticity
  autoplot()

### check residuals
boston_men %>%
  splinef(lambda = 0) %>%
  checkresiduals()
