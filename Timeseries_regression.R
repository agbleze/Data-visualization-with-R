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
