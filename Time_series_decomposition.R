###### time series decomposition  ##########
library(ggplot2)
library(forecast)
library(seasonal)

autoplot(elecsales) + xlab("year") + ylab("GWh") + 
  ggtitle("Annual elec sales: South Australia")

### Moving Average
ma(elecsales, 5)
elecsales

##### plot data with MA
autoplot(elecsales, series = "Data") + 
  autolayer(ma(elecsales, 5), series = "5-MA") +
  xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia") +
  scale_colour_manual(values = c("Data" = "grey50", "5-MA" = "red"),
                      breaks = c("Data", "5-MA"))

########
beer3 <- window(ausbeer, start = 1992)
(ma4 <- ma(beer3, order = 4, centre = FALSE))
(ma2x4 <- ma(beer3, order = 4, centre = TRUE))

##########
autoplot(elecequip, series = "Data") +
  autolayer(ma(elecequip, 12), series = "12-MA") + xlab("year") +
  ylab("New orders index") +
  ggtitle("Electrical equipment manifacturing (Euro area)") +
  scale_colour_manual(values = c("Data" = "grey", "12-MA" = "red"),
                      breaks = c("Data", "12-MA"))

######
elecequip %>%
  decompose(type = "multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition of electrical equip index")

####### decomposing with x-11
elecequip %>%
  seas(x11 = "") -> fitdecompose
autoplot(fitdecompose) +
  ggtitle("X11 decomposition of electrical equipment index")

## seasonal adjusted and trendcycle from timeseries decomposition
autoplot(elecequip, series = "Data") +
  autolayer(trendcycle(fitdecompose), series = "Trend") +
  autolayer(seasadj(fitdecompose), series = "Seasonally Adjusted") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values = c("gray", "blue", "red"),
                      breaks = c("Data", "Seasonally Adjusted", "Trend"))

## seasonal subseries plot from seasonal component
fitdecompose %>%
  seasonal() %>%
  ggsubseriesplot() + ylab("seasonal")

##STL (Seasonal Trend decomposition with Loess) decomposition
elecequip %>%
  stl(t.window = 13, s.window = "periodic", robust = TRUE)%>%
  autoplot()

### forecasting with decomposition
(stldecompose_fit <- stl(elecequip, t.window = 13, s.window = "periodic",
                        robust = TRUE))
stldecompose_fit%>%
  seasadj() %>%
  naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

stldecompose_fit%>%
  forecast(method = "naive")%>%
  autoplot() + ylab("New orders index")

## short approach to forecasting with stl
stlforecast <- stlf(elecequip, method = "naive") %>%
  autoplot() + ggtitle("stl based forecast") + ylab("")
