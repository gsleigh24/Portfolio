library(quantmod)
library(tseries)
library(caTools)
library(forecast)

wbc_data <- getSymbols (Symbols = "WBC.AX", src = "yahoo", from = "01-01-2015", to = Sys.Date(), auto.assign =  FALSE) 
wbc_close <- wbc_data[,4]
wbc_close <- na.omit(wbc_close)
wbc_log <- log(wbc_close)

chart_Series(wbc_close, col = "black")
  add_SMA(n = 100, on = 1, col = "red")
  add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)

ts_wbc_log <- ts(wbc_log)
acf_log <- acf(wbc_log)
pacf_log <- pacf(wbc_log)

adf_wbc <- adf.test(wbc_log, alternative = "stationary", k = 0)

wbc_diff <- diff(wbc_log, lag = 1)
  wbc_diff <- na.omit(wbc_diff)

adf_wbc_diff <- adf.test(wbc_diff, alternative = "stationary", k = 0)
    show(adf_wbc_diff)
  acf_wbc_diff <- acf(wbc_diff)
  pacf_wbc_diff <- pacf(wbc_diff)
  
training_set <- wbc_diff[1:7888,]

auto_arima_wbc <- auto.arima(training_set, stationary = TRUE, ic = "aic", trace = TRUE)
  summary(auto_arima_wbc)
  checkresiduals(auto_arima_wbc)
  
arima_wbc <- arima(wbc_log[1:7888,], order = c(2,0,4))
  summary(arima_wbc)
  
forecast_wbc <- forecast(arima_wbc, h = nrow(wbc_diff)-nrow(training_set))
  autoplot(forecast_wbc) + autolayer(ts_wbc_log)
  