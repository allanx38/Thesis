library(TTR)

Mkt = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2010_ts.csv")
Mkt_ts <- ts(Mkt$Close,frequency=252)
plot.ts(Mkt_ts)

Mkt_ts_comp <- decompose(Mkt_ts)
plot(Mkt_ts_comp)
plot(Mkt_ts -Mkt_ts_comp$seasonal)

log_Mkt_ts <- log(Mkt_ts)
plot.ts(log_Mkt_ts)


Mkt_ts_SMA3 <- SMA(Mkt_ts,n=10)
plot.ts(Mkt_ts_SMA3)

Mkt_ts_forecasts <- HoltWinters(Mkt_ts, beta=FALSE, gamma=FALSE)
Mkt_ts_forecasts$fitted
plot(Mkt_ts_forecasts)

rainseriesforecasts$SSE

acf(rainseriesforecasts2$residuals, lag.max=20)

Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")

plot.ts(rainseriesforecasts2$residuals)