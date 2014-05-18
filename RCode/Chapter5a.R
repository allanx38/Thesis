
library(TTR)

Mkt = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv")
head(Mkt)

# gener ts
Mkt_ts <- ts(Mkt$Close,frequency=252)
plot.ts(Mkt_ts)

Mkt_ts_nf <- ts(Mkt$Close)
plot.ts(Mkt_ts_nf)

# time series that can be described using an additive model
# decompose
Mkt_ts_comp <- decompose(Mkt_ts)
Mkt_ts_comp$seasonal
Mkt_ts_comp$figure
Mkt_ts_comp$type
plot(Mkt_ts_comp)
plot(Mkt_ts -Mkt_ts_comp$seasonal)

log_Mkt_ts <- log(Mkt_ts)
plot.ts(log_Mkt_ts)

Mkt_ts_SMA3 <- SMA(Mkt_ts,n=10)
plot.ts(Mkt_ts_SMA3)

# 2.5.1 Simple Exponential Smoothing
#To use HoltWinters() for simple exponential smoothing, we need
#to set the parameters beta=FALSE and gamma=FALSE
Mkt_ts_forecasts <- HoltWinters(Mkt_ts, beta=FALSE, gamma=FALSE)
Mkt_ts_forecasts

# a. fitted -> the forcasts
tail(Mkt_ts_forecasts$fitted)
plot(Mkt_ts_forecasts)

# b. sum-ofsquared-errors
Mkt_ts_forecasts$
Mkt_ts_forecasts$SSE

# c. with l.start
Mkt_ts_forecasts2 <- HoltWinters(Mkt_ts, beta=FALSE, gamma=FALSE, l.start=6750.76)
tail(Mkt_ts_forecasts$fitted)

# d. forecasting
# forecast package needed
library(forecast)
Mkt_ts_forecasts3 <- forecast.HoltWinters(Mkt_ts_forecasts2, h=1)
tail(Mkt_ts)
Mkt_ts_forecasts3
plot.forecast(Mkt_ts_forecasts3)

# e1. residuals
acf(Mkt_ts_forecasts3$residuals, lag.max=20)

# e2. residuals
Box.test(Mkt_ts_forecasts3$residuals, lag=20, type="Ljung-Box")
plot.ts(Mkt_ts_forecasts3$residuals)

# f. normal spread or errors
plotForecastErrors <- function(forecasterrors)

# 2 Holt's exponential smoothing
#If you have a time series that can be described using an additive model with increasing or decreasing trend and no
#seasonality, you can use Holt's exponential smoothing to make short-term forecasts.
#To use HoltWinters() for
32 Chapter 2. Using R for Time Series Analysis
A Little Book of R For Time Series, Release 0.2
Holts exponential smoothing, we need to set the parameter gamma=FALSE

# Exp smoothing
test = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2013_ts.csv")
train = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2010_13_ts.csv")

head(Mkt)
#test_ts <- ts(test$Close,frequency=252)
#train_ts <- ts(train$Close,frequency=252)
test_ts <- ts(test$Close)
train_ts <- ts(train$Close,frequency=252)

plot.ts(train_ts)
train_ts_comp <- decompose(train_ts)
plot(train_ts_comp)

Mkt_ts_nf <- ts(Mkt$Close)
fit <- ets(Mkt_ts_nf, model='ANN', damped=FALSE)
fit_auto <- ets(train_ts)

?forecast
fcast <- forecast(fit, h=5)
plot(fcast, Mkt_ts_nf)
fcast$model
fcast$mean
fcast$level
fcast$x
fcast$upper
fcast$fitted
head(fcast$fitted)

tail(fcast$fitted)
tail(Mkt)
cc <- fcast$fitted
class(cc)
length(cc) ; length(Mkt)

fcast2 <- forecast(fit_auto)
plot(fcast2)
test_fcast <- ets(test_ts, model = fit_auto)
accuracy(test_fcast)
aa <- test_fcast$fitted

test_ts <- cbind(test_ts, aa)
tail(test_ts)
head(test_ts)
test_ts

Mkt = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_ts.csv")
Mkt_ts <- ts(Mkt$Close, frequency = 252, start=c(2000,1))
tail(Mkt_ts)
plot.ts(Mkt_ts)
Mkt_train <- window(Mkt_ts, start=2000, end=2009.99)
Mkt_test <- window(Mkt_ts, start=2010)
plot.ts(Mkt_train)
plot.ts(Mkt_test)

# base systems
# 1. mean
mean_model <- meanf(Mkt_train, h=5)
accuracy(mean_model)
accuracy(mean_model, Mkt_test) #out of sample

#2. naive
naive_model <- naive(Mkt_train, h=5)
summary(naive_model)
plot(naive_model)
a <- accuracy(naive_model, Mkt_test) #out of sample
class(a)
a[, c(2,3,4)]

?forecast.ts

Mkt_pl <- window(Mkt_ts, start=2006, end=2006.75)
plot.ts(Mkt_pl,
        main="Dax in 2006",
        xlab="Day", ylab="",
        xlim=c(2006, 2006.9))
lines(meanf(Mkt_pl, h=30) $mean, col=4)
lines(rwf(Mkt_pl,h=30)$mean,col=2)
lines(rwf(Mkt_pl,drift=TRUE,h=30)$mean,col=3)
lines(a$mean,col=8)
legend("bottomright",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Drift method"))

mod <- ets(Mkt_pl )
a <- forecast(mod,h=30)
plot(forecast(Mkt$Close))

res <- ets(Mkt$Close, model="AAN")
plot(forecast(res$fitted[1500:2300]))
res$fitted


library(fpp)
dj2 <- window(dj,end=250)
tail(dj)
Mkt = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_ts.csv")
Mkt_ts <- ts(Mkt$Close)

Mkt_pl <- window(Mkt_ts, start=200, end=500)
plot.ts(Mkt_pl,
        main="Dax in 2006",
        xlab="Day", ylab="",
        xlim=c(220, 540))
lines(meanf(Mkt_pl, h=50) $mean, col=4)
lines(rwf(Mkt_pl,h=50)$mean,col=2)
lines(rwf(Mkt_pl,drift=TRUE,h=50)$mean,col=3)
legend("bottomleft",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Drift method"))

# 3 Exp Smoothing
Mkt = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_ts.csv")
#Mkt_ts <- ts(Mkt$Close, frequency = 252, start=c(2000,1))

# fully auto ...
plot(forecast(Mkt$Close))

# ETS
ets_mod <- ets(Mkt$Close)
length(ets_mod$fitted)
nrow(Mkt)
aa <- as.data.frame(ets_mod$fitted)
head(rr)
rr <- as.data.frame(ets_mod$residuals)
xx <- as.data.frame(ets_mod$x)
test <- cbind(Mkt, aa)
test <- cbind(test,rr)
test <- cbind(test,xx)
head(test)

Mkt$fit <- aa

tail(test, n=20)
head(Mkt)
colnames(Mkt[,6]) <- c('fit')

# forecast
ets_for <- forecast(ets_mod, h=2)

plot(ets_mod)
plot(ets_for)

# train and test
Mkt = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_ts.csv")
Mkt_ts <- ts(Mkt$Close)
Mkt_train <- window(Mkt_ts, end=2500)
Mkt_test <- window(Mkt_ts, start=2501, end=3500)

# ETS model rtns ets obj
tr_mod <- ets(Mkt_train)
test_mod <- ets(Mkt_test, model = tr_mod) #apply prev model

class(tr_mod) #ets
class(test_mod) # ets

# forecast rtns forecast obj
tr_for <- forecast(tr_mod, h=5)
test_for <- forecast(Mkt_test, model = tr_mod, h=1)

class(tr_for)
test_for
Mkt_ts[3500:3510]

Mkt_test <- window(Mkt_ts, start=2501, end=3501)
test_for <- forecast(Mkt_test, model = tr_mod, h=1)
test_for
Mkt_ts[3501:3502]

Mkt_test <- window(Mkt_ts, start=2501, end=3502)
test_for <- forecast(Mkt_test, model = tr_mod, h=1)
test_for
Mkt_ts[3502:3503]

Mkt_test <- window(Mkt_ts, start=2501, end=3503)
test_for <- forecast(Mkt_test, model = tr_mod, h=1)
test_for
Mkt_ts[3503:3504]

Mkt_test <- window(Mkt_ts, start=2501, end=3504)
test_for <- forecast(Mkt_test, model = tr_mod, h=1)
a <- as.character(test_for$mean)
i <- as.integer(test_for$mean);i
Mkt$pred <- 1
Mkt$pred[1] <- i
head(Mkt)
Mkt_ts[3504:3505]

ets_calc <- function(Mkt_ts, Mkt){
  mx <- length(Mkt)
  mx <- mx - 1
  #browser()
  for(i in 500 : 510){
    Mkt_t <- window(Mkt_ts, start=(i-300), end=i)
    mod <- ets(Mkt_t)
    a <- forecast(mod,h=1)
    re <-  as.integer(a$mean)
    num <- i
    Mkt$pred[num] <- re
  }
  return(Mkt)
}

Mkt = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_ts.csv")
Mkt_ts <- ts(Mkt$Close)
Mkt$pred <- 0
res <- ets_calc(Mkt_ts, Mkt)
res[490:520,]

accuracy(tr_mod)
accuracy(test_mod)
accuracy(tr_for)
accuracy(test_for)
test_for$fitted
test_mod$fitted
tail(test_for$fitted)
tail(test_mod$fitted)
test_mod$method
test_for$mean
tr_for$mean

Mkt_ts[3499]
Mkt_ts[3501]

accuracy(tr_mod, Mkt_test)
?accuracy
?forecast
?ets

tail(fcast2$mean,h=10)
tail(fit2$)
f <- as.data.frame(fit2$fitted)
r <- as.data.frame(fit2$residuals)
length(f)
ln <- nrow(Mkt)
Mkt2 <- Mkt[-ln , ]
fit3 <- ets(Mkt2$Close)
fcast3 <- forecast(fit3, h=2)
tail(fcast3$method)
tail(fcast3$fitted)
Mkt <- cbind(Mkt,f)

?table()
Mkt <- cbind(Mkt,r)
tail(Mkt)

Mkt = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_ts.csv")
Mkt_ts <- ts(Mkt$Close, frequency = 252, start=c(2000,1))
tail(Mkt_ts)
plot.ts(Mkt_ts)
Mkt_train <- window(Mkt_ts, start=2000, end=2009.99)
Mkt_test <- window(Mkt_ts, start=2010)
plot.ts(Mkt_train)
plot.ts(Mkt_test)

# ----------------------------------------------
ftable(Titanic, row.vars = 1:2,col.vars = 3)
ftable(Titanic, row.vars = 1:2, col.vars = "Survived")
ftable(Titanic, row.vars = 2:1, col.vars = "Survived")
table(Titanic$Class)

class(Titanic)
x <- ftable(mtcars[c("cyl", "vs", "am", "gear")])
x
ftable(x, row.vars = c(2, 4))

