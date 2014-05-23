# Chapter 5 - test
setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")

library(forecast)
library(xtable)

source("../RCode//Utils.R")

Mkt <- read.csv("../Data/Dax_2000_d.csv")
nrow(Mkt)
Mkt$Date[2999]
Mkt_ts <- ts(Mkt$Close)
#Mkt_ts <- ts(Mkt$Close,frequency=252, start=c(2000,1))
#Mkt_train <- window(Mkt_ts, start=2000, end=2009.99)
Mkt_train <- window(Mkt_ts, end=2999.99)
Mkt_test <- window(Mkt_ts, start=3000)


# -----------------------------------------------------
# ------------ intro section
#plot for a look
# savepdf("chp_ts_dax1")
# 
# # Plotting commands here
# Mkt_pl <- window(Mkt_ts, start=200, end=500)
# plot.ts(Mkt_pl,
#         main="Dax over 300 Days",
#         xlab="Day", ylab="",
#         xlim=c(220, 540))
# lines(meanf(Mkt_pl, h=50) $mean, col=4)
# lines(rwf(Mkt_pl,h=50)$mean,col=2)
# lines(rwf(Mkt_pl,drift=TRUE,h=50)$mean,col=3)
# legend("bottomleft",lty=1,col=c(4,2,3),
#        legend=c("Mean method","Naive method","Drift method"))
# dev.off() #savepdf end

# ---------------- Base System 1
?accuracy
# build the  mean model
mean_model <- meanf(Mkt_train, h=5)
a <- accuracy(mean_model, Mkt_test) #out of sample
rownames(a) <- c('Mean Training Set', 'Mean Test Set')

# build the mean model
naive_model <- naive(Mkt_train, h=5)
b <- accuracy(naive_model, Mkt_test) #out of sample
rownames(b) <- c('Naive Training Set', 'Naive Test Set')

# build the drift model
drift_model <- rwf(Mkt_train,drift=TRUE,h=5)
c <- accuracy(drift_model, Mkt_test) #out of sample
rownames(c) <- c('Drift Training Set', 'Drift Test Set')

# combine results
d <- rbind(a,b,c)

# produce latex table
dat <- d[,c(2,3,4,5,6)]
dig <- 0
cap <- c("Mean, Naive and Drift methods applied to 
         to the Dax.","Simple forecasting methods.")
lab = 'tab:chp_ts:sma'
filname ='../Tables/chp_ts_sma.tex'
inclrnam=TRUE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# --- plot all three base systems on Dow
savepdf("chp_ts_dax1")
Mkt_act <- window(Mkt_ts, start=3020, end=3200)
plot.ts(Mkt_train,
        main="Simple Forecasting Methods",
        xlab="Days since 2000", ylab="Dax Closing Price",
        xlim=c(2, 3200))
lines(meanf(Mkt_train, h=350) $mean, col=4)
lines(rwf(Mkt_train,h=350)$mean,col=2)
lines(rwf(Mkt_train,drift=TRUE,h=350)$mean,col=3)
legend("bottomright",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Drift method"))
dev.off() #savepdf end

# --- plot all three base systems on Dow PLUS actual data
savepdf("chp_ts_dax1_plus_act_data")
Mkt_act <- window(Mkt_ts, start=3020, end=3200)
plot.ts(Mkt_train,
        main="Simple Forecasting Methods",
        xlab="Days since 2000", ylab="Dax Closing Price",
        xlim=c(2, 3200))
lines(meanf(Mkt_train, h=350) $mean, col=4)
lines(rwf(Mkt_train,h=350)$mean,col=2)
lines(rwf(Mkt_train,drift=TRUE,h=350)$mean,col=3)
legend("bottomright",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Drift method"))
lines(Mkt_act, col=6)
dev.off() #savepdf end

# --------------- NOT USED AT MO -------------------
# plot diff range
# Mkt_test2 <- window(Mkt_ts, start=1510, end=1600)
# Mkt_train2 <- window(Mkt_ts, start=1000, end=1500)
# plot.ts(Mkt_train2,
#         main="Dax over 300 Days",
#         xlab="Day", ylab="",
#         xlim=c(1000, 1600),
#         ylim=c(3500, 6350))
# lines(meanf(Mkt_train2, h=150) $mean, col=4)
# lines(rwf(Mkt_train2,h=150)$mean,col=2)
# lines(rwf(Mkt_train2,drift=TRUE,h=150)$mean,col=3)
# legend("topleft",lty=1,col=c(4,2,3),
#        legend=c("Mean method","Naive method","Drift method"))
# 
# # plot diff range PLUS actual data
# Mkt_test2 <- window(Mkt_ts, start=1510, end=1600)
# Mkt_train2 <- window(Mkt_ts, start=1000, end=1500)
# plot.ts(Mkt_train2,
#         main="Dax over 300 Days",
#         xlab="Day", ylab="",
#         xlim=c(1000, 1600),
#         ylim=c(3500, 6350))
# lines(meanf(Mkt_train2, h=150) $mean, col=4)
# lines(rwf(Mkt_train2,h=150)$mean,col=2)
# lines(rwf(Mkt_train2,drift=TRUE,h=150)$mean,col=3)
# legend("topleft",lty=1,col=c(4,2,3),
#        legend=c("Mean method","Naive method","Drift method"))
# lines(Mkt_test2,col=6)

# ----------------------------------------

# ------------ ARIMA ----------------------
# -----------------------------------------


#1. Plot the data. Identify any unusual observations.
Mkt <- read.csv("../Data/F100_2000_d.csv")
Mkt_ts <- ts(Mkt$Close)
savepdf("chp_ts_ftse_2000-13")
plot.ts(Mkt_ts,
        main="FTSE 2000 - 2013",
        xlab="Days since 2000", 
        ylab="FTSE Closing Price",
        xlim=c(100, 3300))
dev.off()
dev.cur()

#2. If necessary, transform the data (using a Box-Cox transformation) 
#to stabilize the variance.

#3. If the data are non-stationary: take first differences of the 
#data until the data are stationary.
savepdf("chp_ts_ftse_2000-13_diff")
plot(diff(Mkt_ts),
          main="FTSE 2000 - 2013",
          xlab="Days since 2000", 
          ylab="FTSE Daily Price Movement",
          xlim=c(100, 3300))
dev.off()

#4. Examine the ACF/PACF: Is an AR(p) or MA(q) model appropriate?

tsdisplay(diff(Mkt_ts),main="")

#5. Try your chosen model(s), and use the AICc to search for a better model.

#6. Check the residuals from your chosen model by plotting the ACF of the residuals, and doing a portmanteau test of the residuals. If they do not look like white noise, try a modified model.

#7. Once the residuals look like white noise, calculate forecasts.





# 2. Modelling
#setwd("F:/Allan/R Stuff/MSc/Chp5")
# setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")
# 
# library(forecast)
Mkt <- read.csv("../Data/Dax_2000_d.csv");nrow(Mkt)
#Mkt <- read.csv("Dax_2000.csv");nrow(Mkt)
Mkt_ts <- ts(Mkt$Close)
Mkt_train <- window(Mkt_ts, end=2999.99)
Mkt_test <- window(Mkt_ts, start=3000)
tail(Mkt_test)
length(Mkt_test)

arima_train_mod <- auto.arima(Mkt_train)
arima_fcast <- forecast.Arima(arima_train_mod,Mkt_test)
# accuracy(fcast,Mkt_test)
arima_test_mod <- Arima(Mkt_test, model = arima_train_mod) # 1 step fcast on future data ...
arima_test_fcast <- forecast(arima_test_mod)
fitted.data <- as.data.frame(arima_test_fcast$fitted); nrow(fitted.data)
# 
# ln <- nrow(Mkt)
# lw <- nrow(fitted.data) ;lw
# Mkt_test_df <- Mkt[(ln-lw+1):ln,]  ;nrow(Mkt_test_df)
# #Mkt_test_df <- as.data.frame(Mkt_test) ;nrow(Mkt_test_df)
# Mkt_test_df <- cbind(Mkt_test_df,fitted.data)
# colnames(Mkt_test_df) <- c("Date","Open", "High","Low","Close","p")


source("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode/ts_1.R")
source("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode/ts_2.R")
source("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode/Utils.R")
#source("ts_1.R")
#source("ts_2.R")
#source("Utils.R")

#res <- ts_1(Mkt_test_df,0,"Dax");res
#res2 <- ts_2(Mkt_test_df,0,"Dax");res2

# ----------------------------------------------------
fil <- c("../Data//Dax_2000_d.csv",
         "../Data//CAC_2000_d.csv", 
         "../Data//F100_2000_d.csv",
         "../Data//Dow_2000_d.csv",
         "../Data//N225_2000_d.csv",
         "../Data//Oz_2000.csv")
nm <- c("Dax", "CAC", "F100", "Dow", "Nik", "Oz")
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))

testfnc <- function(fil,nm){
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    Mkt_ts <- ts(Mkt$Close)
    Mkt_train <- window(Mkt_ts, end=2999.99)
    Mkt_test <- window(Mkt_ts, start=3000)
    arima_train_mod <- auto.arima(Mkt_train)
    arima_fcast <- forecast.Arima(arima_train_mod,Mkt_test)
    arima_test_mod <- Arima(Mkt_test, model = arima_train_mod) # 1 step fcast on future data ...
    arima_test_fcast <- forecast(arima_test_mod)
    fitted.data <- as.data.frame(arima_test_fcast$fitted); 
    ln <- nrow(Mkt)
    lw <- nrow(fitted.data)
    Mkt_test_df <- Mkt[(ln-lw+1):ln,] 
    Mkt_test_df <- cbind(Mkt_test_df,fitted.data)
    colnames(Mkt_test_df) <- c("Date","Open", "High","Low","Close","p")
    a <- ts_1(Mkt_test_df, 0, nm[i])
    df.name <- names(a)
    names(df10) <- df.name
    df10 <- rbind(df10, a)
  }
  df10 <- df10[-c(1),]
  browser()
  return(df10)
}

res <- testfnc(fil,nm)

# produce latex table
dat <- res[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("ts1 arima.","arima.")
lab = 'tab:chp_ts:arima1'
filname ='../Tables/chp_ts_arima1.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)
