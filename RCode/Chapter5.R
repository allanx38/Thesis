# Chapter 5
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

# ---------------------------------
# plot diff range
Mkt_test2 <- window(Mkt_ts, start=1510, end=1600)
Mkt_train2 <- window(Mkt_ts, start=1000, end=1500)
plot.ts(Mkt_train2,
        main="Dax over 300 Days",
        xlab="Day", ylab="",
        xlim=c(1000, 1600),
        ylim=c(3500, 6350))
lines(meanf(Mkt_train2, h=150) $mean, col=4)
lines(rwf(Mkt_train2,h=150)$mean,col=2)
lines(rwf(Mkt_train2,drift=TRUE,h=150)$mean,col=3)
legend("topleft",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Drift method"))

# plot diff range PLUS actual data
Mkt_test2 <- window(Mkt_ts, start=1510, end=1600)
Mkt_train2 <- window(Mkt_ts, start=1000, end=1500)
plot.ts(Mkt_train2,
        main="Dax over 300 Days",
        xlab="Day", ylab="",
        xlim=c(1000, 1600),
        ylim=c(3500, 6350))
lines(meanf(Mkt_train2, h=150) $mean, col=4)
lines(rwf(Mkt_train2,h=150)$mean,col=2)
lines(rwf(Mkt_train2,drift=TRUE,h=150)$mean,col=3)
legend("topleft",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Drift method"))
lines(Mkt_test2,col=6)

# ----------------------------------------



