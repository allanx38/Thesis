# Chapter 2 - Plots

setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")
source("../RCode/Utils.R")

# Data
rainfall <- scan("LondonRainfall.txt", skip=1)
rainfall <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rainfall, start=c(1813))
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))


# ------------------------
# series decomposition
# \label{fig:TimeSeriesComponents}
# ------------------------
savepdf('chp2_TimeSeriesComponents')     #start
birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)
dev.off()                   #end 
# --------- END

# ------------------------
#   stationary series - 
# \label{fig:Stationary_ts}
# ------------------------
savepdf('chp2_Stationary_ts')     #start
plot.ts(rainseries,
        main="London Rainfall",
        xlab="Year", 
        ylab="Rainfall")

dev.off()                   #end 
# --------- END


# ------------------------
# decom of additive series
# \label{fig:Add2_ts}
# -----------------------
savepdf('chp2_Add2_ts')     #start
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries,
        main="Sales",
        xlab="Year", 
        ylab="Sales")
dev.off()                   #end
# --------- END


# ------------------------
# decom of multiplic series
# \label{fig:Multi_ts}
# -----------------------
savepdf('chp2_Multi_ts')     #start
plot(souvenirtimeseries,
     main="Souvenir Sales",
     xlab="Year", 
     ylab="Souvenir Sales")
dev.off()                   #end
# --------- END



# ------------------------
# Holt Winters defore smoothing  --> REPEATED
# \label{fig:HW1a}
# -----------------------

savepdf('chp2_HW1')     #start

# --------- END


# ------------------------
# Holt Winters smoothing - statinary
# \label{fig:HW1a}
# -----------------------
savepdf('chp2_HW1a')     #start

rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
plot(rainseriesforecasts,
     main="London Rainfall with Exponential Smoothing",
     xlab="Year", 
     ylab="Rainfall")

dev.off()                   #end

# --------- END

# ------------------------
# Holt Winters smoothing - trend though no seasonality
# \label{fig:HW2a}
# -----------------------
savepdf('chp2_HW2a')     #start
#plot.ts(skirtsseries)
skirtsseriesforecasts <- HoltWinters(skirtsseries,gamma=FALSE)
plot(skirtsseriesforecasts,
     main="Skirt Lenghts with Exponential Smoothing",
     xlab="Year", 
     ylab="Skirt Lenghts")
legend("topright",lty=1,col=c(1,2),
       legend=c("Time Series","Exponential Smoothing"))

dev.off()                   #end

# --------- END

# ------------------------
# Holt Winters smoothing - trend though and seasonality
# \label{fig:HW3a}
# -----------------------
savepdf('chp2_HW3a')     #start
#plot(souvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(souvenirtimeseries)
plot(souvenirtimeseriesforecasts,
     main="Souvenir Sales with Exponential Smoothing",
     xlab="Year", 
     ylab="Souvenir Sales")
legend("topleft",lty=1,col=c(1,2),
       legend=c("Time Series","Exponential Smoothing"))

dev.off()                   #end
# --------- END


# ------------------------
# ACF
# \label{fig:acf80}
# -----------------------
savepdf('chp2_acf80')     #start
plot(acf(rainseries),
     main="ACF of the Rainseries Data",
     ylab="Auto-correlation (ACF)")
dev.off()                   #end
# --------- END



# ------------------------
# PACF
# \label{fig:pacf}
# -----------------------
savepdf('chp2_p_pacf')     #start
plot(pacf(rainseries),
     main="ACF of the Rainseries Data",
     ylab="Auto-correlation (ACF)")
dev.off()                   #end
# --------- END



# apseries <- ts(AirPassengers, frequency=12, start=c(1949))
# plot.ts(apseries)
# aa <- acf(apseries,lag.max=30)
# plot.ts(aa)
# aa <- acf(apseries,lag.max=80)
# plot.ts(aa)
# apseriescomp <- decompose(apseries)
# bb <- acf(apseriescomp$seasonal,lag.max=20)
# plot.ts(bb)
# 
# 
# 
# # produce latex table
# dat <- d[,c(2,3,4,5,6)]
# dig <- 0
# cap <- c("Mean, Naive and Drift methods applied to 
#          to the Dax.","Simple forecasting methods.")
# lab = 'tab:chp_ts:sma'
# filname ='../Tables/chp_ts_sma.tex'
# inclrnam=TRUE
# print_xt(dat,dig,cap,lab,al,filname,inclrnam)
# 
# 
# savepdf("chp_ts_dax1")
# Mkt_act <- window(Mkt_ts, start=3020, end=3200)
# plot.ts(Mkt_train,
#         main="Simple Forecasting Methods",
#         xlab="Days since 2000", ylab="Dax Closing Price",
#         xlim=c(2, 3200))
# lines(meanf(Mkt_train, h=350) $mean, col=4)
# lines(rwf(Mkt_train,h=350)$mean,col=2)
# lines(rwf(Mkt_train,drift=TRUE,h=350)$mean,col=3)
# legend("bottomright",lty=1,col=c(4,2,3),
#        legend=c("Mean method","Naive method","Drift method"))
# dev.off() #s