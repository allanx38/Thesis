# Chapter 5 - test
setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")

# libraries
library(forecast)
library(xtable)

#source
source("../RCode/Utils.R")
source("../RCode/ts_1.R")
source("../RCode/ts_2.R")
#source("ts_1.R")
#source("ts_2.R")
#source("Utils.R")

fil <- c("../Data/Dax_2000_d.csv",
         "../Data/CAC_2000_d.csv", 
         "../Data/F100_2000_d.csv",
         "../Data/Dow_2000_d.csv",
         "../Data/N225_2000_d.csv",
         "../Data/Oz_2000.csv")
nm <- c("Dax", "CAC", "F100", "Dow", "Nik", "Oz")

# -----------------------------------------------------------
# ---------- Base Systems
Mkt <- read.csv("../Data/Dax_2000_d.csv")
nrow(Mkt)
Mkt$Date[2999]
Mkt_ts <- ts(Mkt$Close)
#Mkt_ts <- ts(Mkt$Close,frequency=252, start=c(2000,1))
#Mkt_train <- window(Mkt_ts, start=2000, end=2009.99)
Mkt_train <- window(Mkt_ts, end=2999.99)
Mkt_test <- window(Mkt_ts, start=3000)

# a.build the  mean model
mean_model <- meanf(Mkt_train, h=5)
a <- accuracy(mean_model, Mkt_test) #out of sample
rownames(a) <- c('Mean Training Set', 'Mean Test Set')

# b. build the mean model
naive_model <- naive(Mkt_train, h=5)
b <- accuracy(naive_model, Mkt_test) #out of sample
rownames(b) <- c('Naive Training Set', 'Naive Test Set')

# c. build the drift model
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


# -----------------------------------------
# 2. ARIMA ----------------------
Mkt <- read.csv("../Data/F100_2000_d.csv")
Mkt_ts <- ts(Mkt$Close)
Mkt_train <- window(Mkt_ts, end=2999.99)
Mkt_test <- window(Mkt_ts, start=3000)

# -------------------------------
# 2.1. Plot the data. Identify any unusual observations.
savepdf("chp_ts_ftse_2000-13")
plot.ts(Mkt_train,
        main="FTSE 2000 - 2013",
        xlab="Days since 2000", 
        ylab="FTSE Closing Price",
        xlim=c(100, 3000))
dev.off()

# 2.2. If necessary, transform the data (using a Box-Cox transformation) 
#to stabilize the variance.

# 2.3. If the data are non-stationary: take first differences of the 
#data until the data are stationary.
savepdf("chp_ts_ftse_2000-13_diff")
plot(diff(Mkt_train),
          main="FTSE 2000 - 2013",
          xlab="Days since 2000", 
          ylab="FTSE Daily Price Movement",
          xlim=c(100, 3000))
dev.off()

# -------------------------------------
# 2.4. Examine the ACF/PACF: Is an AR(p) or MA(q) model appropriate?

# all 3 incl diff
savepdf("chp_ts_ftse_2000-13_diff_acf_tsd")
tsdisplay(diff(Mkt_train),main="FTSE 100 between 2000 and 2013",
          xlab="Days since 2000", 
          ylab="FTSE Daily Price Movement")
dev.off()

# a ACF
savepdf("chp_ts_ftse_2000-13_diff_acf")
plot(Acf(diff(Mkt_train)),
     main="ACF of FTSE 100 between 2000 and 2013",
     ,ylim=c(-0.08, 0.08))
dev.off()

# a PACF
savepdf("chp_ts_ftse_2000-13_diff_pacf")
plot(Pacf(diff(Mkt_train)),
     main="PACF of FTSE 100 between 2000 and 2013",
     ylim=c(-0.08, 0.08))
dev.off()

# ----------------------------------------------------
# 2.5. Try your chosen model(s), and use the AICc to search for a better model.
       
mod_ar <- function(Mkt, ord, nm){
  res <- t(as.data.frame(rep(0,4)))
  mod <- Arima(Mkt_ts, order=ord)
  res[1,1] <- nm
  res[1,2] <- round(mod$aic,1)
  res[1,3] <- round(mod$aicc,1)
  res[1,4] <- round(mod$bic,1)
  return(res)
}

results <- t(as.data.frame(rep(0,4)))
colnames(results) <- c('Model','AIC','AICc','BIC')

r2 <- mod_ar(Mkt_train, c(3,1,1), 'Arima(3,1,1)')
results <- rbind(results,r2)
r2 <- mod_ar(Mkt_train, c(3,1,2), 'Arima(3,1,2)')
results <- rbind(results,r2)
r2 <- mod_ar(Mkt_train, c(3,1,3), 'Arima(3,1,3)')
results <- rbind(results,r2)
r2 <- mod_ar(Mkt_train, c(2,1,1), 'Arima(2,1,1)')
results <- rbind(results,r2)
r2 <- mod_ar(Mkt_train, c(2,1,2), 'Arima(2,1,2)')
results <- rbind(results,r2)
r2 <- mod_ar(Mkt_train, c(2,1,3), 'Arima(2,1,3)')
results <- rbind(results,r2)
results <- results[-1,]

# produce latex table
dat <- results
dig <- c(0,0,2,2,2)
cap <- c("Arima results.","Arima results.")
lab = 'tab:chp_ts:arima_res_r'
filname ='../Tables/chp_ts_arima_res_r.tex'
inclrnam=F
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# model_311 <- Arima(Mkt_ts, order=c(3,1,1))
# results[1,1] <- model_311$aic
# results[2,1] <- model_311$aicc
# results[3,1] <- model_311$bic

# ----------------------------------------------------
# 2.6. Check the residuals from your chosen model by plotting the ACF of the residuals, 
#and doing a portmanteau test of the residuals. 
#If they do not look like white noise, try a modified model.

model_used_for_res <- Arima(Mkt_train, order=c(2,1,3))
model_name <- forecast(model_used_for_res)$method

# a mean of residual
residual <- model_used_for_res$residuals
savepdf("chp_ts_ftse_2000-13_mean_residuals")
plot(residual, main = paste("Residuals from model of", model_name), 
     ylab="", xlab="Day")
dev.off()

# b. acf of residual
savepdf("chp_ts_ftse_2000-13_acf_residuals")
Acf(residuals(model_used_for_res),
    main= paste("ACF of Residuals of", model_name))
dev.off()

# c. variance - use plot from a

# d. histogram of residuals - normal distribution
savepdf("chp_ts_ftse_2000-13_hist_residuals")
hist(residual, nclass="FD", main="Histogram of residuals")
dev.off()

# e. portmanteau tests
bb <- Box.test(residuals(model_used_for_res), lag=24, fitdf=4, type="Ljung")
results_bc <- as.data.frame(rep(0,3))
results_bc[1,1] <- round(bb$p.value,4)
results_bc[2,1] <- round(bb$parameter)
results_bc[3,1] <- round(bb$statistic)
#colnames(results_bc) <- c(paste(bb$method,forecast(model_311)$method))
colnames(results_bc) <- c(forecast(model_used_for_res)$method)
rownames(results_bc) <- c('p-value','x-squared','df')
#results_bc[1,1]
results_bc_t <- t(results_bc)

dat <- results_bc_t
dig <- c(0,4,0,0)
cap <- c("Box Ljung test.","Box Ljung test.")
lab = 'tab:chp_ts:arima_res_rbox_l'
filname ='../Tables/chp_ts_arima_res_r_box_l.tex'
inclrnam=TRUE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# 2.7 Once the residuals look like white noise, calculate forecasts.
arima_man_fcast <- forecast.Arima(model_used_for_res,Mkt_test)
fitted.data <- as.data.frame(arima_man_fcast$fitted); 
ln <- nrow(Mkt)
lw <- nrow(fitted.data)
Mkt_test_df <- Mkt[(ln-lw+1):ln,] 
Mkt_test_df <- cbind(Mkt_test_df,fitted.data)
colnames(Mkt_test_df) <- c('Date','Open','High','Low','Close','Forecast')

# plot the results
dat <- tail(Mkt_test_df)
dig <- 0
cap <- c("FTSE 100 foecast.","FTSE 100 foecast.")
lab = 'tab:chp_ts:ftse_100_fcast'
filname ='../Tables/chp_ts_ftse_100_fcast.tex'
inclrnam=F
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# 2.8 auto.arima

arim_mod_fnc <- function(fil,nm){
  #browser()
  dfres <- dfres <- t(c('a','b'))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    Mkt_train <- ts(Mkt$Close)
    #Mkt_train <- window(Mkt_ts, end=2999.99)
    #Mkt_test <- window(Mkt_ts, start=3000)
    arima_train_mod <- auto.arima(Mkt_train)
    #arima_fcast <- forecast.Arima(arima_train_mod)
    dfres <- rbind(dfres,c(nm[i], forecast(arima_train_mod)$method))
  }
  return(dfres)
}

fg <- arim_mod_fnc(fil,nm)
fg <- fg[-1,]
colnames(fg) <- c('Market','Arima Model')

# plot the results
dat <- fg
dig <- 0
cap <- c("Arima models  from national indices.","Arima models.")
lab = 'tab:chp_ts_arima_models'
filname ='../Tables/chp_ts_arima_models.tex'
inclrnam=F
print_xt(dat,dig,cap,lab,al,filname,inclrnam)




# ----------------------------------------------------
# 3. Trading System


df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))

ts_1_fnc <- function(fil,nm,ts1){
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
    if(ts1 == TRUE){
      a <- ts_1(Mkt_test_df, 0, nm[i])
    } else {
      a <- ts_2(Mkt_test_df, 0, nm[i])
    }
    #browser()
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-c(1),]
  #browser()
  return(df10)
}

# run the fnc ts_1
res <- ts_1_fnc(fil,nm,TRUE)

# produce latex table from ts_1
dat <- res[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("ts1 arima.","arima.")
lab = 'tab:chp_ts:arima1'
filname ='../Tables/chp_ts_arima1.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# run the fnc ts_2
res <- ts_1_fnc(fil,nm,FALSE) # F = ts_2

# produce latex table from ts_2
dat <- res[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("ts2 arima.","ts2 arima.")
lab = 'tab:chp_ts:arima2'
filname ='../Tables/chp_ts_arima2.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# ----------------------------------------------------------------------
# --------- RM Generated Files -----------------------------------------
source("../RCode/ts_1.R")
source("../RCode/ts_2.R")
Mkt <- read.csv("../Data/rm_ar334_reg.csv",stringsAsFactors=F)

fil <- c("../Data/rm_ar334_reg.csv")
nm <- c("Dax")
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))

ts_1_fnc_ar <- function(fil,nm,ts1){
  for(i in 1:length(fil)){
    Mkt_p <- Mkt[,c(1,2,3,4,5,18)]
    colnames(Mkt_p) <- c("Date","Open", "High","Low","Close","p")
    a <- ts_1(Mkt_p, 0, 'Dax')
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-c(1),]
  return(df10)
}

res <- ts_1_fnc_ar(fil,nm)
  
# produce latex table from ts_1
dat <- res[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("ts1 arima hybrid reg.","ts1 arima hybrid reg.")
lab = 'tab:chp_ts:arima_hybrid_reg'
filname ='../Tables/chp_ts_arima_hybrid_reg.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)
  
  
  