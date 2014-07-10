# Chapter 5 - test
setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")

# libraries
library(forecast)
library(xtable)

#source
source("../RCode/Utils.R")
source("../RCode/es_1.R")
source("../RCode/ts_1.R")
source("../RCode/ts_2.R")
source("../RCode/ts_3.R")
source("../RCode/ts_3a.R")
source("../RCode/ts_4.R")
source("../RCode//NaiveReversePrev.R")

fil <- c("../Data/Dax_2000_d.csv",
         "../Data/CAC_2000_d.csv", 
         "../Data/F100_2000_d.csv",
         "../Data/Dow_2000_d.csv",
         "../Data/N225_2000_d.csv",
         "../Data/Oz_2000.csv")
nm <- c("Dax", "CAC", "FTSE", "Dow", "Nikkei", "AORD")

# Add Naive follow prev for comparison purposes
# data frame will be fed into sub_df

df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
NaiveRev <- run_NaiveReversePrev(fil, 0, nm)

# -----------------------------------------------------------
# ---------- Base Systems
Mkt <- read.csv("../Data/Dax_2000_d.csv")
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

# b. build the naive model
naive_model <- naive(Mkt_train, h=5)
b <- accuracy(naive_model, Mkt_test) #out of sample
rownames(b) <- c('Naive Training Set', 'Naive Test Set')

# c. build the drift model
drift_model <- rwf(Mkt_train,drift=TRUE,h=5)
c <- accuracy(drift_model, Mkt_test) #out of sample
rownames(c) <- c('Drift Training Set', 'Drift Test Set')

# combine results
d <- rbind(a,c)

# produce latex table
dat <- d[,c(2,3,4,5,6)]
dig <- 0
cap <- c("Mean and Drift methods applied to 
         to the Dax.","Mean and Drift methods applied to 
         to the Dax")
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
#lines(rwf(Mkt_train,h=350)$mean,col=2)
lines(rwf(Mkt_train,drift=TRUE,h=350)$mean,col=3)
legend("bottomright",lty=1,col=c(4,3),
       legend=c("Mean method","Drift method"))
dev.off() #savepdf end

# --- plot all three base systems on Dow PLUS actual data
savepdf("chp_ts_dax1_plus_act_data")
Mkt_act <- window(Mkt_ts, start=3020, end=3200)
plot.ts(Mkt_train,
        main="Simple Forecasting Methods",
        xlab="Days since 2000", ylab="Dax Closing Price",
        xlim=c(2, 3200))
lines(meanf(Mkt_train, h=350) $mean, col=4)
#lines(rwf(Mkt_train,h=350)$mean,col=2)
lines(rwf(Mkt_train,drift=TRUE,h=350)$mean,col=3)
legend("bottomright",lty=1,col=c(4,3),
       legend=c("Mean method","Drift method"))
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

# 1. Exp Smoothing - mean model
# a. build data set - window thru and add prediction

# a1 - calculates mean prediction
exp_mean <- function(Mkt_ts, Mkt, strt, mean_flag){
  #browser()
  Mkta <- Mkt
  cc <- Mkta[1,]
  cc$a <- 0
  ln <- nrow(Mkt)
  for(i in strt:ln){
    st <- i-30
    Mkt_slice <- window(Mkt_ts,start=st,end=i)
    if (mean_flag == TRUE) {
      modf <- meanf(Mkt_slice,h=1)
    } else {
      modf <- rwf(Mkt_slice,drift=TRUE,h=1)
    }
    a <- as.numeric(modf$mean)
    c1 <- Mkta[i,]
    ab <- cbind(c1,a)
    cc <- rbind(cc,ab)
  }
  cc <- cc[-1,]
  return(cc)
}

# a2 -generates data sets with predictions
run_exp_mean <- function(fil,nm){
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    Mkt_ts <- ts(Mkt$Close)
    res <- exp_mean(Mkt_ts,Mkt,400,TRUE)
    browser()
    write.csv(res,paste('../Data/ES/',nm[i],'_es_mean.csv',sep=""),row.names=FALSE)
  }
}

# a3 - run thru data sets - takes while so need to run just once
# run_exp_mean(fil,nm)

# a4 - use data sets in system

fil_mean <- c("../Data/ES/Dax_es_mean.csv",
              "../Data/ES/CAC_es_mean.csv", 
              "../Data/ES/FTSE_es_mean.csv",
              "../Data/ES/Dow_es_mean.csv",
              "../Data/ES/Nikkei_es_mean.csv",
              "../Data/ES/AORD_es_mean.csv")

run_es_1 <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    a <- es_1(Mkt, SLoss, nm[i])         
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res_mean <- run_es_1(fil_mean,0,nm)
res_mean[misc_col] <- 'Mean Method'

misc_col <- 11
# for summary results
total_res <- res_mean

dat <- res_mean[,std6]
dig <- 2
cap =  c('Results from the mean base ES System.',
         'Results from the mean base ES System')
lab = 'tab:es_mean_sys'
filname ='../Tables/chp_ts_es_mean.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# ---------------------------------------
# 2 - Drift model
run_exp_drift <- function(fil,nm){
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    Mkt_ts <- ts(Mkt$Close)
    res <- exp_mean(Mkt_ts,Mkt,400,FALSE)
    write.csv(res,paste('../Data/ES/',nm[i],'_es_drift.csv',sep=""),row.names=FALSE)
  }
}

# a3 - run thru data sets - takes while so need to run just once
#run_exp_drift(fil,nm)

# 
fil_drift <- c("../Data/ES/Dax_es_drift.csv",
              "../Data/ES/CAC_es_drift.csv", 
              "../Data/ES/FTSE_es_drift.csv",
              "../Data/ES/Dow_es_drift.csv",
              "../Data/ES/Nikkei_es_drift.csv",
              "../Data/ES/AORD_es_drift.csv")

res_drift <- run_es_1(fil_drift,0,nm)
res_drift[misc_col] <- 'Drift Method'

# Add to total results
total_res <- rbind(total_res, res_drift)

dat <- res_drift[,std6]
dig <- 2
cap =  c('Results from the drift base ES System.',
         'Results from the drift base ES System')
lab = 'tab:es_drift_sys'
filname ='../Tables/chp_ts_es_drift.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# 3. Exp Smoothing - ets model
# 3a. gen data set
exp_sm <- function(Mkt_ts, Mkt, strt){
  Mkta <- Mkt
  cc <- Mkta[1,]
  cc$a <- 0
  cc$b <- 0
  ln <- nrow(Mkt)
  for(i in strt:ln){
    st <- i-30
    Mkt_slice <- window(Mkt_ts,start=st,end=i)
    modf <- ets(Mkt_slice)
    fcastf <- forecast.ets(modf,h=1)
    a <- as.numeric(fcastf$mean)
    b <- modf$method
    c1 <- Mkta[i,]
    ab <- cbind(c1,b,a)
    cc <- rbind(cc,ab)
  }
  cc <- cc[-1,]
  return(cc)
}

# 3b - generates data sets with predictions
run_exp_sm <- function(fil,nm){
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    Mkt_ts <- ts(Mkt$Close)
    res <- exp_sm(Mkt_ts,Mkt,400)
    browser()
    write.csv(res,paste('../Data/ES/',nm[i],'_es.csv',sep=""),row.names=FALSE)
  }
}

# loop thru data sets
#run_exp_sm(fil,nm)

# 3d Trade ES
fil_es <- c("../Data/ES/Dax_es.csv",
               "../Data/ES/CAC_es.csv", 
               "../Data/ES/FTSE_es.csv",
               "../Data/ES/Dow_es.csv",
               "../Data/ES/Nikkei_es.csv",
               "../Data/ES/AORD_es.csv")

# use prev function
res_es <- run_es_1(fil_es,0,nm)
res_es[misc_col] <- 'Exponential Smoothing'

# Add to total results
total_res <- rbind(total_res, res_es)

dat <- res_es[,std6]
dig <- 2
cap =  c('Results from the ES System.',
         'Results from the ES System')
lab = 'tab:es_sys'
filname ='../Tables/chp_ts_es.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

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
          main="First Difference of FTSE 2000 - 2013",
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
       
mod_ar <- function(Mkt_ts, ord, nm){
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
cap <- c("AIC, AICc and BIC results from alternative ARIMA models.",
         "AIC, AICc and BIC results from alternative ARIMA models")
lab = 'tab:chp_ts:arima_res_r'
filname ='../Tables/chp_ts_arima_res_r.tex'
inclrnam=F
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

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
cap <- c("Box Ljung test of FTSE 100 ARIMA model residuals.",
         "Box Ljung test of FTSE 100 ARIMA model residuals")
lab = 'tab:chp_ts:arima_res_rbox_l'
filname ='../Tables/chp_ts_arima_res_r_box_l.tex'
inclrnam=TRUE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# 2.7 Once the residuals look like white noise, calculate forecasts.
model_used_for_res <- Arima(Mkt_ts, order=c(2,1,3))
model_name <- forecast(model_used_for_res)$method

arima_man_fcast <- forecast.Arima(model_used_for_res,Mkt_test)
fitted.data <- as.data.frame(arima_man_fcast$fitted); 
#ln <- nrow(Mkt)
#lw <- nrow(fitted.data)
#Mkt_test_df <- Mkt[(ln-lw+1):ln,] 
Mkt_test_df <- cbind(Mkt,fitted.data)
colnames(Mkt_test_df) <- c('Date','Open','High','Low','Close','Forecast')

# plot the results
dat <- tail(Mkt_test_df)
dig <- 0
cap <- c("One step ahead forecast for FTSE 100 generated from ARIMA(2,1,3) model.",
         "Forecast for FTSE 100 generated from the ARIMA model")
lab = 'tab:chp_ts:ftse_100_fcast'
filname ='../Tables/chp_ts_ftse_100_fcast.tex'
inclrnam=F
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# -------------------------------------------------
# 2.8 auto.arima

arim_mod_fnc <- function(fil,nm){
  dfres <- dfres <- t(c('a','b'))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    Mkt_train <- ts(Mkt$Close)
    arima_train_mod <- auto.arima(Mkt_train)
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
cap <- c("Arima models chosen to forecast future values in the national indice data sets.",
         "Arima models chosen for the indice data sets")
lab = 'tab:chp_ts_arima_models'
filname ='../Tables/chp_ts_arima_models.tex'
inclrnam=F
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# plot the results for Chp 6 ...
dat <- fg
dig <- 0
cap <- c("Arima models chosen to forecast future values in the national indice data sets.",
         "Arima models chosen for the indice data sets")
lab = 'tab:chp_ts_arima_models_chp6'
filname ='../Tables/chp_ts_arima_models_chp6.tex'
inclrnam=F
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# ----------------------------------------------------
# 3. Trading System
# using the models generated from the auto.arima function

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
      a <- ts_1(Mkt_test_df, 0, nm[i]) # System 1
    } else {
      a <- ts_2(Mkt_test_df, 0, nm[i]) # System 2
    }
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-c(1),]
  return(df10)
}

# run the fnc ts_1
# apply Sys 1 to the auto.arima data
res1 <- ts_1_fnc(fil,nm,TRUE)
res1[misc_col] <- 'ARIMA - System 1'

# Add to total results
total_res <- rbind(total_res, res1)

# produce latex table from ts_1
dat <- res1[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Forecasts generated by the ARIMA models used in the System 1 algorithm.",
         "Forecasts generated by the ARIMA models used in the System 1 algorithm")
lab = 'tab:chp_ts:arima1'
filname ='../Tables/chp_ts_arima1.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# compare to Naive reverse
diff_df1 <- sub_df_av_pl(res1,NaiveRev)
# produce latex table from ts_1
#dat <- diff[,c(1,7,10)]
dat <- diff_df1
dig <- 0
cap <- c("Mean Long/Short PL from system using predictions from ARIMA models with the results from the Naive Reverse system subtracted.",
         "Mean PL from ARIMA models minus mean PL from Naive Reverse system")
lab = 'tab:chp_ts:arima1_diff'
filname ='../Tables/chp_ts_arima1_diff.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# ----------------------------------------------------------------
# run the fnc ts_2
# apply system 2 to auto.arima data
res2 <- ts_1_fnc(fil,nm,FALSE) # F = ts_2
res2[misc_col] <- 'ARIMA - System 2'

# Add to total results
total_res <- rbind(total_res,res2)

# produce latex table from ts_2
dat <- res2[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Forecasts generated by the ARIMA models used in the System 1 algorithm.",
         "Forecasts generated by the ARIMA models used in the System 1 algorithm")
lab = 'tab:chp_ts:arima2'
filname ='../Tables/chp_ts_arima2.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# ----------------------------------------------------------------------
# --------- RM Generated Files -----------------------------------------
# --------- HYBRID ARIMA SYSTEMS ---------------------------------------

source("../RCode/ts_1.R")
source("../RCode/ts_2.R")
Mkt <- read.csv("../Data/rm_ar334_reg.csv",stringsAsFactors=F)

ts_1_2_fnc_ar <- function(fil,nm,ts1){
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i],stringsAsFactors=F)
    Mkt_p <- Mkt[,c(1,2,3,4,5,18)]
    colnames(Mkt_p) <- c("Date","Open", "High","Low","Close","p")
    if(ts1 == TRUE){
      a <- ts_1(Mkt_p, 0, nm[i])
    } else {
      a <- ts_2(Mkt_p, 0, nm[i])
    }
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-c(1),]
  return(df10)
}

# ---------- Predicting Closing Price -----------------
# 1. ------ Arima Ann Predicting Closing Price -----------------
fil <- c("../Data/ARIMA/Predict_Close/ar334_ann_DAX.csv",
         "../Data/ARIMA/Predict_Close/ar334_ann_CAC.csv",
         "../Data/ARIMA/Predict_Close/ar334_ann_FTSE.csv",
         "../Data/ARIMA/Predict_Close/ar334_ann_Dow.csv",
         "../Data/ARIMA/Predict_Close/ar334_ann_Nik.csv",
         "../Data/ARIMA/Predict_Close/ar334_ann_Oz.csv")

#nm <- c("Dax","CAC","FTSE","Dow","Nikkei","AORD")
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))

# a. System 1
res3 <- ts_1_2_fnc_ar(fil,nm,TRUE)
res3[misc_col] <- 'ARIMA/ANN Closing Price'

# Add to total results
total_res <- rbind(total_res, res3)
  
# produce latex table from ts_1
dat <- res3[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Results from passing closing price predictions from hybrid ARIMA/ANN model to System 1.",
         "Results from passing closing price predictions from hybrid ARIMA/ANN model to System 1")
lab = 'tab:chp_ts:arima_ann_sys1'
filname ='../Tables/chp_ts_arima_ann_sys1.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# comp aring to Naive Prev
res_diff3 <- sub_df_av_pl(res3,NaiveRev)

dat <- res_diff3
dig <- 0
cap <- c("Results from a trading system based on forecasts of closing price generated by the Arima/ANN model compared to baseline Naive Reversing methodology.",
         "Arima/ANN predictions passed to System 1 compared to Naive Reversing methodology")
lab = 'tab:chp_ts:arima_ann_sys1_diff'
filname ='../Tables/chp_ts_arima_ann_sys1_diff.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# a. System 2
res4 <- ts_1_2_fnc_ar(fil,nm,FALSE)
res4[misc_col] <- 'ARIMA/ANN Closing Price System 2'

# Add to total results
total_res <- rbind(total_res, res4)

# produce latex table from ts_1
dat <- res4[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Results from passing closing price predictions from hybrid ARIMA/ANN model to System 2.",
         "Results from passing closing price predictions from hybrid ARIMA/ANN model to System 2")
lab = 'tab:chp_ts:arima_ann_sys2'
filname ='../Tables/chp_ts_arima_ann_sys2.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# 2. ------ Arima knn Predicting Closing Price -----------------
fil <- c("../Data/ARIMA/Predict_Close/ar334_knn_Dax.csv",
         "../Data/ARIMA/Predict_Close/ar334_knn_CAC.csv",
         "../Data/ARIMA/Predict_Close/ar334_knn_F100.csv",
         "../Data/ARIMA/Predict_Close/ar334_knn_Dow.csv",
         "../Data/ARIMA/Predict_Close/ar334_knn_Nik.csv",
         "../Data/ARIMA/Predict_Close/ar334_knn_Oz.csv")

# a. System 1
#nm <- c("Dax","CAC","FTSE","Dow","Nikkei","AORD")
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
# a. System 1
res5 <- ts_1_2_fnc_ar(fil,nm,TRUE)
res5[misc_col] <- 'ARIMA/k-NN Closing Price'

# Add to total results
total_res <- rbind(total_res, res5)

# produce latex table from ts_1
dat <- res5[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Results from passing closing price predictions from hybrid ARIMA/k-NN model to System 1.",
         "Results from passing closing price predictions from hybrid ARIMA/k-NN model to System 1")
lab = 'tab:chp_ts:pred_close_arima_knn_sys1'
filname ='../Tables/chp_ts_pred_close_arima_knn_sys1.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# comp aring to Naive Prev
#res_diff <- sub_df_av_pl(res,NaiveRev)
res_diff5 <- sub_df(res5,NaiveRev)

# produce latex table from ts_1
dat <- res_diff5[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Results from a system using forecasts from a Arima/k-NN model with the results of the Naive Reversing System subtracted.",
         "Mean PL from hybrid ARIMA/k-NN models minus mean PL from Naive Reverse system")
lab = 'tab:chp_ts:pred_close_arima_knn_sys1_diff'
filname ='../Tables/chp_ts_pred_close_arima_knn_sys1_diff.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# a. System 
res6 <- ts_1_2_fnc_ar(fil,nm,FALSE)
res6[misc_col] <- 'ARIMA/k-NN Closing Price System 2'

# Add to total results
total_res <- rbind(total_res, res6)

# produce latex table from ts_1
dat <- res6[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Results from passing closing price predictions from hybrid ARIMA/k-NN model to System 2.",
         "Results from passing closing price predictions from hybrid ARIMA/k-NN model to System 2")
lab = 'tab:chp_ts:pred_close_arima_knn_sys2'
filname ='../Tables/chp_ts_pred_close_arima_knn_sys2.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# -----------------------------------------------------------------
#  ------ Arima Ann Predicting Up/Dn - Categorical -----------------
# a. Categorical

# 1. ARMA / ANN (Predicting Up/Dn - Categorical)
source("../RCode/ts_4.R")
source("../RCode/Utils.R")
fil <- c("../Data/ARIMA/PredUpDn_CAT/ar_334_UD_ANN_Dax.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_ANN_CAC.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_ANN_F100.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_ANN_Dow.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_ANN_N225.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_ANN_Oz.csv")
  
#nm <- c("Dax","CAC","FTSE","Dow","Nik","AORD")
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))

res7 <- ts_4_fnc_ar(fil,0, nm)
res7[misc_col] <- 'ARIMA/ANN Up/Down'

# Add to total results
total_res <- rbind(total_res, res7)

# produce latex table from ts_1
dat <- res7[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Results from a trading system using the forecast of categorical label \"U/D\" from hybrid ARIMA/ANN model.",
         "Results from a trading system using the forecast of categorical label \"U/D\" from hybrid ARIMA/ANN model")
lab = 'tab:chp_ts:pUD_CAT_arima_ann_sys'
filname ='../Tables/chp_ts_predUpDn_CAT_arima_ann_sys.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# -----------------------
# 2. ARMA / knn (Predicting Up/Dn - Categorical)
#source("../RCode/ts_4.R")
#source("../RCode/Utils.R")
fil <- c("../Data/ARIMA/PredUpDn_CAT/ar_334_UD_knn_Dax.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_knn_CAC.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_knn_F100.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_knn_Dow.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_knn_N225.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_knn_Oz.csv")

#nm <- c("Dax","CAC","FTSE","Dow","Nikkei","AORD")
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))

res8 <- ts_4_fnc_ar(fil, 0, nm)
res8[misc_col] <- 'ARIMA/k-NN Up/Down'

# Add to total results
total_res <- rbind(total_res, res8)

# produce latex table from ts_1
dat <- res8[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Results from a trading system using the forecast of categorical label \"U/D\" from hybrid ARIMA/k-NN model.",
         "Results from a trading system using the forecast of categorical label \"U/D\" from hybrid ARIMA/k-NN model")
lab = 'tab:chp_ts:pUD_CAT_arima_knn_sys'
filname ='../Tables/chp_ts_predUpDn_CAT_arima_knn_sys.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# 2. ARMA / knn (Predicting Up/Dn - Categorical) - SLoss
res8a <- ts_4_fnc_ar(fil, -100, nm)
res8a[misc_col] <- 'ARIMA/ANN Up/Down Stop Loss'

# Add to total results
total_res <- rbind(total_res, res8a)

# produce latex table from ts_1
dat <- res8a[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Results from a trading system with a stop loss using the forecast of categorical label \"U/D\" from hybrid ARIMA/k-NN model.",
         "Results from a trading system with a stop loss using the forecast of categorical label \"U/D\" from hybrid ARIMA/k-NN model")
lab = 'tab:chp_ts:pUD_CAT_arima_knn_sys_SL'
filname ='../Tables/chp_ts_predUpDn_CAT_arima_knn_sys_SL.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# comp aring to Naive Prev
#res_diff <- sub_df_av_pl(res,NaiveRev)
res_diff8 <- sub_df(res8,NaiveRev)

# produce latex table from ts_1
dat <- res_diff8[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Results from Naive Reversing System subtracted from results generated from predicting Up/Down categorical label using Arima/k-NN.",
         "Predicting UpDn CAT - Arima/k-NN predictions passed to System 4 - ")
lab = 'tab:chp_ts:pUD_CAT_arima_knn_sys_diff'
filname ='../Tables/chp_ts_predUpDn_CAT_arima_knn_sys_diff.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# 3. ARMA / Reg (Logistic) (Predicting Up/Dn - Categorical)


# 4. ARMA / SVM (Predicting Up/Dn - Categorical)
fil <- c("../Data/ARIMA/PredUpDn_CAT/ar_334_UD_svm_Dax.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_svm_CAC.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_svm_F100.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_svm_Dow2.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_svm_N225.csv",
         "../Data/ARIMA/PredUpDn_CAT/ar_334_UD_svm_Oz.csv")

#nm <- c("Dax","CAC","FTSE","Dow","Nikkei","AORD")
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))

res9 <- ts_4_fnc_ar(fil,0, nm)
res9[misc_col] <- 'ARIMA/SVM Up/Down'

# Add to total results
total_res <- rbind(total_res, res9)

# produce latex table from ts_1
dat <- res9[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Results from a trading system using the forecast of categorical label \"U/D\" from hybrid ARIMA/SVM model.",
         "Results from a trading system using the forecast of categorical label \"U/D\" from hybrid ARIMA/SVM model")
lab = 'tab:chp_ts:pUD_CAT_arima_svm_sys'
filname ='../Tables/chp_ts_predUpDn_CAT_arima_svm_sys.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# -------------------------------------------------
#  ------ Arima Ann Predicting Up/Dn - 01 ---------
source("../RCode/ts_3a.R")
#source("../RCode/Utils.R")
# 1. ARMA / ANN - (Predicting Up/Dn - 01)
fil <- c("../Data/ARIMA/PredUpDn_01/ar_334_01_ANN_Dax.csv",
         "../Data/ARIMA/PredUpDn_01/ar_334_01_ANN_CAC.csv",
         "../Data/ARIMA/PredUpDn_01/ar_334_01_ANN_FTSE.csv",
         "../Data/ARIMA/PredUpDn_01/ar_334_01_ANN_Dow.csv",
         "../Data/ARIMA/PredUpDn_01/ar_334_01_ANN_N225.csv",
         "../Data/ARIMA/PredUpDn_01/ar_334_01_ANN_Oz.csv")

#nm <- c("Dax","CAC","FTSE","Dow","Nikkei","AORD")
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))

res10 <- ts_3a_fnc_ar(fil, nm)

# produce latex table from ts_1
dat <- res10[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Results from a trading system using the forecast of a continous label from a hybrid ARIMA/ANN model.",
         "Results from a trading system using the forecast of a continous label from a hybrid ARIMA/ANN model")
lab = 'tab:chp_ts:pUD_01_arima_ann_sys'
filname ='../Tables/chp_ts_predUpDn_01_arima_ann_sys.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# -- Generate Summary tables for Chp6
# 1. Dax
colnames(total_res)[11] <- 'Methodology'
Dx <- total_res[total_res$Mkt == 'Dax',]
Dx2 <- Dx[c(11,3,4,7,10)]

# latex table
dat <- Dx2
dig <- 2
cap = c('Chapter 5 Dax Results',
        'Chapter 5 Dax Results')
lab = 'tab:chp6:dax2_summary'
filname ='../Tables/chp_6_dax2_summary.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# 2. CAC
Cc <- total_res[total_res$Mkt == 'CAC',]
Cc2 <- Cc[c(11,3,4,7,10)]

# latex table
dat <- Cc2
dig <- 2
cap = c('Chapter 5 CAC Results',
        'Chapter 5 CAC Results')
lab = 'tab:chp6:cac2_summary'
filname ='../Tables/chp_6_cac2_summary.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# 3. FTSE
Ft <- total_res[total_res$Mkt == 'FTSE',]
Ft2 <- Ft[c(11,3,4,7,10)]

# latex table
dat <- Ft2
dig <- 2
cap = c('Chapter 5 FTSE Results',
        'Chapter 5 FTSE Results')
lab = 'tab:chp6:ftse2_summary'
filname ='../Tables/chp_6_ftse2_summary.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# 4. Dow
Dw <- total_res[total_res$Mkt == 'Dow',]
Dw2 <- Dw[c(11,3,4,7,10)]

# latex table
dat <- Dw2
dig <- 2
cap = c('Chapter 5 Dow Results',
        'Chapter 5 Dow Results')
lab = 'tab:chp6:dow2_summary'
filname ='../Tables/chp_6_dow2_summary.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# 5. Nikkei
Nk <- total_res[total_res$Mkt == 'Nikkei',]
Nk2 <- Nk[c(11,3,4,7,10)]

# latex table
dat <- Nk2
dig <- 2
cap = c('Chapter 5 Nikkei Results',
        'Chapter 5 Nikkei Results')
lab = 'tab:chp6:nik2_summary'
filname ='../Tables/chp_6_nik2_summary.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# 6. Oz
Oz <- total_res[total_res$Mkt == 'AORD',]
Oz2 <- Oz[c(11,3,4,7,10)]

# latex table
dat <- Oz2
dig <- 2
cap = c('Chapter 5 AORD Results',
        'Chapter 5 AORD Results')
lab = 'tab:chp6:aord2_summary'
filname ='../Tables/chp_6_aord2_summary.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)



# # 2. ARMA / knn (Predicting Up/Dn - 01)
# 
# source("../RCode/ts_3.R")
# fil_01_ar_knn <- c("../Data/ARIMA/PredUpDn_01/ar_334_01_knn_Dax.csv",
#          "../Data/ARIMA/PredUpDn_01/ar_334_01_knn_CAC.csv",
#          "../Data/ARIMA/PredUpDn_01/ar_334_01_knn_FTSE.csv",
#          "../Data/ARIMA/PredUpDn_01/ar_334_01_knn_Dow.csv",
#          "../Data/ARIMA/PredUpDn_01/ar_334_01_knn_Nik.csv",
#          "../Data/ARIMA/PredUpDn_01/ar_334_01_knn_Oz.csv")
# #nm <- c("Dax","CAC","FTSE","Dow","Nikkei","AORD")
# df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
# 
# res11 <- ts_3_fnc_ar(fil_01_ar_knn, nm)
# 
# # produce latex table from ts_1
# dat <- res11[,c(1,3,4,5,7,8,10)]
# dig <- 0
# cap <- c("Results from a trading system using the forecast of a continous label from a hybrid ARIMA/k-NN model.",
#          "Results from a trading system using the forecast of a continous label from a hybrid ARIMA/ANN model")
# lab = 'tab:chp_ts:pUD_01_arima_knn_sys'
# filname ='../Tables/chp_ts_predUpDn_01_arima_knn_sys.tex'
# inclrnam=FALSE
# print_xt(dat,dig,cap,lab,al,filname,inclrnam)
# 
# # comp to Naive
# #res_diff <- sub_df_av_pl(NaiveRev, res)
# res_diff11 <- sub_df(res11,NaiveRev)
# 
# dat <- res_diff11[,c(1,3,4,5,7,8,10)]
# dig <- 0
# cap <- c("Results from Naive Reversing System subtracted from results generated from predicting Up/Down Numerical label using Arima/k-NN.",
#          "Predicting UpDn 01 - Arima/k-NN predictions passed to System 3.")
# lab = 'tab:chp_ts:pUD_01_arima_knn_sys_diff'
# filname ='../Tables/chp_ts_predUpDn_01_arima_knn_sys_diff.tex'
# inclrnam=FALSE
# print_xt(dat,dig,cap,lab,al,filname,inclrnam)
# 
# 
# # b3. ARMA / Reg  (Predicting Up/Dn - 01)
# fil <- c("../Data/ARIMA/PredUpDn_01/ar_334_01_Reg_Dax.csv",
#          "../Data/ARIMA/PredUpDn_01/ar_334_01_Reg_CAC.csv",
#          "../Data/ARIMA/PredUpDn_01/ar_334_01_Reg_FTSE.csv",
#          "../Data/ARIMA/PredUpDn_01/ar_334_01_Reg_Dow.csv",
#          "../Data/ARIMA/PredUpDn_01/ar_334_01_Reg_Nik.csv",
#          "../Data/ARIMA/PredUpDn_01/ar_334_01_Reg_Oz.csv")
# #nm <- c("Dax","CAC","FTSE","Dow","Nikkei","AORD")
# df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
# 
# res12 <- ts_3_fnc_ar(fil, nm)
# 
# # produce latex table from ts_1
# dat <- res12[,c(1,3,4,5,7,8,10)]
# dig <- 0
# cap <- c("Predicting UpDn 01 - Arima/Reg predictions passed to System 3",
#          "Predicting UpDn 01 - Arima/Reg predictions passed to System 3.")
# lab = 'tab:chp_ts:01_arima_reg_sys'
# filname ='../Tables/chp_ts_predUpDn_01_arima_reg_sys.tex'
# inclrnam=FALSE
# print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# END

