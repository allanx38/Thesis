# Chapter 4
setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")

# Housekeeping
library(xtable)
library(TTR)
library(candlesticks)

source("../RCode//Utils.R")
source("../RCode//NaiveLongSystem.R")
source("../RCode//NaiveLongSystem2.R")
source("../RCode//NaiveReversePrev.R")
source("../RCode//SMA_sys.R")
source("../RCode//MACD_XO.R")
source("../RCode//Aroon.R")
source("../RCode//SAR.R")
source("../RCode//Stoch.R")
source("../RCode//ROC.R")
source("../RCode//ROC2.R")
source("../RCode//MACD_OB.R")
source("../RCode//Bout_sys.R")
source("../RCode//Quant90_sys.R")
source("../RCode//Candle_Hammer.R")
source("../RCode//Candle_Hammer_aroon.R")
source("../RCode//Candle_Engulf.R")
source("../RCode//Candle_Engulf_aroon.R")
source("../RCode//Candle_Doji_aroon.R")

fil <- c("../Data/Dax_2000_d.csv",
         "../Data/CAC_2000_d.csv", 
         "../Data/F100_2000_d.csv",
         "../Data/Dow_2000_d.csv",
         "../Data/N225_2000_d.csv",
         "../Data/Oz_2000.csv")
#nm <- c("Dax", "CAC", "FTSE", "Dow", "Nikkei", "AORD")
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11)) # to hold results

std6 <- c(1,3,4,5,7,8,10)

df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
NaiveRev <- run_NaiveReversePrev(fil, 0, nm)

# ------------------------------------------
# ---------  1. Naive Long (Sub Chapter) --------

run_NaiveLongSystem <- function(fil, SLoss, nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    a <- NaiveLongSystem(Mkt, SLoss, nm[i])
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res1 <- run_NaiveLongSystem(fil,0,nm)

# produce latex table
dat <- res1[,c(1,3,5,7)]
dig <- 2
cap = c('Naive Long System. A very simple system in which the algorithm assumes the market will rise and enters a long trade each day.',
            'Results from the Naive Long System')
lab = 'tab:nlng_results'
filname ='../Tables/chp_ta_naive_long.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# ---------------------------------------------
# ---------- previous close and today's close

run_NaiveLongSystem2 <- function(fil,SLoss, nm){
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])
  a <- NaiveLongSystem2(Dax, 0, nm[i])
  df10 <- rbind(df10, a)
}
df.name <- names(a)
names(df10) <- df.name
df10 <- df10[-1,]
return(df10)
}

res2 <- run_NaiveLongSystem2(fil,0,nm)
  
# produce latex table
dat <- res2[,c(1,3,5,7)]
dig <- 2
cap = c('Naive Long System changed such that the trading period is the previous close price minus today\'s close.',
            'Results from the Naive Long System trading close to close')
lab = 'tab:nlng_results_2'
filname ='../Tables/chp_ta_naive_long_ctoc.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# -----------------------------------------------------------------------------
# ------------ Reverse Previous ------------------------------------------------
# -----------------------------------------------------------------------------

res3 <- run_NaiveReversePrev(fil, 0, nm)

# produce latex table
dat <- res3[,c(1,3,4,5,7,8,10)]
dig <- 2
cap = c('Results from a naive trading system which simply trades in the opposite direction to the previous day\'s movement.',
                      'Results from the Naive Reversing System.')
lab = 'tab:n_rev_results'
filname ='../Tables/chp_ta_naive_reverse_prev.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# repeat latex table for Chp6 - affects numbering if re-use ...
dat <- res3[,c(1,3,4,5,7,8,10)]
dig <- 2
cap = c('Results from a naive trading system which simply trades in the opposite direction to the previous day\'s movement.',
        'Results from the Naive Reversing System.')
lab = 'tab:n_rev_results_chp6'
filname ='../Tables/chp_ta_naive_reverse_prev_chp6.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# rpeat with a stop loss
res3a <- run_NaiveReversePrev(fil, -75, nm)
#tt <- sub_df(res3a,res3);tt

# produce latex table
dat <- res3a[,std6]
dig <- 2
cap = c('Naive system which reverses the previous day\'s trade direction with stop loss.',
        'Naive Following System.')
lab = 'tab:n_rev_results_sl'
filname ='../Tables/chp_ta_naive_reverse_prev_sl.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# -------------------------------------------------------------------------------
# --------------------------------------------------------------------------------
# section{Trend Detection Indicators}

# SMA
run_BaseSystem1SMA <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Dax <- read.csv(fil[i])
    a <- BaseSystem1SMA(Dax, 5, SLoss, nm[i])
    b <- BaseSystem1SMA(Dax, 25, SLoss, nm[i])
    c <- BaseSystem1SMA(Dax, 50, SLoss, nm[i])
    d <- BaseSystem1SMA(Dax, 100, SLoss, nm[i])
    e <- BaseSystem1SMA(Dax, 200, SLoss, nm[i])
    df10 <- rbind(df10, a, b, c, d, e)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res4 <- run_BaseSystem1SMA(fil,0,nm)

dat <- res4[,c(1,3,4,5,7,8,10,11)]
dig <- 2
cap = c('Results from a system based on SMA.','Results from a system based on SMA')
lab = 'tab:sma_results'
filname ='../Tables/chp_ta_sma.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# SMA SLoss
run_BaseSystem1SMA2 <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Dax <- read.csv(fil[i])
    h <- BaseSystem1SMA(Dax, 100, -50, nm[i])
    hh <- BaseSystem1SMA(Dax, 100, -100, nm[i])  #don't use i !!!!!
    df10 <- rbind(df10,h,hh)
  }
  df.name <- names(hh)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res5 <- run_BaseSystem1SMA2(fil,0,nm)

dat <- res5[,c(1,2,3,4,5,6,8,9,11)]
dig <- 2
cap =  c('Results from a system based on SMA with stop loss.',
                      'Results from a system based on SMA with stop loss')
lab = 'tab:sma_results_Sloss'
filname ='../Tables/chp_ta_sma_sloss.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# ----------------------------------------------------------------------
# subsection{Moving Average Convergence/Divergence (MACD)}
# subsubsection{MACD as trend Indicator}

run_MACD_XO <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    ma <- MACD( Mkt[,"Open"], 12, 26, 9, maType="EMA" ) #calc MACD values
    Mkt <- cbind(Mkt, ma)
    a <- MACD_XO(Mkt, SLoss, nm[i])
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res6 <- run_MACD_XO(fil,0,nm)

dat <- res6[,std6]
dig <- 2
cap =  c('Results from a system using MACD as a trend indicator.',
         'Results from a system using MACD as a trend indicator')
lab = 'tab:mac_trend_results'
filname ='../Tables/chp_ta_macd.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# ----------------------------------------------------------
# ---------------------------  Aroon ------------------------

run_aroon_sys <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    ar <- aroon(Mkt[c(3,4)], n=20)                 #calc Aroon values
    Mkt <- cbind(Mkt, ar)                           #Add Aroon values to orig data set
    a <- aroon_sys(Mkt, SLoss, nm[i])
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res7 <- run_aroon_sys(fil,0,nm)

dat <- res7[,std6]
dig <- 2
cap =  c('Results from a system based on the Aroon indicator.',
                      'Results from a system based on the Aroon indicator')
lab = 'tab:aroon_results'
filname ='../Tables/chp_ta_aroon.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# Aroon with SLoss
aroondfsl <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])                        #read data 
  ar <- aroon(Dax[c(3,4)], n=20)                 #calc Aroon values
  Dax <- cbind(Dax, ar)                           #Add Aroon values to orig data set
  a <- aroon_sys(Dax, -100, nm[i])                  #Call fnc
  aroondfsl <- rbind(aroondfsl, a)
}
df.name <- names(a)
names(aroondfsl) <- df.name 

res7a <- run_aroon_sys(fil,-100,nm)
aroondfsl <- res7a

dat <- res7a[,std6]
dig <- 2
cap =  c('Results from a system based on the Aroon indicator with stop loss.',
                      'Results from a system based on the Aroon indicator with stop loss')
lab = 'tab:aroon_results_sloss'
filname ='../Tables/chp_ta_aroon_sloss.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# Aroon - Diffs - between Aroon and Aroon with Stop Loss
aroondfsldf <- as.data.frame(matrix(seq(3),nrow=1,ncol=3))
ln <- nrow(aroondfsl)
res <- 1:3
for(i in 1:ln){
  res[1] <- aroondfsl[i,1]
  res[2] <- as.numeric(res7a[i,3]) - as.numeric(res7[i,3])
  res[3] <- as.numeric(res7a[i,4]) - as.numeric(res7[i,4])
  aroondfsldf <- rbind(aroondfsldf,res)
}
df.name <- c("Market", "Long Difference", "Short Difference")
names(aroondfsldf) <- df.name
aroondfsldf <- aroondfsldf[-1,] 

dat <- aroondfsldf[,c(1,2,3)]
dig <- 2
cap =  c('Impact of using stop loss with Aroon trend indicator.',
                      'Impact of using stop loss with Aroon trend indicator')
lab = 'tab:aroon_results_sloss_diff'
filname ='../Tables/chp_ta_aroon_sloss_diff.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# Aroon compared to baseline system
res7_diff <- sub_df_av_pl(res7,NaiveRev)
#print table
dat <- res7_diff
dig <- 0
cap =  c('Results from baseline Reversing System subtracted from Aroon results.',
         'Aroon results minus baseline')
lab = 'tab:aroon_results_diff'
filname ='../Tables/chp_ta_aroon_diff.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# -------------------------------------------------------------------------
# ------------ Trend REversal -------------------------

# ----------- SAR
run_sar_sys <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    sar <- SAR(Mkt[c(3,4)]) #HL
    Mkt <- cbind(Mkt,sar)
    a <- sar_sys(Mkt,SLoss, nm[i])
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res8 <- run_sar_sys(fil,0,nm)

dat <- res8[,std6]
dig <- 2
cap =  c('Results from a system based on the SAR indicator.',
         'Results from a system based on the SAR indicator')
lab = 'tab:sar_results'
filname ='../Tables/chp_ta_sar.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# ----------------------------------------------------------
# ---------------------------  MACD OB ------------------------

run_MACD_OB <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    ma <- MACD( Mkt[,"Open"], 12, 26, 9, maType="EMA" ) #calc MACD values
    Mkt <- cbind(Mkt, ma)                               #Add MACD values to orig data set
    lw <- quantile(Mkt$macd, na.rm=T, probs=0.15)       #Calc low val for algo
    up <- quantile(Mkt$macd, na.rm=T, probs=0.85)       #Calc up val for algo
    a <- MACD_OB(Mkt, 0, nm[i], lw, up) 
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res9 <- run_MACD_OB(fil,0,nm)

dat <- res9[,std6]
dig <- 2
cap =  c('Results from a trading system based on MACD being used as a trend reveral indicator.',
                      'Results from a system based on MACD as trend reversal indicator')
lab = 'tab:mac_ob_results'
filname ='../Tables/chp_ta_macd_ob.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


#----------------------------------------------------------
# ---------------------------  stoch ------------------------

ln <- nrow(df10)
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])
  st <- stoch(Dax[c(3,4,5)]) #HL
  Dax <- cbind(Dax,st)
  a <- stoch_sys(Dax, 0, nm[i])
  df10 <- rbind(df10, a)
}
df10 <- df10[-c(1:ln-1),]

run_stoch_sys <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    st <- stoch(Mkt[c(3,4,5)]) #HL
    Mkt <- cbind(Mkt,st)
    a <- stoch_sys(Mkt, SLoss, nm[i]) 
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res10 <- run_stoch_sys(fil,0,nm)

dat <- res10[,std6]
dig <- 2
cap =  c('Results from a system based on the Stochastic indicator.',
                      'Results from a system based on the Stochastic indicator')
lab = 'tab:stoch_results'
filname ='../Tables/chp_ta_stoch.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# Stock plus SLoss
res10a <- run_stoch_sys(fil,-100,nm)

dat <- res10a[,std6]
dig <- 2
cap =  c('Results from a system based on the Stochastic indicator with a stop loss.',
         'Results from a system based on the Stochastic indicator with a stop loss')
lab = 'tab:stoch_results_sloss'
filname ='../Tables/chp_ta_stoch_sloss.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

#----------------------------------------------------------
# ---------------------------  ROC ------------------------

run_roc_sys <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    roc <- ROC( Mkt$Close )                          #calc MACD values
    Mkt <- cbind(Mkt, roc)                           #Add MACD values to orig data set
    lw <- quantile(Mkt$roc, na.rm=T, probs=0.15)     #Calc low val for algo
    up <- quantile(Mkt$roc, na.rm=T, probs=0.85)     #Calc up val for algo
    a <- roc_sys(Mkt, SLoss, nm[i], lw, up)          
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res11 <- run_roc_sys(fil,0,nm)

dat <- res11[,std6]
dig <- 2
cap =  c('Results from a system based on the ROC indicator.',
         'Results from a system based on the ROC indicator')
lab = 'tab:mac_roc_results'
filname ='../Tables/chp_ta_roc.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# ROC 2
#If previous ROC was greater or smaller than 0:
#source("../RCode//ROC2.R")
# ln <- nrow(df10)
# #results <- 1:11
# for(i in 1:length(fil)){
#   Mkt <- read.csv(fil[i])                          #read data 
#   roc <- ROC( Mkt$Close )                          #calc MACD values
#   Mkt <- cbind(Mkt, roc)                           #Add MACD values to orig data set
#   lw <- quantile(Mkt$roc, na.rm=T, probs=0.15)     #Calc low val for algo
#   up <- quantile(Mkt$roc, na.rm=T, probs=0.85)     #Calc up val for algo
#   a <- roc_sys2(Mkt, 0, nm[i])              #Call fnc
#   df10 <- rbind(df10, a)               #add results
# }
# df10 <- df10[-c(1:ln-1),]                #NOTE ln-1 !!!!!
# 
# dat <- df10[-1,std6]
# dig <- 2
# cap =  c('ROC2.',
#                                 'ROC2')
# lab = 'tab:mac_roc2_results'
# filname ='../Tables/chp_ta_roc2.tex'
# inclrnam=FALSE
# print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# -----------------------------------------------------------------
# -------------section{Break-out systems}

#----------------------------------------------------------------
# ---------------------------  Break Out ------------------------
run_BaseSystem2Bout <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    a <- BaseSystem2Bout(Mkt, SLoss, nm[i])         
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res12 <- run_BaseSystem2Bout(fil,0,nm)

dat <- res12[,std6]
dig <- 2
cap =  c('Results from the Daily High / Low Breakout System.',
         'Results from the Daily High / Low Breakout System')
lab = 'tab:hl_bout_sys'
filname ='../Tables/chp_ta_b_out.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# comp to Naive
res_diff <- sub_df(res12,NaiveRev)

dat <- res_diff[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Results from Daily High / Low Breakout System compared with Naive Reversing System",
         "Daily High / Low Breakout System compared with Naive Reversing System")
lab = 'tab:hl_bout_sys_diff'
filname ='../Tables/chp_ta_b_out_diff.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


#----------------------------------------------------------------
# ---------------------------  90% Quant ------------------------

run_BaseSystem3Quant902 <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    a <- BaseSystem3Quant902(Mkt, SLoss, nm[i])        
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res14 <- run_BaseSystem3Quant902(fil,0,nm)

dat <- res14[,std6]
dig <- 2
cap =  c('Results from a system that breaks out from the 90\\% quantile level of the day\'s minor move.',
         'Results from a break out system using the day\'s the minor move')
lab = 'tab:q_90_results'
filname ='../Tables/chp_ta_90q.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# comp to Naive
res_diff <- sub_df(res14,NaiveRev)

dat <- res_diff[,c(1,3,4,5,7,8,10)]
dig <- 0
cap <- c("Results 90\\% Quantile level Breakout System compared with Naive Reversing System",
         "Daily 90\\% Quantile level Breakout System compared with Naive Reversing System")
lab = 'tab:chp_ta_90q_diff'
filname ='../Tables/chp_ta_90q_diff.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# ----------------------------------------------------------------------------
# -------section{Candlestick Patterns}

run_candle_hammer <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i],stringsAsFactors = FALSE)
    Mkt <- Mkt[,c(1,2,3,4,5)]
    Mkt$Date <- as.POSIXct(Mkt$Date,format='%d/%m/%Y')
    Mkt_xts <- xts(Mkt[,c(2,3,4,5)],Mkt$Date)
    hh <- as.data.frame(CSPHammer(Mkt_xts))
    hi <- as.data.frame(CSPInvertedHammer(Mkt_xts))
    Mkt <- cbind(Mkt,hh)
    Mkt <- cbind(Mkt,hi)
    a <- candle_hammer(Mkt,SLoss, nm[i])        
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res14 <- run_candle_hammer(fil,0,nm)

# latex table
dat <- res14[,c(1,3,5,6,7)]
dig <- 2
cap = c('Results from a system based on the Hammer and Inverted Hammer candlestick patterns.',
        'Results from a system based on the Hammer and Inverted Hammer candlestick patterns')
lab = 'tab:hammer_results'
filname ='../Tables/chp_ta_hammer.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# plus aroon
run_candle_hammer_aroon <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i],stringsAsFactors = FALSE)
    Mkt <- Mkt[,c(1,2,3,4,5)]
    Mkt$Date <- as.POSIXct(Mkt$Date,format='%d/%m/%Y')
    Mkt_xts <- xts(Mkt[,c(2,3,4,5)],Mkt$Date)
    hh <- as.data.frame(CSPHammer(Mkt_xts))
    hi <- as.data.frame(CSPInvertedHammer(Mkt_xts))
    Mkt <- cbind(Mkt,hh)
    Mkt <- cbind(Mkt,hi)
    ar <- aroon(Mkt$Close,n=20)
    Mkt <- cbind(Mkt,ar)
    a <- candle_hammer_aroon(Mkt,SLoss, nm[i])        
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res14a <- run_candle_hammer_aroon(fil,0,nm)

# latex table
dat <- res14a[,c(1,3,5,6,7)]
dig <- 2
cap =  c('Results from a system based on the Hammer and Inverted Hammer candlestick patterns occurring in a downtrend as defined by the aroon value.',
         'Results from a system based on the Hammer and Inverted Hammer candlestick patterns occurring in a downtrend')
lab = 'tab:hammer_aroon_results'
filname ='../Tables/chp_ta_hammer_d_trend.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# -------------------------------------------------------------
# -----------------  Engulfing Candlestick -------------------

run_candle_engulf <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i],stringsAsFactors = FALSE)
    #create xts obj
    Mkt$Date <- as.POSIXct(Mkt$Date,format='%d/%m/%Y')
    Mkt_xts <- xts(Mkt[,c(2,3,4,5)],Mkt$Date)
    en <- as.data.frame(CSPEngulfing(Mkt_xts))
    #use data frame again
    Mkt <- cbind(Mkt,en)
    a <- candle_engulf(Mkt,SLoss, nm[i])        
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res15 <- run_candle_engulf(fil,0,nm)

# latex table
dat <- res15[,std6]
dig <- 2
cap =  c('Results from a system based on the Engulfing candlestick pattern.',
          'Results from a system based on the Engulfing candlestick pattern')
lab = 'tab:engulf_results'
filname ='../Tables/chp_ta_englf.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


# with Aroon
run_candle_engulf_aroon <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i],stringsAsFactors = FALSE)
    #create xts obj
    Mkt$Date <- as.POSIXct(Mkt$Date,format='%d/%m/%Y')
    Mkt_xts <- xts(Mkt[,c(2,3,4,5)],Mkt$Date)
    en <- as.data.frame(CSPEngulfing(Mkt_xts))
    #use data frame again
    Mkt <- cbind(Mkt,en)
    ar <- aroon(Mkt$Close,n=20)
    Mkt <- cbind(Mkt,ar)
    a <- candle_engulf_aroon(Mkt,SLoss, nm[i])        
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res15a <- run_candle_engulf_aroon(fil,0,nm)

# latex table
dat <- res15a[,std6]
dig <- 2
cap =  c('Results from a system based on the Engulfing candlestick pattern in a trending market.',
         'Results from a system based on the Engulfing candlestick pattern in a trending market')
lab = 'tab:engulf_aroon_results'
filname ='../Tables/chp_ta_englf_aroon.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# ----------------------------------------------------------
# ---------------------------  Doji ------------------------
run_candle_doji_aroon <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i],stringsAsFactors = FALSE)
    #create xts obj
    Mkt$Date <- as.POSIXct(Mkt$Date,format='%d/%m/%Y')
    Mkt_xts <- xts(Mkt[,c(2,3,4,5)],Mkt$Date)
    dj <- as.data.frame(CSPDoji(Mkt_xts))
    #back to data fram
    Mkt <- cbind(Mkt,dj)
    ar <- aroon(Mkt$Close,n=20)
    Mkt <- cbind(Mkt,ar)
    a <- candle_doji_aroon(Mkt,SLoss, nm[i])        
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res16 <- run_candle_doji_aroon(fil,0,nm)

# latex table
dat <- res16[,std6]
dig <- 2
cap = c('Results from a system based on the Doji candlestick pattern in a trending market.',
        'Results from a system based on the Doji candlestick pattern in a trending market')
lab = 'tab:doji_aroon_results'
filname ='../Tables/chp_ta_doji.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# END
