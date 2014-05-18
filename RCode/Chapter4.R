# Chapter 4
setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")

# Housekeeping
library(xtable)
library(TTR)
library(candlesticks)

source("../RCode//Utils.R")
source("../RCode//NaiveLongSystem.R")
source("../RCode//NaiveLongSystem2.R")
source("../RCode//NaiveFollowPrev.R")
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
nm <- c("Dax", "CAC", "F100", "Dow", "Nik", "Oz")
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11)) # to hold results

#s <- read.csv('../Data/Dax_2000_d.csv')

# ------------------------------------------
# ---------  Naive Long Sub Chapter --------

for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])
  a <- NaiveLongSystem(Dax, 0, nm[i])
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- rbind(df10, a)
}

# Print a table
xt <- xtable(
  df10[-1,c(1,3,5,6)],
  digits = 2,
  caption = c('Naive Long System. A very simple system in which the algorithm assumes the market will rise and enters a long trade each day.',
              'Naive Long System'),
  label = 'tab:nlng_results'
)

align(xt) <- c('l','l','c','c','c')
print(
  xt, 
  file='../Tables/chp_ta_naive_long.tex',
  include.rownames=FALSE,  
  caption.placement = "top",
  hline.after=NULL,
  add.to.row=list(pos=list(-1,0, nrow(xt)),
                  command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))

# ---------------------------------------------
# ---------- previous close and today's close

ln <- nrow(df10)
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])
  a <- NaiveLongSystem2(Dax, 0, nm[i])
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- rbind(df10, a)
}
df10 <- df10[-c(1:ln-1),]


xt <- xtable(
  df10[-1,c(1,3,5,6)], 
  digits = 2, 
  caption = c('Naive Long System changed such that the trading period is the previous close price minus today\'s close.',
              'Naive Long System - - Close to Close'),
  label = 'tab:nlng_results_2'
)
align(xt) <- c('l','l','c','c','c')
print(xt, 
      file='../Tables/chp_ta_naive_long_ctoc.tex',
      include.rownames=FALSE, 
      caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))

# -----------------------------------------------------------------------------
# ------------ Follow Previous ------------------------------------------------
# -----------------------------------------------------------------------------

ln <- nrow(df10)
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])
  a <- NaiveFollowPrev(Dax, 0, nm[i])
  df10 <- rbind(df10, a)
}
df10 <- df10[-c(1:ln-1),]        #NOTE ln -1


xt <- xtable(
  df10[-1,c(1,3,4,5,6,8,9)], 
  digits = 2, 
  caption = c('Naive system which repeats the previous day\'s trade direction.',
              'Naive Following System.'),
  label = 'tab:ntfresults')

align(xt) <- c('l','l','c','c','c','c','c','c')
print(
  xt, 
  file='../Tables/chp_ta_naive_follow_prev.tex',
  include.rownames=FALSE, 
  caption.placement = "top",
  hline.after=NULL,
  add.to.row=list(pos=list(-1,0, nrow(xt)),
                  command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))


# -------------------------------------------------------------------------------
# --------------------------------------------------------------------------------
# section{Trend Detection Indicators}

# SMA
ln <- nrow(df10)
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])
  a <- BaseSystem1SMA(Dax, 5, 0, nm[i])
  b <- BaseSystem1SMA(Dax, 25, 0, nm[i])
  c <- BaseSystem1SMA(Dax, 50, 0, nm[i])
  d <- BaseSystem1
  (Dax, 100, 0, nm[i])
  e <- BaseSystem1SMA(Dax, 200, 0, nm[i])
  df10 <- rbind(df10, a, b, c, d, e)
}
df10 <- df10[-c(1:ln-1),]

xt <- xtable(df10[-1,c(1,3,4,5,6,8,9,11)], digits = 2, 
             caption = c('Results from SMA system.','SMA Base System'),
             label = 'tab:sma_results')
align(xt) <- c('l','l','c','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_sma.tex',
      include.rownames=FALSE,caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))


# SMA with SLoss

ln <- nrow(df10)
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])
  #f <- BaseSystem1SMA(Dax, 5, -50, nm[i])
  #g <- BaseSystem1SMA(Dax, 5, -100, nm[i])
  h <- BaseSystem1SMA(Dax, 100, -50, nm[i])
  hh <- BaseSystem1SMA(Dax, 100, -100, nm[i])  #don't use i !!!!!
  df10 <- rbind(df10,h,hh)
}
df10 <- df10[-c(1:ln-1),]


xt <- xtable(
  df10[-1,c(1,2,3,4,5,6,8,9,11)], 
  digits = 2,
  caption = c('Results from SMA system with Stop Loss.',
              'SMA Base System with Stop Loss'),
  label = 'tab:sma_results_Sloss')
align(xt) <- c('l','l','c','c','c','c','c','c','c','c')
print(
  xt, 
  file='../Tables/chp_ta_sma_sloss.tex',
  include.rownames=FALSE, 
  caption.placement = "top",
  hline.after=NULL,
  add.to.row=list(pos=list(-1,0, nrow(xt)),
                  command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))

# ----------------------------------------------------------------------
# subsection{Moving Average Convergence/Divergence (MACD)}
# subsubsection{MACD as trend Indicator}

ln <- nrow(df10)
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])
  ma <- MACD( Dax[,"Open"], 12, 26, 9, maType="EMA" ) #calc MACD values
  Dax <- cbind(Dax, ma)                               #Add MACD values to orig data set
  a <- MACD_XO(Dax, 0, nm[i])
  df10 <- rbind(df10, a)
}
df10 <- df10[-c(1:ln-1),]


  xt <- xtable(df10[-1,c(1,3,4,5,6,8,9)], 
               digits = 2, 
               caption = c('MACD used a trend indicator.','MACD as Trend Indicator'),
               label = 'tab:mac_trend_results')
align(xt) <- c('l','l','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_macd.tex',
      include.rownames=FALSE, caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))


# ----------------------------------------------------------
# ---------------------------  Aroon ------------------------

ln <- nrow(df10)
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])                        #read data 
  ar <- aroon(Dax[c(3,4)], n=20)                 #calc Aroon values
  Dax <- cbind(Dax, ar)                           #Add Aroon values to orig data set
  a <- aroon_sys(Dax, 0, nm[i])                  #Call fnc
  df10 <- rbind(df10, a)             #add results
}
df10 <- df10[-c(1:ln-1),]                #NOTE ln-1 !!!!!


xt <- xtable(df10[-1,c(1,3,4,5,6,8,9)], digits = 2, 
             caption = c('Aroon trend indicator.',
                         'Aroon trend indicator'),
             label = 'tab:aroon_results')
align(xt) <- c('l','l','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_aroon.tex',
      include.rownames=FALSE, caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))


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


xt <- xtable(aroondfsl[-1,c(1,3,4,5,6,8,9)], digits = 2, 
             caption = c('Aroon trend indicator with stop loss.',
                         'Aroon trend indicator with Stop Loss'),
             label = 'tab:aroon_results_sloss')
align(xt) <- c('l','l','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_aroon_sloss.tex',
      include.rownames=FALSE, caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))


# Diffs

aroondfsldf <- as.data.frame(matrix(seq(3),nrow=1,ncol=3))
ln <- nrow(aroondfsl)
res <- 1:3
for(i in 1:ln){
  res[1] <- aroondfsl[i,1]
  res[2] <- as.numeric(aroondfsl[i,3]) - as.numeric(df10[i,3])
  res[3] <- as.numeric(aroondfsl[i,4]) - as.numeric(df10[i,4])
  aroondfsldf <- rbind(aroondfsldf,res)
}
df.name <- c("Market", "Long Difference", "Short Difference")
names(aroondfsldf) <- df.name
aroondfsldf <- aroondfsldf[-1,] 


xt <- xtable(aroondfsldf[-1,c(1,2,3)], digits = 2, 
             caption = c('Impact of stop loss on Aroon.',
                         'Impact of using stop loss with Aroon trend indicator.'),
             label = 'tab:aroon_results_sloss_diff')
align(xt) <- c('l','l','c','c')
print(xt, 
      file='../Tables/chp_ta_aroon_diff.tex',
      include.rownames=FALSE, caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))

# -------------------------------------------------------------------------
# ------------ Trend REversal -------------------------

# ----------- SAR
ln <- nrow(df10)
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])
  sar <- SAR(Dax[c(3,4)]) #HL
  Dax <- cbind(Dax,sar)
  a <- sar_sys(Dax,0, nm[i])
  df10 <- rbind(df10, a)
}
df10 <- df10[-c(1:ln-1),]

xt <- xtable(df10[-1,c(1,3,4,5, 6, 9)], digits = 2, 
             caption = c('Results from SAR system.','SAR Base System'),
             label = 'tab:sar_results')
align(xt) <- c('l','l','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_sar.tex',
      include.rownames=FALSE,caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))

# ----------------------------------------------------------
# ---------------------------  MACD OB ------------------------

ln <- nrow(df10)
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])                             #read data 
  ma <- MACD( Dax[,"Open"], 12, 26, 9, maType="EMA" ) #calc MACD values
  Dax <- cbind(Dax, ma)                               #Add MACD values to orig data set
  lw <- quantile(Dax$macd, na.rm=T, probs=0.15)       #Calc low val for algo
  up <- quantile(Dax$macd, na.rm=T, probs=0.85)       #Calc up val for algo
  a <- MACD_OB(Dax, 0, nm[i], lw, up)                 #Call fnc
  df10 <- rbind(df10, a)                  #add results
}
df10 <- df10[-c(1:ln-1),]                #NOTE ln-1 !!!!!


xt <- xtable(df10[-1,c(1,3,4,5, 6, 8,9)], digits = 2, 
             caption = c('MACD can also be used as a trend reveral indicator.',
                         'MACD as Trend Reversal Indicator'),
             label = 'tab:mac_ob_results')
align(xt) <- c('l','l','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_macd_ob.tex',
      include.rownames=FALSE, caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))


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

xt <- xtable(df10[-1,c(1,3,4,5,6,8,9)], digits = 2, 
             caption = c('Results from Stochastics system.',
                         'Stochastics system'),
             label = 'tab:stoch_results')
align(xt) <- c('l','l','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_stoch.tex',
      include.rownames=FALSE, caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))


ln <- nrow(df10)
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])
  st <- stoch(Dax[c(3,4,5)]) #HL
  Dax <- cbind(Dax,st)
  a <- stoch_sys(Dax, -100, nm[i])
  df10 <- rbind(df10, a)
}
df10 <- df10[-c(1:ln-1),]

xt <- xtable(df10[-1,c(1,3,4,5,6,8,9)], digits = 2, 
             caption = c('Results from Stochastics system and using a Stop Loss.',
                         'Stochastics system with stop loss'),
             label = 'tab:stoch_results_sloss')
align(xt) <- c('l','l','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_stoch_sloss.tex',
      include.rownames=FALSE, caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))

#----------------------------------------------------------
# ---------------------------  ROC ------------------------

ln <- nrow(df10)
#results <- 1:11
for(i in 1:length(fil)){
  Mkt <- read.csv(fil[i])                          #read data 
  roc <- ROC( Mkt$Close )                          #calc MACD values
  Mkt <- cbind(Mkt, roc)                           #Add MACD values to orig data set
  lw <- quantile(Mkt$roc, na.rm=T, probs=0.15)     #Calc low val for algo
  up <- quantile(Mkt$roc, na.rm=T, probs=0.85)     #Calc up val for algo
  a <- roc_sys(Mkt, 0, nm[i], lw, up)              #Call fnc
  df10 <- rbind(df10, a)               #add results
}
df10 <- df10[-c(1:ln-1),]                #NOTE ln-1 !!!!!


xt <- xtable(df10[-1,c(1,3,4,5,6,8,9)], digits = 2, 
             caption = c('ROC.',
                         'ROC'),
             label = 'tab:mac_ob_results')
align(xt) <- c('l','l','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_roc.tex',
      include.rownames=FALSE, caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))


#If previous ROC was greater or smaller than 0:

#source("../RCode//ROC2.R")
ln <- nrow(df10)
#results <- 1:11
for(i in 1:length(fil)){
  Mkt <- read.csv(fil[i])                          #read data 
  roc <- ROC( Mkt$Close )                          #calc MACD values
  Mkt <- cbind(Mkt, roc)                           #Add MACD values to orig data set
  lw <- quantile(Mkt$roc, na.rm=T, probs=0.15)     #Calc low val for algo
  up <- quantile(Mkt$roc, na.rm=T, probs=0.85)     #Calc up val for algo
  a <- roc_sys2(Mkt, 0, nm[i])              #Call fnc
  df10 <- rbind(df10, a)               #add results
}
df10 <- df10[-c(1:ln-1),]                #NOTE ln-1 !!!!!

xt <- xtable(df10[-1,c(1,3,4,5,6,8,9)], digits = 2, 
             caption = c('ROC2.',
                         'ROC2'),
             label = 'tab:mac_ob_results')
align(xt) <- c('l','l','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_roc2.tex',
      include.rownames=FALSE, caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))


# -----------------------------------------------------------------
# -------------section{Break-out systems}

#----------------------------------------------------------------
# ---------------------------  Break Out ------------------------
ln <- nrow(df10)
for(i in 1:length(fil)){
Dax <- read.csv(fil[i])
a <- BaseSystem2Bout(Dax, 0, nm[i])
df10 <- rbind(df10, a)
}
df10 <- df10[-c(1:ln-1),]

xt <- xtable(
  df10[-1,c(1,3,4,5,6,8,9)], 
  digits = 2,
  caption = c('Results from Daily High / Low Breakout System.',
              'Daily High / Low Breakout System'),
  label = 'tab:hl_bout_sys')
align(xt) <- c('l','l','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_b_out.tex',
      include.rownames=FALSE, 
      caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))

#----------------------------------------------------------------
# ---------------------------  90% Quant ------------------------

ln <- nrow(df10)
for(i in 1:length(fil)){
Dax <- read.csv(fil[i])
a <- BaseSystem3Quant902(Dax, 0, nm[i])
df10 <- rbind(df10, a)
}
df10 <- df10[-c(1:ln-1),]

xt <- xtable(df10[-1,c(1,3,4,5,6,8,9)], digits = 2, 
             caption = c('Base System 3 - 90 Quantile.','Base System 3'),
             label = 'tab:q_90_results')
align(xt) <- c('l','l','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_90q.tex',
      include.rownames=FALSE, caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))

# ----------------------------------------------------------------------------
# -------section{Candlestick Patterns}

ln <- nrow(df10)
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i],stringsAsFactors = FALSE)
  Dax <- Dax[,c(1,2,3,4,5)]
  Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
  Dax_xts <- xts(Dax[,c(2,3,4,5)],Dax$Date)
  hh <- as.data.frame(CSPHammer(Dax_xts))
  hi <- as.data.frame(CSPInvertedHammer(Dax_xts))
  Dax <- cbind(Dax,hh)
  Dax <- cbind(Dax,hi)
  a <- candle_hammer(Dax,0, nm[i])
  df10 <- rbind(df10, a)
}
df10 <- df10[-c(1:ln-1),]

xt <- xtable(df10[-1,c(1,3,5, 6)], digits = 2, 
             caption = c('Results from Hammer / Inverted Hammer.','Hammer System'),
             label = 'tab:hammer_results')
align(xt) <- c('l','l','c','c','c')
print(xt, 
      file='../Tables/chp_ta_hammer.tex',
      include.rownames=FALSE,caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))


ln <- nrow(df10)
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i],,stringsAsFactors = FALSE)
  Dax <- Dax[,c(1,2,3,4,5)]
  Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
  Dax_xts <- xts(Dax[,c(2,3,4,5)],Dax$Date)
  hh <- as.data.frame(CSPHammer(Dax_xts))
  hi <- as.data.frame(CSPInvertedHammer(Dax_xts))
  Dax <- cbind(Dax,hh)
  Dax <- cbind(Dax,hi)
  ar <- aroon(Dax$Close,n=20)
  Dax <- cbind(Dax,ar)
  a <- candle_hammer_aroon(Dax,0, nm[i])
  df10 <- rbind(df10, a)
}
df10 <- df10[-c(1:ln-1),]


xt <- xtable(df10[-1,c(1,3,5, 6)], digits = 2, 
             caption = c('Results from Hammer / Inverted Hammer occurring in a downtrend as defined by the aroon value.','Hammer System in downtrend.'),
             label = 'tab:hammer_aroon_results')
align(xt) <- c('l','l','c','c','c')
print(xt, 
      file='../Tables/chp_ta_hammer_d_trend.tex',
      include.rownames=FALSE,caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))


# -------------------------------------------------------------
# -----------------  Engulfing Candlestick -------------------

ln <- nrow(df10)
for(i in 1:length(fil)){
  #data frame
  Dax <- read.csv(fil[i],stringsAsFactors = FALSE)
  #create xts obj
  Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
  Dax_xts <- xts(Dax[,c(2,3,4,5)],Dax$Date)
  en <- as.data.frame(CSPEngulfing(Dax_xts))
  #use data fram again
  Dax <- cbind(Dax,en)
  ar <- aroon(Dax$Close,n=20)
  Dax <- cbind(Dax,ar)
  a <- candle_engulf(Dax,0, nm[i])
  df10 <- rbind(df10, a)
}
df10 <- df10[-c(1:ln-1),]


xt <- xtable(df10[-1,c(1,3,4,5,6,8,9)], digits = 2, 
             caption = c('Results from Engulfing Candlestick.','Engulfing Candlestick System'),
             label = 'tab:engulf_results')
align(xt) <- c('l','l','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_englf.tex',
      include.rownames=FALSE,caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))


ln <- nrow(df10)
for(i in 1:length(fil)){
  #data frame
  Dax <- read.csv(fil[i],stringsAsFactors = FALSE)
  #create xts obj
  Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
  Dax_xts <- xts(Dax[,c(2,3,4,5)],Dax$Date)
  en <- as.data.frame(CSPEngulfing(Dax_xts))
  #use data fram again
  Dax <- cbind(Dax,en)
  ar <- aroon(Dax$Close,n=20)
  Dax <- cbind(Dax,ar)
  a <- candle_engulf_aroon(Dax,0, nm[i])
  df10 <- rbind(df10, a)
}
df10 <- df10[-c(1:ln-1),]


xt <- xtable(df10[-1,c(1,3,4,5,6,8,9)], digits = 2, 
             caption = c('Results from Engulfing Candlestick.','Engulfing Candlestick System'),
             label = 'tab:engulf_aroon_results')
align(xt) <- c('l','l','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_englf_aroon.tex',
      include.rownames=FALSE,caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))


# ----------------------------------------------------------
# ---------------------------  Doji ------------------------

ln <- nrow(df10)
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i],,stringsAsFactors = FALSE)
  Dax <- Dax[,c(1,2,3,4,5)]
  #xts object
  Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
  Dax_xts <- xts(Dax[,c(2,3,4,5)],Dax$Date)
  dj <- as.data.frame(CSPDoji(Dax_xts))
  #back to data fram
  Dax <- cbind(Dax,dj)
  ar <- aroon(Dax$Close,n=20)
  Dax <- cbind(Dax,ar)
  a <- candle_doji_aroon(Dax,0, nm[i])
  df10 <- rbind(df10, a)
}
df10 <- df10[-c(1:ln-1),]

#Print table
xt <- xtable(df10[-1,c(1,3,4,5,6,8,9)], digits = 2, 
             caption = c('Results from Doji Candlestick.','Doji Candlestick System'),
             label = 'tab:doji_aroon_results')
align(xt) <- c('l','l','c','c','c','c','c','c')
print(xt, 
      file='../Tables/chp_ta_doji.tex',
      include.rownames=FALSE,caption.placement = "top",
      hline.after=NULL,
      add.to.row=list(pos=list(-1,0, nrow(xt)),
                      command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))