setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")

# libraries
library(forecast)
library(xtable)
library(TTR)

#source
source("../RCode/Utils.R")
source("../RCode/ROC2.R")


fil <- c("../Data/Dax_2000_d.csv",
         "../Data/CAC_2000_d.csv", 
         "../Data/F100_2000_d.csv",
         "../Data/Dow_2000_d.csv",
         "../Data/N225_2000_d.csv",
         "../Data/Oz_2000.csv")
nm <- c("Dax", "CAC", "FTSE", "Dow", "Nikkei", "AORD")
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11)) # to hold results

run_roc_sys <- function(fil,SLoss,nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    #roc <- ROC( Mkt$Close )                         
    roc <- momentum( Mkt$Close, n=25)
    Mkt <- cbind(Mkt, roc)                           #Add MACD values to orig data set
    #lw <- quantile(Mkt$roc, na.rm=T, probs=0.15)     #Calc low val for algo
    #up <- quantile(Mkt$roc, na.rm=T, probs=0.85)     #Calc up val for algo
    a <- roc_sys2(Mkt, SLoss, nm[i])          
    df10 <- rbind(df10,a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

res11 <- run_roc_sys(fil,0,nm)
res11


dx <- read.csv("../Data/Dax_2000_d.csv")
roc <- ROC( dx$Close,n=10 )
rocm <- momentum( dx$Close,n=10 )
dx <- cbind(dx, roc)
dx <- cbind(dx, rocm)
tail(dx,n=50)
tail(roc)
summary(roc)
