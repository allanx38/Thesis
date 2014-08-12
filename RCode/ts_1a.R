setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")

# source
source("../RCode/Utils.R")

# include
library(TTR)
#library(stargazer)
library(ggplot2)
library(xtable)


fil <- c("../Data/Dax_2000_d.csv",
         "../Data/CAC_2000_d.csv", 
         "../Data/F100_2000_d.csv",
         "../Data/Dow_2000_d.csv",
         "../Data/N225_2000_d.csv",
         "../Data/Oz_2000.csv")
nm <- c("Dax", "CAC", "F100", "Dow", "Nik", "Oz")

Dax <- read.csv("../Data/Dax_2000_d.csv", stringsAsFactors = FALSE)
tail(Dax)
atr <- ATR(Dax[,c('High','Low','Close')],n=14)
Dax$atr <- atr[,2]
Dax$atr_av <- ifelse(Dax$atr>0,round((Dax$atr/Dax$Close)*100),NA)
Dax$atr_av <- round((Dax$atr/Dax$Close)*100,2)

nrow(Dax)
dd <- Dax[Dax$atr_av > 5,]
nrow(dd)
dd$pl <- dd$Close - dd$Open
tail(dd)
sum(dd$pl, na.rm=T)
sum(dd$pl, na.rm=T) / nrow(dd)


max(Dax$atr_av,na.rm=T)

