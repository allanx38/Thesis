# Chapter 3

setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")

# source
source("../RCode/Utils.R")

# include
library(TTR)
library(stargazer)
library(ggplot2)
library(xtable)

# 1. head of Dax
Dax <- read.csv("../Data/Dax_2000_d.csv")

# produce latex table
dat <- head(Dax)
dig <- 0
cap <- c("First 6 rows of the Dax data set","First 6 rows of the Dax data set.")
lab = 'tab:daxhead'
filname ='../Tables/chp_3_1_daxhead.tex'
inclrnam=F
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# 2. tail of Dax
dat <- tail(Dax)
dig <- 0
cap <- c("Final 6 rows of the Dax data set","Final 6 rows of the Dax data set.")
lab = 'tab:daxtail'
filname ='../Tables/chp_3_1_daxtail.tex'
inclrnam=F
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# 3. summary stats
#print(summary(Dax))

# 4. plot Dax 2000 - 2013
plot(Dax$Close)
