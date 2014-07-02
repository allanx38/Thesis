# 1. Exp Smoothing
setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")
library(forecast)

exp_sm <- function(Mkt_ts, Mkt, strt){
  #browser()
  Mkta <- Mkt
  cc <- Mkta[1,]
  cc$a <- 0
  ln <- nrow(Mkt)
  #lb <- 300 #lookback
  for(i in strt:ln){
    st <- i-30
    Mkt_slice <- window(Mkt_ts,start=st,end=i)
    modf <- ets(Mkt_slice)
    #fcast <- forecast.ets(mod)
    #a <- fcast$fitted[300]
    a <- modf$method
    b <- Mkta[i,]
    ab <- cbind(b,a)
    cc <- rbind(cc,ab)
  }
  cc <- cc[-1,]
  return(cc)
}

Mkt <- read.csv("../Data/Dax_2000_d.csv")
tail(Mkt)
Mkt_ts <- ts(Mkt$Close)
Mkt_train <- window(Mkt_ts, start=2000, end=2030)
mod <- ets(Mkt_ts)
mod$method
mod$fitted
fcast <- forecast.ets(mod, h=5)

as <- exp_sm(Mkt_ts,Mkt, 3000)

nrow(Mkt)
Mkt_train <- window(Mkt_ts, start=2000, end=2030)
tail(Mkt_train)
mod2 <- ets(Mkt_train)
mod2
fcast2 <- forecast.ets(mod2, h=5)
fcast2


# nrow(Mkt)
# Mkt$Date[2999]
# Mkt_ts <- ts(Mkt$Close)
# #Mkt_ts <- ts(Mkt$Close,frequency=252, start=c(2000,1))
# #Mkt_train <- window(Mkt_ts, start=2000, end=2009.99)
# Mkt_train <- window(Mkt_ts, end=2999.99)
# Mkt_test <- window(Mkt_ts, start=3000)
# 
# # a.build the  mean model
# mean_model <- ets(Mkt_train)
# a <- accuracy(mean_model, Mkt_test) #out of sample
# rownames(a) <- c('Mean Training Set', 'Mean Test Set')
# a

# exp_sm <- function(Mkt_ts, Mkt, st){
#   #browser()
#   Mkta <- Mkt
#   cc <- Mkta[1,]
#   cc$a <- 0
#   ln <- nrow(Mkt)
#   lb <- 300 #lookback
#   for(i in 301:ed){
#     st <- i-300
#     Mkt_slice <- window(Mkt_ts,start=st,end=i)
#     mod <- ets(Mkt_slice, model="AAN")
#     fcast <- forecast.ets(mod)
#     a <- fcast$fitted[300]
#     b <- Mkta[i,]
#     ab <- cbind(b,a)
#     cc <- rbind(cc,ab)
#   }
#   cc <- cc[-1,]
#   return(cc)
# }

#aroon
Mkt_ar <- read.csv("../Data/Dax_2000_d.csv")
ar <- aroon(Mkt_ar[c(3,4)], n=20)
ar <- aroon(Mkt_ar[c(5)], n=20)
tail(ar,n=20)

Mkt_ar2 <- cbind(Mkt_ar,ar)
tail(Mkt_ar2,n=20)

nrow(Mkt_ar2)
aup <- Mkt_ar2[Mkt_ar2$aroonUp > 0.65 & Mkt_ar2$aroonDn < 0.4,]
nrow(aup)
adn <- Mkt_ar2[Mkt_ar2$aroonDn > 0.65 & Mkt_ar2$aroonUp < 0.35,]
nrow(adn)



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