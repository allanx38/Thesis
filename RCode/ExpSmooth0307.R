# 1. Exp Smoothing
setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")
library(forecast)

exp_sm <- function(Mkt_ts, Mkt, strt){
  #browser()
  Mkta <- Mkt
  cc <- Mkta[1,]
  cc$a <- 0
  cc$b <- 0
  ln <- nrow(Mkt)
  #lb <- 300 #lookback
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

Mkt <- read.csv("../Data/Dax_2000_d.csv")
tail(Mkt)
Mkt_ts <- ts(Mkt$Close)
Mkt_train <- window(Mkt_ts, start=2000, end=2030) 
mod <- ets(Mkt_ts)
mm <- mod$method
class(mm)
mod$fitted
mod$initstate
fcast <- forecast.ets(mod, h=1)
m <- as.numeric(fcast$mean)
m
class(m)

as <- exp_sm(Mkt_ts,Mkt, 3000)
as1 <- exp_sm(Mkt_ts,Mkt, 400)
tail(as1)
write.csv(as1,'../Data/ES/dax_es.csv')

es <- read.csv("../Data/ES/dax_es.csv",stringsAsFactors=F)
tail(es,n=5)
table(es$b)

# 1. Add U/D
es$pred_d <- ifelse(es$a > es$Close, 'U','D')

es$pm <- c( NA, es$b[ - length(es$b) ] )
es$pp <- c( NA, es$a[ - length(es$a) ] )
es$pu <-c( NA, es$pred_d[ - length(es$pred_d) ] )

es$b[100]
es2 <- es[es$pp != 'ETS(A,N,N)', ]
tail(es2)
nrow(es2)

source("../RCode/es_1.R")
source("../RCode/Utils.R")

res <- es_1(es2,0,"Dax")
res

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