# 1. Exp Smoothing
setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")
setwd("F:/Allan/R Stuff/ES")

library(forecast)


# Mkt <- read.csv("../Data/Dax_2000_d.csv")
# nrow(Mkt)
# Mkt$Date[2999]
# Mkt_ts <- ts(Mkt$Close)
# #Mkt_ts <- ts(Mkt$Close,frequency=252, start=c(2000,1))
# #Mkt_train <- window(Mkt_ts, start=2000, end=2009.99)
# Mkt_train <- window(Mkt_ts, end=2999.99)
# Mkt_test <- window(Mkt_ts, start=3000)
# 

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


# 2 - Dow
dw <- read.csv("Dow_2000_d.csv")
dw_ts <- ts(dw$Close)
as <- exp_sm(dw_ts,dw, 400)
write.csv(as,'dow_es.csv')

# 2 - FTSE
ft <- read.csv("F100_2000_d.csv")
ft_ts <- ts(ft$Close)
asft <- exp_sm(ft_ts,ft, 400)
tail(asft)
write.csv(asft,'ft_es.csv')

# 2 - CAC
ft <- read.csv("cac_2000.csv")
ft_ts <- ts(ft$Close)
tail(ft_ts)
asft <- exp_sm(ft_ts,ft, 400)
tail(asft)
write.csv(asft,'cac_es.csv',row.names=FALSE)

# 2 - Nik
ft <- read.csv("N225_2000.csv")
ft_ts <- ts(ft$Close)
tail(ft_ts)
asft <- exp_sm(ft_ts,ft, 400)
tail(asft)
write.csv(asft,'nik_es.csv')

# 2 - Oz
ft <- read.csv("Oz_2000.csv")
ft_ts <- ts(ft$Close)
asft <- exp_sm(ft_ts,ft, 400)
write.csv(asft,'oz_es.csv')


# --- Trading syst
source("es_1.R")
source("Utils.R")

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

res <- es_1(es2,0,"Dax")
res

# -------------------------------------------------------
# base sys
setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")
dw <- read.csv("../Data/Dax_2000_d.csv")
dw_ts <- ts(dw$Close)

mean_model <- meanf(dw_ts, h=5)
res <- exp_mean(dw_ts,dw,400)
tail(res,n=20)
write.csv(res,'dow_mean.csv',row.names=FALSE)

exp_mean <- function(Mkt_ts, Mkt, strt){
  #browser()
  Mkta <- Mkt
  cc <- Mkta[1,]
  cc$a <- 0
  ln <- nrow(Mkt)
  for(i in strt:ln){
    st <- i-30
    Mkt_slice <- window(Mkt_ts,start=st,end=i)
    modf <- meanf(Mkt_slice,h=1)
    a <- as.numeric(modf$mean)
    c1 <- Mkta[i,]
    ab <- cbind(c1,a)
    cc <- rbind(cc,ab)
  }
  cc <- cc[-1,]
  return(cc)
}


# ---
# naive


naive_model <- naive(dw_ts, h=1)
naive_model$mean
tail(dw_ts)


# --- drift
setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")
dw <- read.csv("../Data/Dax_2000_d.csv")
dw_ts <- ts(dw$Close)

drift_model <- rwf(dw_ts,drift=TRUE,h=1)

res2 <- exp_drift(dw_ts,dw,400)
tail(res2)

exp_drift <- function(Mkt_ts, Mkt, strt){
  #browser()
  Mkta <- Mkt
  cc <- Mkta[1,]
  cc$a <- 0
  ln <- nrow(Mkt)
  for(i in strt:ln){
    st <- i-30
    Mkt_slice <- window(Mkt_ts,start=st,end=i)
    modf <- rwf(Mkt_slice,drift=TRUE,h=1)
    #fcastf <- forecast.ets(modf,h=1)
    a <- as.numeric(modf$mean)
    c1 <- Mkta[i,]
    ab <- cbind(c1,a)
    cc <- rbind(cc,ab)
  }
  cc <- cc[-1,]
  return(cc)
}



# --------------
# chp 3 - trend duration / %
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