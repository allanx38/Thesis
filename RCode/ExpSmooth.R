# 1. Exp Smoothing

Mkt <- read.csv("../Data/Dax_2000_d.csv")
tail(Mkt)
Mkt_ts <- ts(Mkt$Close)
mod <- ets(Mkt_ts)
mod
mod$fitted
fcast <- forecast.ets(mod, h=5)
fcast

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
# 
# Mkt <- read.csv("../Data/Dax_2000_d.csv")
# Mkt_ts <- ts(Mkt$Close)
# res <- exp_sm(Mkt_ts, Mkt, 3500)
# write.csv(res,'../Data/Dax_ets_aan_300.csv')