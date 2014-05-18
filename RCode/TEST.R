
library(TTR)
library(xtable)
library(candlesticks)
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\MACD_fnc.R")
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\MACD_over_bought.R")
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\Utils.R")

ali <- c('l','l')
ali <- c(ali, rep('c',10))
ali

Dax <- as.data.frame(getYahooData("^GDAXI", format(Sys.Date()-50, format="%Y%m%d"), format(Sys.Date(), format="%Y%m%d")),stringsAsFactors = FALSE)
Dax_xts <- getYahooData("^GDAXI", format(Sys.Date()-50, format="%Y%m%d"), format(Sys.Date(), format="%Y%m%d"))

atr <- ATR(Dax[,c("High","Low","Close")], n=14)
atr <- ATR(Dax_xts[,c("High","Low","Close")], n=14)
tail(atr)

dd <- as.data.frame(Dax_xts)
tail(dd)
dd$Date <- rownames(dd)

class(Dax)
Dax$Date <- rownames(Dax)
tail(Dax)
str(Dax)
Dax$Date <- as.POSIXct(Dax$Date,format='%Y-%m-%d')
Dax <- Dax[,c(6,1,2,3,4)]
atr <- ATR(Dax[,c("High","Low","Close")], n=14)
atr <- ATR(Dax[,c(2,3,4)], n=12)

Dax2 <- getYahooData("^GDAXI", format(Sys.Date()-50, format="%Y%m%d"), format(Sys.Date(), format="%Y%m%d"))
class(Dax2)



tail(Dax)
Dax <- Dax[,c(6,1,2,3,4)]
#atr <- ATR(Mkt[,c("High","Low","Close")], n=14)
atr <- ATR(Dax[,c(2,3,4)], n=14)

class(atr)
Mkt <- cbind(Mkt,atr)
tail(Mkt)
class(Mkt)
#------------------------------------------------

Mkt = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
Mkt = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/F100_2000_d.csv",stringsAsFactors = FALSE)
Mkt = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/CAC_2000_d.csv",stringsAsFactors = FALSE)

sma.value <- SMA(Mkt["Close"], 5)
atr <- ATR(Mkt[,c("High","Low","Close")], n=14)
class(atr)
Mkt <- cbind(Mkt,atr)

Mkt <- cbind(Mkt, sma.value)
Mkt$MAD <- Mkt$Close - Mkt$sma.value
Mkt$PosMAD <- ifelse(Mkt$MAD > 0, Mkt$MAD, NA)
round(quantile(Mkt$PosMAD, na.rm=T))
mean(Mkt$PosMAD,na.rm=T)
tail(Mkt)
tail(atr)
range(atr,na.rm=T)

# -----------------------------------------------
sma_fun <- function(Mkt,lim){
  sma.value <- SMA(Mkt["Close"], 10)  #create sma vector
  Mkt <- cbind(Mkt, sma.value)        #add sma vector as new col
  Mkt$ma.diff <- Mkt$Close - Mkt$sma.value
  Mkt$OH <- Mkt$High - Mkt$Open
  res <- ifelse(Mkt$ma.diff>0,ifelse(Mkt$OH > lim, Mkt$Close - (Mkt$Open + lim), NA),NA)
}


Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
FTSE = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/F100_2000_d.csv",stringsAsFactors = FALSE)
CAC = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/CAC_2000_d.csv",stringsAsFactors = FALSE)


revDF <- Dax[,c(1,2)]
dx <- sma_fun(Dax,50)

revFT <- FTSE[,c(1,2)]
ft <- sma_fun(FTSE, 40)

revCAC <- CAC[,c(1,2)]
cc <- sma_fun(CAC, 30)

revDF <- cbind(revDF, dx)
revFT <- cbind(revFT, ft)
revCAC <- cbind(revCAC, cc)

res <- merge(revDF[c(1,3)],revFT[c(1,3)], by="Date")
res <- merge(res,revCAC[c(1,3)], by="Date")
res
tail(res)
sum(res$dx+res$ft+res$cc,na.rm=T)
res$sm <- res$dx + res$ft + res$cc
range(res$sm,na.rm=T)

tail(revDF)
names(revDF) <- c('Date', 'Open', 'Dax')

sma.value <- SMA(Mkt["Close"], 10)  #create sma vector
Mkt <- cbind(Mkt, sma.value)        #add sma vector as new col
Mkt$ma.diff <- Mkt$Close - Mkt$sma.value
Mkt$OH <- Mkt$High - Mkt$Open
Mkt$DxRes <- ifelse(Mkt$ma.diff>0,ifelse(Mkt$OH > 50, Mkt$Close - (Mkt$Open + 50), NA),NA)
sum(Mkt$DxRes, na.rm=T)
a
b <- na.omit(a)
b <- a[complete.cases(a)]
b

#Dax$prevpl <- c( NA, Dax$ma.diff[ - length(Dax$ma.diff) ] )
#Dax$prevprevMAdiff <- c( NA, Dax$prevMAdiff[ - length(Dax$prevMAdiff) ] )

a <- ifelse(Dax$prevprevMAdiff > 0,ifelse((Dax$Close - Dax$Open) < 0, Dax$Close - Dax$Open,NA),NA)

sum(a, na.rm=T)

Dax$res <- ifelse(Dax$prevprevMAdiff > 0, ifelse()


tail(Dax)


write.csv(Dax, "data\\ma_test.csv")



# --------------

#madc
macd <- MACD( Dax[,"Open"], 12, 26, 9, maType="EMA" )
Dax <- cbind(Dax, macd)
tail(Dax, n=10)
head(Dax)

tail(FT)
FT <- FT[,c(1:5)]

write.csv(Dax, "data\\macd.csv")

a <- MACD_fnc(Dax, 0, "Dax")

b <- MACD_fnc(FT, 0, "Dax")

c <- MACD_OB(Dax, 0, "Dax")

#--  MACD overbought
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\MACD_OB.R")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
macd <- MACD( Dax[,"Open"], 12, 26, 9, maType="EMA" )
Dax <- cbind(Dax, macd)
quantile(Dax$macd, na.rm=T)
lw <- quantile(Dax$macd, na.rm=T, probs=0.15);lw
up <- quantile(Dax$macd, na.rm=T, probs=0.85);up
#tail(Dax)
macd_ob <- MACD_OB(Dax, 0 , "Dax", lw, up);macd_ob
tail(Dax)

# -----------------------------

fil <- c("../Data/Dax_2000_d.csv",
         "../Data/CAC_2000_d.csv", 
         "../Data/F100_2000_d.csv",
         "../Data/Dow_2000_d.csv",
         "../Data/N225_2000_d.csv",
         "../Data/Oz_2000.csv")
nm <- c("Dax", "CAC", "F100", "Dow", "Nik", "Oz")

#ln <- nrow(existingDF)
for(i in 1:length(fil)){
  browser()
  Dax <- read.csv(fil[i])                             #read data 
  ma <- MACD( Dax[,"Open"], 12, 26, 9, maType="EMA" ) #calc MACD values
  Dax <- cbind(Dax, ma)                               #Add MACD values to orig data set
  lw <- quantile(Dax$macd, na.rm=T, probs=0.15)       #Calc low val for algo
  up <- quantile(Dax$macd, na.rm=T, probs=0.85)       #Calc up val for algo
  a <- MACD_OB(Dax, 0, nm[i], lw, up)                 #Call fnc
  existingDF <- rbind(existingDF, a)                  #add results
}
df.name <- names(a)
names(existingDF) <- df.name
existingDF <- existingDF[-c(1:ln),]


#--  SMA
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\SMA_sys.R")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
a <- BaseSystem1SMA(Dax, 5, -100, "Dax");a
b <- BaseSystem1SMA(Dax, 5, -50, "Dax");b

# ----------  Oscillators

# 1 SAR
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\SAR.R")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
sar <- SAR(Dax[c(3,4)]) #HL
Dax <- cbind(Dax,sar)
tail(Dax)
write.csv(Dax,"data\\sar.csv")
sr <- sar_sys(Dax,0,"Dax")

#2. Stoch
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\Stoch.R")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
st <- stoch(Dax[c(3,4,5)]) #HL
Dax <- cbind(Dax,st)

x <- stoch_sys(Dax,0,"Dax")

tail(Dax)
tail(st)
range(Dax$fastD, na.rm=T)
range(Dax$slowD, na.rm=T)

#3. ROC
library(TTR)
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\ROC2.R")
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\Utils.R")

Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
roc <- ROC( Dax$Close )
Dax <- cbind(Dax, roc)


as <- lags(Dax$roc,n=2)
pre_df <- as.data.frame.matrix(as)
names(pre_df) <- c("roc","preC1","preC2")
Dax <- merge(Dax,pre_df,by='roc')
Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
ndx <- order(Dax$Date)
Dax <- Dax[ndx,]
Dax <- Dax[c(2,1,3,4,5,6,7,8)]
Dax$preC1 <- round(Dax$preC1 * 100,2)
Dax$preC2 <- round(Dax$preC2 * 100,2)

rs <- roc_sys(Dax, 0, "Dax")
rs

Dax$ROC <- momentum(Dax$Close, n=12, na.pad = T)
quantile(Dax$ROC, na.rm=T, probs=0.25)
quantile(Dax$ROC, na.rm=T, probs=0.75)
tail(Dax)

tail(Dax)
head(Dax)

at <- ATR(Dax[,c("High","Low","Close")], n=14)
Dax<- cbind(Dax, at)
Dax <- Dax[,c(1,2,3,4,5,7)]

# --------------------------------
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
Dax$tt <- ifelse(Dax$Open<Dax$Close, #1
                  Dax$Close[-3]
                 ,0) #1

tail(Dax,n=15)
aa <- c(rep(NA,2),head(Dax$Close,-2))
head(aa)
head(Dax)

# -----------------------------------------
# Candlestick /data
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
#add ATR
at <- ATR(Dax[,c("High","Low","Close")], n=14)
Dax<- cbind(Dax, at)
Dax <- Dax[,c(1,2,3,4,5,7)]
#Add Aroon
ar <- aroon(Dax[c(3,4)], n=20)
Dax <- cbind(Dax, ar)
# ROC momentum
Dax$ROC <- momentum(Dax$Close, n=12, na.pad = T)
#MACD
macd <- MACD( Dax[,"Open"], 12, 26, 9, maType="EMA" )
Dax <- cbind(Dax, macd)
#SMA
Dax$SMA10 <- SMA(Dax["Close"], 10)  #create sma vector
Dax$SMADiff <- Dax$Close - Dax$SMA10

#- find Doji
Dax$Doji <- ifelse((abs(Dax$Close-Dax$Open)/Dax$atr)*100 < 10, 'Doji', NA)

#- find Hammer
Dax$Hammer <- ifelse( (Dax$Close-Dax$Low) / (Dax$High-Dax$Low) * 100 > 90, 
                     ifelse( , , ) , 
                     )

Dax$pos <- abs((Dax$Close-Dax$Open)/Dax$atr)*100
Dax$bd_size <- (abs(Dax$Close-Dax$Open) / Dax$atr) * 100
Dax$shadow <- ((Dax$High-Dax$Low) / Dax$atr) * 100

for(i in 1:nrow(Dax)){
  if(!is.na(Dax$pos[i])){
    if(Dax$pos[i] > 90 ){
      if(Dax$bd_size[i] > 5){
        if(!is.na(Dax$shadow[i]) > 40){
          if(Dax$pos[i] > 90){
            if(Dax$Close[i]-Dax$Open > 0){
              Dax$Hammer[i] <- 'Hammer'
            } else {
              Dax$Hammer[i] <- 'Hanging Man'
            }
          }
          if(Dax$pos[i] < 10){
            if(Dax$Close[i]-Dax$Open > 0){
              Dax$Hammer[i] <- 'Inverted Hammer'
            } else {
              Dax$Hammer[i] <- 'Shooting Star'
            }
          }
        }
      }
    }
  }
}

tail(Dax)

if(Dax$pos[i] > 90){Dax$Hammer[i] <- 'Ham'}

for(i in 1:nrow(Dax)){
  Dax$pos[i] <- abs((Dax$Close[i]-Dax$Open[i])/Dax$atr[i])*100
  Dax$bd_size[i] <- (abs(Dax$Close[i]-Dax$Open[i]) / Dax$atr[i]) * 100
  #shadow[i] <- ((Dax$High[i]-Dax$Low[i]) / Dax$atr[i]) * 100
  if(Dax$pos[i] > 90){Dax$Hammer[i] <- pos}
}

(Abs(Cells(row, 2) - Cells(row, 5)) / Cells(row, 6)) * 100
shadow = (HL / Cells(row, 6) * 100)




tail(Dax)
head(at)
write.csv(Dax,"data\\candlestick_dax.csv")

head(ndx)

dt <- sort(Dax$Date)

class(Dax2)

tail(pre_df)
tail(Dax)
head(Dax)
head(Dax2)

mm <- as.matrix(as, nrow=3570, ncol=3)
as <- c( NA,  lags(Dax$Close,n=2))
tail(as)

# -------------------------------- Candlestick - HAMMER

Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/candlestick_dax.csv",stringsAsFactors = FALSE)


Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\Candle_Hammer.R")
a <- cand_ham_aroon(Dax,0,"Dax");a


install.packages("candlesticks", repos="http://R-Forge.R-project.org")

library(candlesticks)

## Not run: 
getSymbols('YHOO',adjust=TRUE)
DaxA <- getYahooData("^GDAXI", format(Sys.Date()-40, format="%Y%m%d"))
DaxA$Hammer <- CSPHammer(DaxA)

tail(YHOO)
tail(Dax)
tail(DaxA)

# filter for hammer patterns
CSPHammer(YHOO)
DaxA$Hammer <- CSPHammer(DaxA)
DaxA$InvertedHammer <-CSPInvertedHammer(DaxA)
rs <- candle_hammer(DaxA,0,"Dax");rs

aa
tail(aa)
tail(DaxA,n=30)

CSPHammer
Hammer

library(xts)
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\Candle_Hammer.R")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
#rs <- candle_hammer(Dax,0,"Dax");rs
Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
Dax_xts <- xts(Dax[,c(2,3,4,5)],Dax$Date)
hh <- CSPHammer(Dax_xts)
hi <- CSPInvertedHammer(Dax_xts)
Dax_xts <- cbind(Dax_xts,hh)
Dax_xts <- cbind(Dax_xts,hi)

ar <- aroon(Dax_xts$Close,n=20)
Dax_xts <- cbind(Dax_xts,ar)

----- Sweave ----------------
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\Candle_Hammer_aroon.R")
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\Candle_Engulf_aroon.R")
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\Candle_Doji_aroon.R")

Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
Dax_xts <- xts(Dax[,c(2,3,4,5)],Dax$Date)
#hh <- as.data.frame(CSPHammer(Dax_xts))
#hi <- as.data.frame(CSPInvertedHammer(Dax_xts))

#en <- as.data.frame(CSPEngulfing(Dax_xts))

dj <- as.data.frame(CSPDoji(Dax_xts))
tail(dj)
sum(is.na(dj$Doji))
sum(dj$Doji)
sum(dj$DragonflyDoji)
sum(dj$GravestoneDoji)
class(dj)
nrow(dj)

#Dax <- cbind(Dax,hh)
#Dax <- cbind(Dax,hi)
Dax <- cbind(Dax,dj)

ar <- aroon(Dax$Close,n=20)
Dax <- cbind(Dax,ar)

a <- candle_doji_aroon(Dax,0, "Dax")
a

a <- candle_engulf_aroon(Dax,0, "Dax")
a

sum(Dax$Doji)
sum(Dax$DragonflyDoji)
sum(Dax$GravestoneDoji)

sum(Dax$Bull.Engulfing)
sum(Dax$Bear.Engulfing,na.rm=T)


tail(Dax)
Dax$HH <- ifelse(Dax$Hammer==T,ifelse(Dax$Close > Dax$Open, Dax$HH <- TRUE, Dax$HH <- FALSE), Dax$HH <- FALSE)
tail(Dax[Dax$Hammer == TRUE,c(1,2,5,6,11)])

nrow(Dax[Dax$Hammer == TRUE & (Dax$Close > Dax$Open),c(1,2,5,6,11)])
nrow(Dax[Dax$Hammer == TRUE & (Dax$Close < Dax$Open),c(1,2,5,6,11)])

nrow(Dax[Dax$InvertedHammer == TRUE & (Dax$Close > Dax$Open),c(1,2,5,6,11)])
nrow(Dax[Dax$InvertedHammer == TRUE & (Dax$Close < Dax$Open),c(1,2,5,6,11)])


a <- cand_ham_aroon(Dax,0, "Dax")
a

ham <- Dax[Dax$InvertedHammer == T,]
sum(ham$InvertedHammer)

ham_ardn <- ham[ham$aroonDn > 65 & !is.na(ham$aroonDn),]
sum(ham_ardn$Hammer)
ham_ardn[c(1,6,8,9)]

tail(Dax)

zz <- Dax_xts[Dax_xts$Hammer == T]
tail(zz)

zzh <- zz[zz$Close > zz$Open,]
tail(zzh)
sum(zzh$Hammer)

zzhDn <- zzh[zzh$aroonDn >= 70,]
tail(zzhDn)
sum(zzhDn$Hammer)

tail(Dax_xts)
tail(hh)
sum(Dax_xts$Hammer)
nrow(Dax_xts)
nrow(hh)

sum(is.na(hi))

rs <- candle_hammer(Dax,0,"Dax");rs

# ---
library(xts)
library(quantmod)
library(TTR)

#quantmod
getSymbols('^GDAXI',src='yahoo')
getSymbols('^GDAXI',src='yahoo',from = "2014-01-01", to = Sys.Date())
getSymbols('^GDAXI',src='yahoo',from = Sys.Date() - 50, to = Sys.Date())
tail(GDAXI)

#TTR
ibm <- getYahooData("IBM", 1999044, 20050607)
nyse.symbols <- stockSymbols("NYSE")

tail(nyse.symbols)

#???
Dax <- getYahooData("^GDAXI", format(Sys.Date()-50, format="%Y%m%d"), format(Sys.Date(), format="%Y%m%d"))



getSymbols('^GDAXI',src='yahoo',from = Sys.Date() - 50, to = Sys.Date())
Dax <- GDAXI
Dax$Date <- rownames(GDAXI)
Dax <- Dax[,c(6,1,2,3,4)]
names(Dax) <- c("Date", "Open","High","Low","Close")
tail(Dax)
str(Dax)

Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')

ar <- aroon(Dax$Close, n=20)
Dax <- cbind(Dax, ar)


Dax <- as.data.frame(getYahooData("^GDAXI", format(Sys.Date()-50, format="%Y%m%d"), format(Sys.Date(), format="%Y%m%d")))

Dax <- getYahooData("^GDAXI", format(Sys.Date()-50, format="%Y%m%d"), format(Sys.Date(), format="%Y%m%d"))

Dax$Date <- rownames(Dax)
Dax <- Dax[,c(6,1,2,3,4)]
tail(Dax)
str(Dax)
Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
Dax_xts <- xts(Dax[,c(2,3,4,5)],Dax$Date)
hh <- as.data.frame(CSPHammer(Dax))
hi <- as.data.frame(CSPInvertedHammer(Dax))
Dax <- cbind(Dax,hh)
Dax <- cbind(Dax,hi)

Dax_x <- getYahooData("^GDAXI", format(Sys.Date()-50, format="%Y%m%d"), format(Sys.Date(), format="%Y%m%d"))
str(Dax_x)
Dax_x$Date <- rownames(Dax_x)
tail(Dax_x)
hh <- CSPHammer(Dax_x)
Dax_x <- cbind(Dax_x,hh)




# ------
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
Dax_xts <- xts(Dax[,c(2,3,4,5)],Dax$Date)
hh <- CSPEngulfing(Dax_xts)
Dax_xts <- cbind(Dax_xts,hh)
sum(Dax_xts$Bull.Engulfing)

CSPHammer(YHOO)
aa <- CSPEngulfing(YHOO)
tail(aa)

tail(Dax)
tail(Dax_xts)
df <- as.data.frame(Dax_xts)
tail(df)
str(df)
rownames(df)
df$Date <- rownames(df)
tail(hh)
length(hh[hh$Hammer==TRUE])

# filter for hammer patterns that occur in downtrends
CSPHammer(YHOO) & TrendDetectionChannel(YHOO)[,"DownTrend"]
CSPHammer(YHOO, minlowershadowCL=1/5, maxuppershadowCL=0.1, minbodyCL=0.01)
CSPHammer(YHOO)

tail(adf)
names(adf) <- c("orig","preCl1","preCl2")



Dax <- cbind(Dax,adf[,c(2,3)])
Dax <- cbind(Dax,as)

dd <- merge(Dax, as)
             
tail(Dax)

roc <- ROC(Dax$Close,n=12) #HL

#rc <- ROC(Dax$Close,n=12) #HL
roc <- round(roc * 100, 2)

Dax <- cbind(Dax,roc)
lw <- quantile(Dax$roc, na.rm=T, probs=0.25) 
up <- quantile(Dax$roc, na.rm=T, probs=0.75)

Dax <- cbind(Dax,roc)

Dax$Roc100 <- round(Dax$roc * 100,2)

lw <- quantile(Dax$Roc100, na.rm=T, probs=0.25)       #Calc low val for algo
up <- quantile(Dax$Roc100, na.rm=T, probs=0.75)       #Calc up val for algo
a <- roc_sys(Dax, 0, "Dax", lw, up)
a
class(a)

tail(Dax)
tail(roc)
range(Dax$roc,na.rm=T)
Dax$roc <- Dax$roc * 1000

write.csv(Dax,"data\\roc_dax.csv")

x <- roc_sys(Dax,0,"Dax")
quantile(Dax$roc,na.rm=T)

today <- Sys.Date()
today <- format(Sys.Date()-30, format="%Y%m%d")

ibm <- getYahooData("^GDAXI", 20140404, today)
ibm <- getYahooData("^GDAXI", format(Sys.Date()-40, format="%Y%m%d"), format(Sys.Date(), format="%Y%m%d"))
ibm[,2]

nrow(ibm)

vv <- as.data.frame(ibm)

class(index(ibm))
index(ibm)
as.character.Date(index(ibm))
aa <- as.character.Date(index(ibm))
length(aa)
vv <- cbind(vv,aa)
ibm$Date <- as.Date(index(ibm),format="%d%m")
tail(ibm)
dimnames(ibm)
rownames(ibm)
substr("abcdefg", 2, 4)
ibm$Date2 <- substr(index(ibm), 1, 10)

# ---- ADX
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\aroon.R")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
adx <- ADX(Dax[c(3,4,5)])
tail(adx)
class(adx)
range(adx$ADX)
Dax <- cbind(Dax,adx)
tail(Dax)
range(Dax$ADX,na.rm=T)
quantile(Dax$ADX,na.rm=T)

# ------ Aroon
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\aroon.R")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dow_2000_d.csv",stringsAsFactors = FALSE)
ar <- aroon(Dax[c(3,4)], n=20)
Dax <- cbind(Dax, ar)
tail(Dax)
write.csv(Dax,"data\\ar.csv")
aaa <- aroon_sys(Dax, -50, "Dax")
aaa
getwd()

# ------------- BBands
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
head(Dax)
Daxbb <- Dax[,c(3,4,5)]
head(Daxbb)
bb <- BBands(Daxbb, n=20)
tail(bb)
Dax <- cbind(Dax,bb)
tail(Dax)

# ------------------------- lag
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
head(Dax)
nrow(Dax)
lg <- lags(Dax$Close,n=1)
head(lg)
class(lg)
nrow(lg)
tail(Dax);tail(lg)

# ------- RunSum
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
head(Dax)

Dax$rsC <- runSum(Dax$Close, n=5)
Dax$rsH <- runSum(Dax$High, n=5)
Dax$rsPer <- round((Dax$rsC / Dax$rsH)*100,2)
tail(rs, n=10)
tail(Dax$Close)
tail(Dax)
sum(Dax$Close[3570:3566])
range(na.omit(Dax$rsPer))

#------------------------------------------
setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")

source("../../RCode//Utils.R")
source("../../RCode//BaseSystem1SMA.R")
source("../../RCode//BaseSystem2Bout.R")
source("../../RCode//BaseSystem3Quant90.R")
source("../RCode//NaiveLongSystem2.R")
source("../../RCode//NaiveFollowPrev.R")
source("../../RCode//MACD_XO.R")

fil <- c("../Data/Dax_2000_d.csv",
         "../Data/CAC_2000_d.csv", 
         "../Data/F100_2000_d.csv",
         "../Data/Dow_2000_d.csv",
         "../Data/N225_2000_d.csv",
         "../Data/Oz_2000.csv")
nm <- c("Dax", "CAC", "F100", "Dow", "Nik", "Oz")

getwd()
Dax <- read.csv(fil[3])
head(Dax)
tail(Dax)

existingDF <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])
  a <- NaiveLongSystem2(Dax, 0, nm[i])
  df.name <- names(a)
  names(existingDF) <- df.name
  existingDF <- rbind(existingDF, a)
}

print(nm)
for(i in 1:length(fil)){
  print(summary(Dax))
}

range(Dax)

# ---------------
source("../../RCode//Utils.R")
source("../../RCode//SMA_sys.R")
source("../../RCode//MACD_XO.R")
#source("../../RCode//MACD_OB.R")
source("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\RCode\\Aroon.R")

  fil <- c("D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\Data\\Dax_2000_d.csv",
           "D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\Data\\CAC_2000_d.csv",
           "D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\Data\\F100_2000_d.csv",
           "D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\Data\\Dow_2000_d.csv",
           "D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\Data\\N225_2000_d.csv",
           "D:\\Allan\\DropBox\\MSc\\Dissertation\\Thesis\\Data\\Oz_2000.csv")
nm <- c("Dax", "CAC", "F100", "Dow", "Nik", "Oz")

df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
for(i in 1:length(fil)){
  Dax <- read.csv(fil[i])                        #read data 
  ar <- aroon(Dax[c(3,4)], n=20)                 #calc Aroon values
  Dax <- cbind(Dax, ar)                           #Add Aroon values to orig data set
  a <- aroon_sys(Dax, 0, nm[i])                  #Call fnc
  df10 <- rbind(df10, a)             #add results
}

df10[2,1]

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


aroondfsldf <- as.data.frame(matrix(seq(3),nrow=1,ncol=3))
ln <- nrow(aroondfsl)
res <- 1:3
for(i in 2:ln){
  #browser()
  res[1] <- aroondfsl[i,1]
  res[2] <- as.numeric(aroondfsl[i,3]) - as.numeric(df10[i,3])
  res[3] <- as.numeric(aroondfsl[i,4]) - as.numeric(df10[i,4])
  aroondfsldf <- rbind(aroondfsldf,res)
}
aroondfsldf[5,3]

# --------  DAX Report

Dax_xts <- getYahooData("^GDAXI", format(Sys.Date()-50, format="%Y%m%d"), format(Sys.Date(), format="%Y%m%d"))
Dax_xts <- Dax_xts[,c(1,2,3,4)]
hh <- CSPHammer(Dax_xts)
hi <- CSPInvertedHammer(Dax_xts)
Dax_xts <- cbind(Dax_xts,hh)
Dax_xts <- cbind(Dax_xts,hi)
Dax <- as.data.frame(Dax_xts)
Dax$Date <- as.character.Date(index(Dax_xts),format="%d %b")
Dax <- Dax[,c(7,1,2,3,4,5,6)]
tail(Dax_xts)
tail(Dax)

#Dax_xts <- xts(Dax[,c(2,3,4,5)],Dax$Date)
hh <- as.data.frame(CSPHammer(Dax_xts))
hi <- as.data.frame(CSPInvertedHammer(Dax_xts))
Dax <- cbind(Dax,hh)
Dax <- cbind(Dax,hi)
Dax$Date <- as.character.Date(index(Dax_xts),format="%d %b")

ar2 <- aroon(DaxA$Close,n=20)
DaxA <- cbind(DaxA,ar2)

Dax <- as.data.frame(DaxA)
Date <- as.character.Date(index(DaxA),format="%d %b")
Dax <- cbind(Dax,Date)
Dax <- Dax[,c(6,1,2,3,4)]
ar <- aroon(Dax$Close, n=20)
Dax <- cbind(Dax, ar)

tail(DaxA)

#-------------------------------------
setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")
library(xtable)
library(TTR)
library(candlesticks)
library(xts)
source("Utils.R")
source("RCode//Candle_Hammer.R")
source("Candle_Hammer_aroon.R")
source("Candle_Engulf_aroon.R")
source("Candle_Doji_aroon.R")

  fil <- c("../Data//Dax_2000_d.csv",
           "../Data//CAC_2000_d.csv", 
           "../Data//F100_2000_d.csv",
           "../Data//Dow_2000_d.csv",
           "../Data//N225_2000_d.csv",
           "../Data//Oz_2000.csv")
nm <- c("Dax", "CAC", "F100", "Dow", "Nik", "Oz")

fil <- c("../Data//CAC_2000_d.csv", 
         "../Data//F100_2000_d.csv",
         "../Data//Dow_2000_d.csv",
         "../Data//N225_2000_d.csv",
         "../Data//Oz_2000.csv")
nm <- c("Dax", "CAC", "F100", "Dow", "Nik", "Oz")
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))

Dax <- read.csv("../Data//Dax_2000_d.csv",stringsAsFactors = FALSE)

pub <- function(x){
  #browser()
  ln <- nrow(df10)
  for(i in 1:length(fil)){
  Dax <- read.csv(fil[i],stringsAsFactors = FALSE)
  #Dax <- read.csv(fil[i],,stringsAsFactors = FALSE)
  #Dax <- read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
  Dax <- Dax[,c(1,2,3,4,5)]
  Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
  Dax_xts <- xts(Dax[,c(2,3,4,5)],Dax$Date)
  en <- as.data.frame(CSPEngulfing(Dax_xts))
  Dax <- cbind(Dax,en)
  ar <- aroon(Dax$Close,n=20)
  Dax <- cbind(Dax,ar)
  a <- candle_engulf_aroon(Dax,0, "Dax")
  df10 <- rbind(df10, a)
  }
  return (df10)
}

pub(3)

mk <- read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/F100_2000_d.csv",stringsAsFactors = FALSE)
tail(mk)
aa <- aroon(mk$Close,n=20)
tail(aa)
mk <- cbind(mk,aa)
tail(mk)

