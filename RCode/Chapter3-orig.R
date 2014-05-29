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
dat <- tail(Dax)
dig <- 0
cap <- c("First 6 rows of the Dax data set","First 6 rows of the Dax data set.")
lab = 'tab:daxhead'
filname ='../Tables/chp_3_1_daxhead.tex'
inclrnam=F
print_xt(dat,dig,cap,lab,al,filname,inclrnam)


Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)


xtable(tail(Dax), digits = 2, 
       caption = c('Naive Long System. Every day the algorithm buys the market.',
                   'Naive Long System'))

# ----------- Open Stats ----------------------
Dax <- Dax[,-c(6,7)]
Dax$prevHigh <- c( NA, Dax$High[ - length(Dax$High) ] )
Dax$prevLow <- c( NA, Dax$Low[ - length(Dax$Low) ] )
c <- ATR(Dax[,c("High","Low","Close")], n=14)
Dax$atr <- c[,"atr"]
Dax$atr_per <- round((Dax$atr /  Dax$Close)*100,2)

Dax$O_pHL <- ifelse(Dax$Open>Dax$prevLow, ifelse(Dax$Open<Dax$prevHigh,1,0),0)

(sum(Dax$O_pHL,na.rm=T)/length(Dax$O_pHL))*100

plot(Dax$Date,Dax$atr_per,type='l')

dt <- as.POSIXct(Dax$Date,format = "%d/%m/%Y")
Dax_xts <- xts(Dax$atr_per, dt)
chartSeries(Dax_xts)
max(Dax$atr_per,na.rm=T)
min(Dax$atr_per,na.rm=T)
mean(Dax$atr_per,na.rm=T)

# ---------------Dax
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
qplot(Dax$Date, Dax$Close, data = Dax, geom='line') + ggtitle("Dax 2000-2013") + ylab('')
ggplot(Dax,aes(x=Date,y=Close)) + geom_line() + ggtitle("Dax Prices between 2000 and 2013") + ylab('Dax Closing Price')
q

ggplot(Dax, aes(Date)) + 
  geom_line(aes(y = Close, colour = "var0")) + 
  geom_line(aes(y = Open, colour = "var1"))

x <- c(3316:3570)
Dx2 <- Dax[,c(1,2,5)]
Dx2 <- Dx2[x,]
tail(Dx2)
ggplot(Dx2,aes(x=Date,y=Close)) + geom_line() + ggtitle("Dax Prices in 2013") + ylab('Dax Closing Price')


# ----- Chp 5
Dow = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/DowDay.csv",stringsAsFactors = FALSE)
ggplot(Dow,aes(x=Time,y=O)) + geom_line() + ggtitle("Dow on 30th March 2007") + ylab('Dow Price')



Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')


# Add 2 lines ------------------
ggplot(Dx2, aes(Date)) + 
  geom_line(aes(y = Close, colour = "Close")) + 
  geom_line(aes(y = Open, colour = "Open"))

# ---------------- ATR
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Oz_2000.csv")
tail(Dax)
str(Dax)
Dax <- Dax[-c(6,7)]
atr <- ATR(Dax[,c('High','Low','Close')],n=14)
str(atr)
Dax$atr <- atr[,2]
Dax$atr_av <- ifelse(Dax$atr>0,round((Dax$atr/Dax$Close)*100),NA)
Dax$atr_av <- round((Dax$atr/Dax$Close)*100,2)

summary(Dax$atr)
Dax2 <- Dax[,c(6,7)]
colnames(Dax2) <- c('ATR','ATR/Close')
stargazer(Dax2)
tail(Dax2)

Dax3 <- Dax[,c(1,6,7)]
tail(Dax3)
Dax3$Date <- as.POSIXct(Dax3$Date3,format='%d/%m/%Y')
ggplot(Dax3,aes(x=Date,y=atr_av)) + geom_line() + ggtitle("ATR / Closing Prices 2000 to 2013") + ylab('ATR / Closing Prices')

# ----------- Close Stats ----------------------
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv",stringsAsFactors = FALSE)
Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')

Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/CAC_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/F100_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dow_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/N225_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Oz_2000.csv")

tail(Dax)
str(Dax)

Dax <- Dax[,-c(6,7)]
Dax$prevHigh <- c( NA, Dax$High[ - length(Dax$High) ] )
Dax$prevLow <- c( NA, Dax$Low[ - length(Dax$Low) ] )

Dax$C_pHL <- ifelse(Dax$Close<Dax$prevLow, 1, ifelse(Dax$Close>Dax$prevHigh,1,0))

(sum(Dax$C_pHL,na.rm=T)/length(Dax$C_pHL))*100

# 1. Close outside prev H/L
round(length(Dax$Close[Dax$Close > Dax$prevHigh | Dax$Close < Dax$prevLow]) / length(Dax$Close) *100)

# 2. Close outside 10 90 stoch
#Dax$Stoch10 <- ifelse(!is.na(Dax$prevHigh), Dax$prevLow + ((Dax$prevHigh - Dax$prevLow)*0.1), NA)
#Dax$Stoch90 <- ifelse(!is.na(Dax$prevHigh), Dax$prevLow + ((Dax$prevHigh - Dax$prevLow)*0.9), NA)
#Dax$OpenStoch <- ifelse(!is.na(Dax$prevHigh), ifelse(Dax$Open[Dax$Open<Dax$prevHigh & Dax$Open>Dax$prevLow], round((Dax$Open - Dax$prevLow) / (Dax$prevHigh - Dax$prevLow),2) * 100, NA), NA)
length(Dax$Close[Dax$Close > (Dax$prevLow + ((Dax$prevHigh - Dax$prevLow)*0.9)) | Dax$Close < (Dax$prevLow + ((Dax$prevHigh - Dax$prevLow)*0.1))]) / length(Dax$Close)

# 3. Close outside 30 70 stoch
length(Dax$Close[Dax$Close > (Dax$prevLow + ((Dax$prevHigh - Dax$prevLow)*0.7)) | Dax$Close < (Dax$prevLow + ((Dax$prevHigh - Dax$prevLow)*0.3))]) / length(Dax$Close) #81%

# O to C levels
tail(Dax)
Dax$OC <- abs(Dax$Open - Dax$Close)
range(Dax$OC)
quantile(Dax$OC) #pick a column ...
qq <- quantile(Dax$OC, probs=0.90)
qq

# -------------------------------- OH OL stats-------------------------------

Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/CAC_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/F100_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dow_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/N225_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Oz_2000.csv")
summary(Dax)
str(Dax)
head(Dax)
Dax$OH <- Dax$High - Dax$Open
Dax$OL <- Dax$Open - Dax$Low
Dax$absOC <- abs(Dax$Open - Dax$Close) # really want absolute value here

names(Dax)
#Dax$mm <- apply(Dax[9],2, function(x) if (Dax$OH>Dax$OL) Dax$OL else Dax$OH )
Dax$mn <- ifelse(Dax$OH>Dax$OL,Dax$OL,Dax$OH)
Dax$mx <- ifelse(Dax$OH>Dax$OL,Dax$OH,Dax$OL)
head(Dax)

#-- min
#summary stats
# 1. min / max together
range(Dax$mn)

# 2. quantiles
quantile(Dax$mn) #pick a column ...
qq <- quantile(Dax$mn, probs=0.90)
# quantile(Dax$mn, probs=c(0.10,0.90))
quantile(Dax$mn, probs=0.90)
quantile(Dax$mn, probs=0.80)
quantile(Dax$mn, probs=0.70)

quantile(Dax$mx)

#-- max
# 1. min / max together
range(Dax$mx)

# 2. quantiles
quantile(Dax$mx) #pick a column ...
quantile(Dax$mx, probs=0.90)
qq <- quantile(Dax$mx, probs=0.90)
# quantile(Dax$mn, probs=c(0.10,0.90))
quantile(Dax$mx, probs=0.90)
quantile(Dax$mx, probs=0.80)
quantile(Dax$mx, probs=0.70)

quantile(Dax$mx)

#--- OC ------
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/CAC_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/F100_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dow_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/N225_2000_d.csv")
Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Oz_2000.csv")

Dax$OC <- abs(Dax$Open - Dax$Close) # really want absolute value here
range(Dax$OC)
quantile(Dax$OC)

#-- also need to know OC > 90% minor qt
Dax$OH <- Dax$High - Dax$Open


#a. OH > 46 i.e.> 90% Q
Dax$OH46 <- ifelse(Dax$OH>46,Dax$OH,NA)
#length(Dax$OH[Dax$OH>46])
range(Dax$OH46,na.rm=TRUE)
quantile(Dax$OH46,na.rm=TRUE)

#b. interested in OC on days OH>46
Dax$OC_OH <- ifelse(Dax$OH>46,Dax$Close-(Dax$Open+46),NA)
range(Dax$OC_OH,na.rm=TRUE)
quantile(Dax$OC_OH,na.rm=TRUE)

#c. OL > 46
Dax$OL <- Dax$Open - Dax$Low
Dax$OL46 <- ifelse(Dax$OL>46,Dax$OL,NA)
range(Dax$OL46,na.rm=TRUE)
quantile(Dax$OL46,na.rm=TRUE)

#d. interested in OC on days OL>46
Dax$OC_OL <- ifelse(Dax$OL>46,(Dax$Open-46)-Dax$Close,NA)
range(Dax$OC_OL,na.rm=TRUE)
quantile(Dax$OC_OL,na.rm=TRUE)
sum(Dax$OC_OL,na.rm=TRUE)


quantile(Dax$OH,probs=0.5)
length(Dax$OH[Dax$OH>35])
sum(Dax$OH[Dax$OH>35])
sum(ifelse(Dax$OH>46,Dax$Close-(Dax$Open+46),0))
qq <- quantile(Dax$OH,probs=0.5)

calcOH <- function(x){
sum(ifelse(Dax$OH>quantile(Dax$OH,probs=x),Dax$Close-(Dax$Open+quantile(Dax$OH,probs=x)),0))
}
calcOH(0.1)
calcOH(0.4)
calcOH(0.6)
calcOH(0.8)
calcOH(0.9)

#-- Currency
Curr = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/EURGBP.csv")
Curr$prevCl <- c( NA, Curr$Close[ - length(Curr$Close) ] )

Curr$CH <- Curr$High - Curr$prevCl
Curr$CL <- Curr$prevCl - Curr$Low
Curr$mn <- ifelse(Curr$CH>Curr$CL,Curr$CL,Curr$CH)
Curr$mx <- ifelse(Curr$CH>Curr$CL,Curr$CH,Curr$CL)

range(Curr$mn, na.rm=TRUE)
quantile(Curr$mn, na.rm=TRUE)
quantile(Curr$mn, na.rm=TRUE,probs=0.9)

range(Curr$mx, na.rm=TRUE)
quantile(Curr$mx, na.rm=TRUE)
quantile(Curr$mx, na.rm=TRUE,probs=0.9)

tail(Curr)

# -----------------------------------
# kind of system
sl = 30 #stop loss
qq <- quantile(Dax$mn, probs=0.80)
Dax$QC <- Dax$absOC - qq  #calc abs o-c minus quantile move ...
sum(Dax$QC) #not really taking o-H , O-L => count mn > quantile x SLoss?
loss <- length(which(Dax$mn > qq)) * sl
loss
sum(Dax$QC) - loss
(sum(Dax$QC) - loss) / 13
sum(Dax$QC) / 13 / 52
mean(Dax$absOC)
quantile(Dax$mx, probs=0.20)

Dax = read.csv("D:/Allan/DropBox/MSc/Dissertation/Thesis/Data/Dax_2000_d.csv")
tail(Dax)
Dax$Close[3570]
