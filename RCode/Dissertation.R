  # Read
Dax <- read.csv("Data//Dax_2000.csv",header=TRUE)
head(Dax)
tail(Dax)
summary(Dax)

# Stargazer summ info ...
library(stargazer)
stargazer(Dax[1:6,],summary=FALSE) # kind of like head()
stargazer(Dax)  # latex summary

# --------------------- Columns -----------------------
Dax <- read.csv("Data//Dow_2000.csv",header=TRUE)
Dax$prevHigh <- c( NA, Dax$High[ - length(Dax$High) ] )
Dax$prevLow <- c( NA, Dax$Low[ - length(Dax$Low) ] )
Dax$OH <- Dax$High - Dax$Open
Dax$OL <- Dax$Open - Dax$Low
Dax$absOC <- abs(Dax$Open - Dax$Close)
write.csv(Dax,"Data//test.csv")
Dax <- read.csv("Data//test.csv",header=TRUE)
Dax$OpenStoch <- ifelse(!is.na(Dax$prevHigh), ifelse(Dax$Open[Dax$Open<Dax$prevHigh & Dax$Open>Dax$prevLow], round((Dax$Open - Dax$prevLow) / (Dax$prevHigh - Dax$prevLow),2) * 100, NA), NA)
write.csv(Dax,"Data//Dow_2000_stats.csv")
Dax <- read.csv("Data//Dow_2000_stats.csv",header=TRUE)

levels(Dax$)

# --------------------- Open stats -----------------------
# 1. Open bet prev HL
# Dax$prevHigh <- c( "NA", Dax$High[ - length(Dax$High) ] )
# Dax$prevLow <- c( "NA", Dax$Low[ - length(Dax$Low) ] )
# How many open between Prev HL
length(Dax$Open[Dax$Open<Dax$prevHigh & Dax$Open>Dax$prevLow]) / length(Dax$Open) * 100

# 2. where - \% stoch prev HL
# a. Prev H - prev L => if open between
# b. Open - prevL
# c. (Open - prevL) / (Prev H - prev L)
write.csv(Dax,"Data//test.csv")
Dax <- read.csv("Data//test.csv",header=TRUE)

Dax$a <- ifelse(!is.na(Dax$prevHigh), ifelse(Dax$Open[Dax$Open<Dax$prevHigh & Dax$Open>Dax$prevLow], Dax$prevHigh - Dax$prevLow, NA), NA)
#Dax$b <- ifelse(!is.na(Dax$prevHigh), ifelse(Dax$Open[Dax$Open<Dax$prevHigh & Dax$Open>Dax$prevLow], Dax$Open - Dax$prevLow, NA), NA)
Dax$OpenStoch <- ifelse(!is.na(Dax$prevHigh), ifelse(Dax$Open[Dax$Open<Dax$prevHigh & Dax$Open>Dax$prevLow], round((Dax$Open - Dax$prevLow) / (Dax$prevHigh - Dax$prevLow),2) * 100, NA), NA)

#ifelse(!is.na(4), 2, 1)

# ----------------------- CloseStats -----------------------------------
# 1. Close outside prev H/L
round(length(Dax$Close[Dax$Close > Dax$prevHigh | Dax$Close < Dax$prevLow]) / length(Dax$Close) *100) # 56%

# 2. Close outside 10 90 stoch
#Dax$Stoch10 <- ifelse(!is.na(Dax$prevHigh), Dax$prevLow + ((Dax$prevHigh - Dax$prevLow)*0.1), NA)
#Dax$Stoch90 <- ifelse(!is.na(Dax$prevHigh), Dax$prevLow + ((Dax$prevHigh - Dax$prevLow)*0.9), NA)
#Dax$OpenStoch <- ifelse(!is.na(Dax$prevHigh), ifelse(Dax$Open[Dax$Open<Dax$prevHigh & Dax$Open>Dax$prevLow], round((Dax$Open - Dax$prevLow) / (Dax$prevHigh - Dax$prevLow),2) * 100, NA), NA)
length(Dax$Close[Dax$Close > (Dax$prevLow + ((Dax$prevHigh - Dax$prevLow)*0.9)) | Dax$Close < (Dax$prevLow + ((Dax$prevHigh - Dax$prevLow)*0.1))]) / length(Dax$Close) #64%

# 3. Close outside 30 70 stoch
length(Dax$Close[Dax$Close > (Dax$prevLow + ((Dax$prevHigh - Dax$prevLow)*0.7)) | Dax$Close < (Dax$prevLow + ((Dax$prevHigh - Dax$prevLow)*0.3))]) / length(Dax$Close) #81%

# hit both pH and prevL

# -------------------------------- Open Close stats--------------------------
# open between prev HL
#Dax <- read.csv("Data//Dax_2000.csv",header=TRUE)
#Dax <- read.csv("Data//Dow_2000.csv",header=TRUE)
Dax <- read.csv("Data//N225_2000.csv",header=TRUE)
#Dax <- read.csv("Data//F100_2000.csv",header=TRUE)
#Dax <- read.csv("Data//SP500_2000.csv",header=TRUE)
#Dax <- read.csv("Data//Oz_2000.csv",header=TRUE)
Dax$prevHigh <- c( "NA", Dax$High[ - length(Dax$High) ] )
Dax$prevLow <- c( "NA", Dax$Low[ - length(Dax$Low) ] )

#length(Dax$Open[Dax$Open<Dax$prevHigh & Dax$Open>Dax$prevLow]) / length(Dax$Open) * 100
# Close outside prev H/L
#round(length(Dax$Close[Dax$Close > Dax$prevHigh | Dax$Close < Dax$prevLow]) / length(Dax$Close) *100) # 56%
# Combine 2
a <- sum(ifelse(Dax$Open > Dax$prevLow & Dax$Open < Dax$prevHigh,ifelse(Dax$Close>Dax$prevHigh | Dax$Close<Dax$prevLow,TRUE,FALSE),FALSE))

b <- length(Dax$Close)
c <- sum(Dax$Open > Dax$prevLow & Dax$Open < Dax$prevHigh)
a/b
a/c
a <- sum(ifelse(Dax$Open < Dax$prevLow | Dax$Open > Dax$prevHigh,ifelse(Dax$Close>Dax$prevHigh | Dax$Close<Dax$prevLow,TRUE,FALSE),FALSE))
c <- sum(Dax$Open < Dax$prevLow | Dax$Open > Dax$prevHigh)
a/c

# -------------------------------- OH OL stats-------------------------------

Dax <- read.csv("Data//N225_2000_d.csv",header=TRUE)
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

# --------- Base System 1 - 90% quantile ----------------
# essentially a break out system
# buy at movement > 90% qunatile.
# needs av OC > 90% qunatile ...

Dax <- read.csv("Data//Dax_2000.csv",header=TRUE)
Dax <- read.csv("Data//Dow_2000.csv",header=TRUE)
#Dax <- read.csv("Data//N225_2000.csv",header=TRUE)
#Dax <- read.csv("Data//F100_2000.csv",header=TRUE)
#Dax <- read.csv("Data//SP500_2000.csv",header=TRUE)
#Dax <- read.csv("Data//Oz_2000.csv",header=TRUE)
Dax$OH <- Dax$High - Dax$Open
Dax$OL <- Dax$Open - Dax$Low
Dax$absOC <- abs(Dax$Open - Dax$Close)
Dax$mn <- ifelse(Dax$OH>Dax$OL,Dax$OL,Dax$OH)
Dax$mx <- ifelse(Dax$OH>Dax$OL,Dax$OH,Dax$OL)
head(Dax)

# just look at high ...
spread = 2
sloss = -40
qq <- quantile(Dax$mn, probs=0.90)
Dax$HQC <- ifelse((Dax$High - Dax$Open) > qq, Dax$Close - (Dax$Open + qq) - spread, NA)
Dax$HQC <- ifelse(Dax$HQC < sloss, sloss, Dax$HQC)
range(Dax$HQC, na.rm=TRUE)
sum(Dax$HQC, na.rm=TRUE)
sum(Dax$HQC, na.rm=TRUE) / 13

# just look at low ...
qq <- quantile(Dax$mn, probs=0.70)
Dax$LQC <- ifelse((Dax$Open - Dax$Low) > qq, (Dax$Open - qq) - Dax$Close - spread, NA)
range(Dax$LQC, na.rm=TRUE)
Dax$LQC <- ifelse(Dax$LQC < sloss, sloss, Dax$LQC)
range(Dax$LQC, na.rm=TRUE)
sum(Dax$LQC, na.rm=TRUE)
sum(Dax$LQC, na.rm=TRUE) / 13
# head(Dax$LQC, n=20)

write.table(Dax,"D:/RWorkingDir/Data//N225_2000_2.csv", sep=",")
write.csv(Dax,"D:/RWorkingDir/Data//N225_2000_2.csv")


# --------- Base System 2 - SMA ----------------
# Long when prices > MA
# Short when prices < MA

library(TTR)
#Dax <- read.csv("Data//Dax_2000.csv",header=TRUE)
Dax <- read.csv("Data//Dow_2000.csv",header=TRUE)
#Dax <- read.csv("Data//N225_2000.csv",header=TRUE)
#Dax <- read.csv("Data//F100_2000.csv",header=TRUE)
#Dax <- read.csv("Data//SP500_2000.csv",header=TRUE)
#Dax <- read.csv("Data//Oz_2000.csv",header=TRUE)

tail(Dax)
Dax <- read.csv("Data//N225_2000.csv",header=TRUE)
sma <- SMA(Dax["Open"], 300)
Dax <- cbind(Dax, sma)
#tail(Dax[c(1,2,5,8,9)], n=30)
# tail(Dax)
# head(Dax, n=20)
Dax$Long <- ifelse(Dax$Open > Dax$sma, Dax$Close - Dax$Open, NA)
Dax$Short <- ifelse(Dax$Open < Dax$sma, Dax$Open-Dax$Close, NA)

# sum(ifelse(Dax$Open > Dax$sma, Dax$Close - Dax$Open, NA), na.rm=TRUE)
# sum(ifelse(Dax$Open < Dax$sma, Dax$Open-Dax$Close, NA), na.rm=TRUE)
# sum(ifelse(Dax$Long > 0, Dax$Long, NA), na.rm=TRUE)
# sum(ifelse(Dax$Long < 0, Dax$Long, NA), na.rm=TRUE)

sum(Dax$Long, na.rm=TRUE)
sum(Dax$Short, na.rm=TRUE)
sum(Dax$Long, na.rm=TRUE) / sum(!is.na(Dax$Long)) # Long pl per trade
round(sum(!is.na(Dax$Long[Dax$Long > 0])) / sum(!is.na(Dax$Long)) * 100) # Long Winners
sum(Dax$Short, na.rm=TRUE) / sum(!is.na(Dax$Short)) # Short pl per trade
round(sum(!is.na(Dax$Short[Dax$Short > 0])) / sum(!is.na(Dax$Short)) * 100) # Short Winners

#sum(!is.na(Dax$Long[Dax$Long < 0]))
#sum(!is.na(Dax$Short))
#sum(!is.na(Dax$Long))


# --------- Base System 3 - H/L Breakout ----------------



# --------- Nik Dow Corrl
Dow <- read.csv("Data//Dow_2000.csv",header=TRUE,stringsAsFactors = FALSE)
Dow <- Dow[c(1,2,3,4,5)]
Dow$Date <- as.Date(Dow$Date, format="%d-%b-%y")
str(Dow)
tail(Dow)
write.csv(Dow,"Data//Dow_2000_d.csv")
Dow2 <- read.csv("Data//Dow_2000_d.csv",header=TRUE,stringsAsFactors = FALSE)

Dow <- read.csv("Data//Dow_96_tick.csv",header=TRUE,stringsAsFactors = FALSE)
Dow <- Dow[c(1,2,3,4,5)]
Dow$Date <- as.Date(Dow$Date, format="%d/%m/%Y")
str(Dow)
tail(Dow)
write.csv(Dow,"Data//Dow_96_tick_d.csv")
Dow2 <- read.csv("Data//Dow_2000_d.csv",header=TRUE,stringsAsFactors = FALSE)


Dax <- read.csv("Data//Dax_2000.csv",header=TRUE, stringsAsFactors = FALSE)
Dax <- Dax[c(1,2,3,4,5)]
tail(Dax)
Dax$Date <- as.Date(Dax$Date, format="%d/%m/%Y")
write.csv(Dax,"Data//Dax_2000_d.csv")

Dax <- read.csv("Data//F100_2000.csv",header=TRUE, stringsAsFactors = FALSE)
#Dax <- Dax[c(1,2,3,4,5)]
tail(Dax)
Dax$Date <- as.Date(Dax$Date, format="%d/%m/%Y")
write.csv(Dax,"Data//F100_2000_d.csv")

Nik <- read.csv("Data//N225_2000.csv",header=TRUE, stringsAsFactors = FALSE)
#Nik <- Nik[c(1,2,3,4,5)]
tail(Nik)
Nik$Date <- as.Date(Nik$Date, format="%d/%m/%Y")
write.csv(Nik,"Data//N225_2000_d.csv")

Dax <- read.csv("Data//CAC_2000.csv",header=TRUE, stringsAsFactors = FALSE)
#Dax <- Dax[c(1,2,3,4,5)]
tail(Dax)
Dax$Date <- as.Date(Dax$Date, format="%d/%m/%Y")
write.csv(Dax,"Data//CAC_2000_d.csv")

Dax <- read.csv("Data//Oz_2000.csv",header=TRUE, stringsAsFactors = FALSE)
#Dax <- Dax[c(1,2,3,4,5)]
tail(Dax)
Dax$Date <- as.Date(Dax$Date, format="%d/%m/%Y")
write.csv(Dax,"Data//Oz_2000_d.csv")

Dax <- read.csv("Data//Silver.csv",header=TRUE, stringsAsFactors = FALSE)
#Dax <- Dax[c(1,2,3,4,5)]
tail(Dax)
str(Dax)
Dax$DATE <- as.Date(Dax$DATE, format="%d/%m/%Y")
write.csv(Dax,"Data//Gold_d.csv")

Dax <- read.csv("Data//GBUS.csv",header=TRUE, stringsAsFactors = FALSE)
#Dax <- Dax[c(1,2,3,4,5)]
tail(Dax)
str(Dax)
Dax$Date <- as.Date(Dax$Date, format="%d/%m/%Y")
write.csv(Dax,"Data//GBUS_d.csv")


#DD <- merge(Nik, Dow, by = "Date")
DD <- merge(Nik, Dax, by = "Date")

tail(Dow)
head(Dow)
tail(Dax)
head(Dax)
tail(DD)
head(DD)
?merge()
intersect(Dow$Date, Dax$Date)
newdata <- mtcars[order(mpg),]
DD.order <- order(DD$Date)
order.pop <- order(some.states$Population)
str(DD)
DD$Date <- as.Date(DD$Date, format="%d-%b-%y")
head(dt)

# a <- ifelse((DD$Close.x - DD$Open.x)>0,(DD$Close.y - DD$Open.y),(DD$Open.y - DD$Close.y))
# sum(a)
# sum(a)/13

DD$IndPL <- ifelse((DD$Close.x - DD$Open.x)>0,(DD$Close.y - DD$Open.y),(DD$Open.y - DD$Close.y))
win <- ifelse(DD$IndPL>0,DD$IndPL,NA) #winnwers
lose <- ifelse(DD$IndPL<0,DD$IndPL,NA) # losers
sum(win, na.rm=TRUE) + sum(lose, na.rm=TRUE) #PL
mean(lose, na.rm=TRUE) # av win
mean(win, na.rm=TRUE) # av loss
sum(DD$IndPL>0) /  ( sum(DD$IndPL<0) + sum(DD$IndPL>0) ) # win %

# Long
DD$IndPL <- ifelse((DD$Close.x - DD$Open.x)>0,(DD$Close.y - DD$Open.y),NA)
win <- ifelse(DD$IndPL>0,DD$IndPL,NA) #winnwers
lose <- ifelse(DD$IndPL<0,DD$IndPL,NA) # losers
sum(win, na.rm=TRUE) + sum(lose, na.rm=TRUE) #PL
mean(lose, na.rm=TRUE) # av win
mean(win, na.rm=TRUE) # av loss
sum(DD$IndPL>0,na.rm=TRUE) /  ( sum(DD$IndPL<0,na.rm=TRUE) + sum(DD$IndPL>0,na.rm=TRUE) ) # win %

# Short
DD$IndPL <- ifelse((DD$Close.x - DD$Open.x)<0,(DD$Open.y - DD$Close.y),NA)
win <- ifelse(DD$IndPL>0,DD$IndPL,NA) #winnwers
lose <- ifelse(DD$IndPL<0,DD$IndPL,NA) # losers
sum(win, na.rm=TRUE) + sum(lose, na.rm=TRUE) #PL
mean(lose, na.rm=TRUE) # av win
mean(win, na.rm=TRUE) # av loss
sum(DD$IndPL>0,na.rm=TRUE) /  ( sum(DD$IndPL<0,na.rm=TRUE) + sum(DD$IndPL>0,na.rm=TRUE) ) # win %


write.csv(DD,"Data//N225Dow.csv")

DD$NikPL <- DD$Close.x - DD$Open.x
DD$DowPL <- DD$Close.y - DD$Open.y
DD$DowOH <- DD$High.y - DD$Open.y
DD$DowOL <- DD$Open.y - DD$Low.y
DD$Dowmn <- ifelse(DD$DowOH>DD$DowOL,DD$DowOL,DD$DowOH)
DD$Dowmx <- ifelse(DD$DowOH>DD$DowOL,DD$DowOH,DD$DowOL)

tail(DD)

a <- ifelse(DD$NikPL>0,DD$DowPL,NA) # dax etc win pl on Nik UP days
amin <- ifelse(DD$NikPL>0,DD$Dowmn,NA) # dax etc min OLHs on NIK UP days
z <- ifelse(a>0,DD$Dowmn, NA) # dax etc min OLHs on dax wins on NIK UP days
quantile(z,na.rm=TRUE)
quantile(amin,na.rm=TRUE)
sum(a,na.rm=TRUE)
sum(a,na.rm=TRUE)/13
a <- ifelse(a < -40, -40, a)
b <- ifelse(a>0,TRUE,FALSE)
win <- sum(b, na.rm=TRUE)
lose <- sum(!b, na.rm=TRUE)
round(win / (win+lose) * 100)

tail(DD$NikPL)
a <- ifelse(DD$NikPL< (-0),-DD$DowPL,NA)
a <- ifelse(a < -125, -125, a)
b <- ifelse(a>0,TRUE,FALSE)
win <- sum(b, na.rm=TRUE)
lose <- sum(!b, na.rm=TRUE)
round(win / (win+lose) * 100)

mean(ifelse(DD$NikPL>0,DD$DowPL,NA),na.rm=TRUE)


DD$Test <- lapply(DD$NikPL, function(x) x/10)
tail(DD)

myfn <- function(x){
  if x > 0 
    if y > 40
}

# --------- O-C - make up
library(TTR)
Dax <- read.csv("Data//Dow_2000.csv",header=TRUE)
Dax$OC <- round(abs(Dax$Open - Dax$Close))
sma6OC <- round(SMA(Dax$OC, 6))
sma20OC <- round(SMA(Dax$OC, 20))
Dax <- cbind(Dax, sma6OC, sma20OC)
tail(Dax, n=50)

#--------------------------------------------------------------
library(TTR)
# DVI - oscillator
Dax$DVI <- DVI(Dax$Close)
tail(Dax, n=20)
write.csv(Dax,"D:/RWorkingDir/Data//test.csv")
colnames(Dax)
rownames(Dax)
names(Dax)

Dax2 <- read.csv("Data//test.csv",header=TRUE)
write.csv(Dax2,"D:/RWorkingDir/Data//test2.csv")
colnames(Dax2)
names(Dax2)

# DVI strat , long below 0.5
Dax$HQC <- ifelse((Dax$High - Dax$Open) > qq, Dax$Close - (Dax$Open + qq) - spread, NA)

# ifelse(<condition>,ifelse(<condition>,<yes>,<no>),<no>)
Dax2$HDiv <- ifelse((Dax2$High - Dax2$Open) > qq, ifelse(Dax2$DVI.dvi > 0.5, Dax2$Close - (Dax2$Open + qq) - spread, NA), NA)
sum(Dax2$HDiv, na.rm=TRUE)
length(Dax2$HDiv[>0.5])
summary(Dax)

sum(!is.na(Dax2$HDiv)) # counts non NA occurences => ans 413  WHY????, rtn T or F??
Dax2$HDiv[!complete.cases(Dax2$HDiv)] # => list if NA
Dax2$HDiv[complete.cases(Dax2$HDiv)]  # => lists 413 rows
length(Dax2$HDiv[complete.cases(Dax2$HDiv)]) # => ans 413, 3157 for NA

winners <- Dax2$HDiv[Dax2$HDiv>0]
sum(winners, na.rm=TRUE)
sum(!is.na(winners))
losers <- Dax2$HDiv[Dax2$HDiv<0]
sum(losers, na.rm=TRUE)
sum(!is.na(losers))

sum(Dax2$HDiv[Dax2$HDiv>0], na.rm=TRUE)
sum(!is.na(Dax2$HDiv[Dax2$HDiv>0]))

# to know
# 1. days mx > absOC
# 2. days mn > qq value (80% for 0.8?)