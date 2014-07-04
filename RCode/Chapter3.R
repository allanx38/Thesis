# Chapter 3

setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")
#setwd("F:/Allan/R Stuff/MSc/RCode")

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
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))


# -------------------------------------------------------
# 1. head of Dax
Dax <- read.csv("../Data/Dax_2000_d.csv", stringsAsFactors = FALSE)

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
savepdf("chp3_dax_2000_2013")
Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y')
#qplot(Dax$Date, Dax$Close, data = Dax, geom='line') + ggtitle("Dax 2000-2013") + ylab('')
ggplot(Dax,aes(x=Date,y=Close)) + geom_line() + ggtitle("Dax Prices between 2000 and 2013") + ylab('Dax Closing Price')
dev.off()

# 5 plot Dax 2000
savepdf(chp3_dax_2013)
x <- c(3316:3570)
Dx2 <- Dax[,c(1,2,5)]
Dx2 <- Dx2[x,]
#tail(Dx2)
ggplot(Dx2,aes(x=Date,y=Close)) + geom_line() + ggtitle("Dax Prices in 2013") + ylab('Dax Closing Price')
dev.off()


# 6. ATR 
# a. Table
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

# b. plot
Dax3 <- Dax[,c(1,6,7)]
tail(Dax3)
#Dax3$Date <- as.POSIXct(Dax3$Date3,format='%d/%m/%Y')
Dax3$Date <- as.POSIXct(Dax3$Date3,format='%Y-%m-%d')

savepdf("chp3_Dax_atr")
ggplot(Dax3,aes(x=Date,y=atr_av)) + geom_line() + ggtitle("ATR / Closing Prices 2000 to 2013") + ylab('ATR / Closing Prices')
dev.off()

# 7. opening prices - table
Open_in_prev_HL <- function(fil, nm){
  df <- t(c('a','b'))
  #browser()
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    Mkt$prevHigh <- c( NA, Mkt$High[ - length(Mkt$High) ] )
    Mkt$prevLow <- c( NA, Mkt$Low[ - length(Mkt$Low) ] )
        Mkt$C_pHL <- ifelse(Mkt$Open>Mkt$prevLow & Mkt$Open<Mkt$prevHigh, 1,0)
    a <- round((sum(Mkt$C_pHL,na.rm=T)/length(Mkt$C_pHL))*100)
    b <- nm[i]
    df <- rbind(df,t(c(a,b)))
  }
  colnames(df) <- c('Mkt', 'Open')
  df <- df[-1,]
}

res <- Open_in_prev_HL(fil,nm)

# produce latex table
dat <- res
dig <- 0
cap = c('Open inside previous H/L',
        'Open inside previous H/L')
lab = 'tab:open_in_prev_HL'
filname ='../Tables/chp_3_open_in_prev_HL.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

# -------------------------------------hxcvbn34567890-uiopu\zxcbzxcbnk
# 8. closing prices - table - 3 x tables

fil <- c("../Data/Dax_2000_d.csv", 
         "../Data/F100_2000_d.csv",
         "../Data/Dow_2000_d.csv",
         "../Data/N225_2000.csv",
         "../Data/Oz_2000.csv")
nm <- c("Dax", "F100", "Dow", "Nik", "Oz")

Close_out_prev_HL <- function(fil, nm){
  df <- t(c('a','b'))
  #browser()
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    Mkt$prevHigh <- c( NA, Mkt$High[ - length(Mkt$High) ] )
    Mkt$prevLow <- c( NA, Mkt$Low[ - length(Mkt$Low) ] )
    Mkt$C_pHL <- ifelse(Mkt$Close<Mkt$prevLow, 1, ifelse(Mkt$Close>Mkt$prevHigh,1,0))
    a <- round((sum(Mkt$C_pHL,na.rm=T)/length(Mkt$C_pHL))*100)
    b <- nm[i]
    df <- rbind(df,t(c(a,b)))
  }
  colnames(df) <- c('Mkt', 'Open')
  df <- df[-1,]
}

# a. Close outside prev H/L
res <- Close_out_prev_HL(fil,nm)

# produce latex table
dat <- res
dig <- 0
cap = c('Close outside previous H/L',
        'Close outside previous H/L')
lab = 'tab:close_out_prev_HL'
filname ='../Tables/chp_3_close_out_prev_HL.tex'
inclrnam=FALSE
print_xt(dat,dig,cap,lab,al,filname,inclrnam)

#round(length(Dax$Close[Dax$Close > Dax$prevHigh | Dax$Close < Dax$prevLow]) / length(Dax$Close) *100)

# -----------------------------------------
# b. Op to Close min max



# c. Open to Close quantiles ...
tail(Dax)
Dax$OC <- abs(Dax$Open - Dax$Close)
range(Dax$OC)
quantile(Dax$OC) #pick a column ...
qq <- quantile(Dax$OC, probs=0.90)

# 9. High / Low Price x1 table


# 10. OH/OL fluctuations - x5 tables

Dax$OL <- Dax$Open - Dax$Low
#Dax$absOC <- abs(Dax$Open - Dax$Close) # really want absolute value here
Dax$mn <- ifelse(Dax$OH>Dax$OL,Dax$OL,Dax$OH)
Dax$mx <- ifelse(Dax$OH>Dax$OL,Dax$OH,Dax$OL)

# a. Minor move ...
range(Dax$mn)

# b. quantiles of minor move
quantile(Dax$mn) #pick a column ...
qq <- quantile(Dax$mn, probs=0.90)

# c. Major move ...
range(Dax$mx)

# d. major move quantiles
quantile(Dax$mx) #pick a column ...
quantile(Dax$mx, probs=0.90)

# e. Op to Closw quantiles
Dax$OC <- abs(Dax$Open - Dax$Close) # really want absolute value here
range(Dax$OC)
quantile(Dax$OC)
