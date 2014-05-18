library(TTR)

calcWinPer <- function(x){
  wins <- length(x[x>0])
  losses <- length(x[x<0])
  return(wins/(wins+losses)*100)
}

calcAverageWin <- function(x){
  #browser()
  wins <- length(x)
  winpl <- sum(x, na.rm=T)
  return((winpl/wins))
}

BaseSystem1SMA <- function(Mkt, sma, SLoss, MktName, Spread){
  # Calculates the profit/loss from trading according to SMA.
  #
  # Args:
  #   x: 
  #   y: 
  #
  # Returns:
  #   profit/loss from trading according to SMA.
  
  results <- 1:11
  names(results) <- c("Mkt", "SMA", "S Loss", "LongPL","ShortPL", 
                      "L Win %", "Av L Win", "Av L Loss", "S Win %", "Av S Win", "Av S Loss")
  
  sma.value <- SMA(Mkt["Open"], sma)  #create sma vector
  Mkt <- cbind(Mkt, sma.value)        #add sma vector as new col
  
  Mkt$Long <- ifelse(Mkt$Open > Mkt$sma.value, Mkt$Close - Mkt$Open - Spread, NA)
  results["LongPL"] <- sum(Mkt$Long, na.rm=T)
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$Long < SLoss, SLoss - Spread, Mkt$Long)
    results["LongPL"] <- sum(Mkt$Long, na.rm=T)
  }
  
  Mkt$Short <- ifelse(Mkt$Open < Mkt$sma.value, Mkt$Open - Mkt$Close - Spread, NA)
  results["ShortPL"] <- sum(Mkt$Short, na.rm=T)
  if (SLoss < 0) {
    Mkt$Short <- ifelse(Mkt$Short < SLoss, SLoss - Spread, Mkt$Short)
    results["ShortPL"] <- sum(Mkt$Short, na.rm=T)
  }
  
  # Long Winner %
  results["L Win %"] <- calcWinPer(Mkt$Long)
  
  # Av Long Win
  results["Av L Win"] <- calcAverageWin(Mkt$Long[Mkt$Long > 0])
  
  # Av Long Loss
  results["Av L Loss"] <- calcAverageWin(Mkt$Long[Mkt$Long < 0])
  
  # Short Winner %
  results["S Win %"] <- calcWinPer(Mkt$Short)
  
  # Av Short Win
  results["Av S Win"] <- calcAverageWin(Mkt$Short[Mkt$Short > 0])
  
  # Av Short Loss
  results["Av S Loss"] <- calcAverageWin(Mkt$Short[Mkt$Short < 0])
  
  results <- round(results)
  results["Mkt"] <- MktName
  results["S Loss"] <- SLoss
  results["SMA"] <- sma
  return(results)
}

Dax <- read.csv("D://Allan//DropBox//MSc//Dissertation//Thesis//Data//Dax_2000_d.csv")
spr <- 1
a <- BaseSystem1SMA(Dax, 5, 0, "Dax", spr)
b <- BaseSystem1SMA(Dax, 25, 0, "Dax", spr)
c <- BaseSystem1SMA(Dax, 50, 0, "Dax", spr)
d <- BaseSystem1SMA(Dax, 100, 0, "Dax", spr)
e <- BaseSystem1SMA(Dax, 200, 0, "Dax", spr)

existingDF <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
names(existingDF) <- names(a)
existingDF <- rbind(existingDF, a, b, c, d, e)
existingDF <- existingDF[-1,] ; existingDF

a <- BaseSystem1SMA(Dax, 200, -50, "Dax", spr)
b <- BaseSystem1SMA(Dax, 200, -100, "Dax", spr)
c <- BaseSystem1SMA(Dax, 5, -50, "Dax", spr)
d <- BaseSystem1SMA(Dax, 5, -100, "Dax", spr)

existingDF <- rbind(existingDF, a, b, c, d)
existingDF <- existingDF[-1,] ; existingDF

# ------------ Sys 2 ---------------------------------------------






# ------------ Sys 3 ---------------------------------------------
BaseSystem3Quant902 <- function(Mkt, SLoss, MktName){
  # Calculates the profit/loss from trading according to SMA.
  #
  # Args:
  #   x: 
  #   y: 
  #
  # Returns:
  #   profit/loss from trading according to SMA.
  
  results <- 1:10
  names(results) <- c("Mkt", "S Loss", "LongPL","ShortPL", 
                      "L Win %", "Av L Win", "Av L Loss", "S Win %", "Av S Win", "Av S Loss")
  
  Mkt$OH <- Mkt$High - Mkt$Open
  Mkt$OL <- Mkt$Open - Mkt$Low
  Mkt$mn <- ifelse(Mkt$OH>Mkt$OL,Mkt$OL,Mkt$OH)
  Mkt$mx <- ifelse(Mkt$OH>Mkt$OL,Mkt$OH,Mkt$OL)
  
  qq <- quantile(Mkt$mn, probs=0.90)
  Mkt$Long <- ifelse((Mkt$High - Mkt$Open) > qq, Mkt$Close - (Mkt$Open + qq), NA)
  results["LongPL"] <- sum(Mkt$Long, na.rm=T)
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$Long < SLoss, SLoss, Mkt$Long)
    results["LongPL"] <- sum(Mkt$Long, na.rm=T)
  }
  
  Mkt$Short <- ifelse((Mkt$Open - Mkt$Low) > qq, (Mkt$Open - qq) - Mkt$Close, NA)
  results["ShortPL"] <- sum(Mkt$Short, na.rm=T)
  if (SLoss < 0){
    Mkt$Short <- ifelse(Mkt$Short < SLoss, SLoss, Mkt$Short)
    results["ShortPL"] <- sum(Mkt$Short, na.rm=T)
  }
  
  # Long Winner %
  results["L Win %"] <- calcWinPer(Mkt$Long)
  
  # Av Long Win
  results["Av L Win"] <- calcAverageWin(Mkt$Long[Mkt$Long > 0])
  
  # Av Long Loss
  results["Av L Loss"] <- calcAverageWin(Mkt$Long[Mkt$Long < 0])
  
  # Short Winner %
  results["S Win %"] <- calcWinPer(Mkt$Short)
  
  # Av Short Win
  results["Av S Win"] <- calcAverageWin(Mkt$Short[Mkt$Short > 0])
  
  # Av Short Loss
  results["Av S Loss"] <- calcAverageWin(Mkt$Short[Mkt$Short < 0])
  
  # write.csv(Mkt,"Data//BaseSystem1Q90.csv")
  results <- round(results)
  results["Mkt"] <- MktName
  results["S Loss"] <- SLoss
  return(results)
}

Dax <- read.csv("Data//CommonDate//Dax_2000_d.csv")
Dax <- read.csv("Data//CommonDate//F100_2000_d.csv")
Dax <- read.csv("Data//CommonDate//CAC_2000_d.csv")

source("Dev.R")
a <- BaseSystem1Quant902(Dax, -100, "Dax"); a
b <- BaseSystem1Quant902(Dax, -50, "Dax"); b
c <- BaseSystem1Quant902(Dax, 0, "Dax"); c # No SLOss


existingDF <- as.data.frame(matrix(seq(10),nrow=1,ncol=10))
names(existingDF) <- names(a)
existingDF <- rbind(existingDF, a, b, c)
existingDF <- existingDF[-1,] ; existingDF

