# Break Out 90 Quantile

ComboSystemBoutQuant90 <- function(Mkt, SLoss, OffSet , MktName){
  results <- 1:10
  names(results) <- c("Mkt", "S Loss", "LongPL","ShortPL", 
                      "L Win %", "Av L Win", "Av L Loss", "S Win %", "Av S Win", "Av S Loss")
  
  Mkt$prevHigh <- c( NA, Mkt$High[ - length(Mkt$High) ] )
  Mkt$prevLow <- c( NA, Mkt$Low[ - length(Mkt$Low) ] )
  
  Mkt$opPreHigh <- Mkt$prevHigh - Mkt$Open
  Mkt$opPreLow <- Mkt$Open - Mkt$prevLow
  
  Mkt$Long <- ifelse(Mkt$opPreHigh > OffSet, ifelse(Mkt$High > Mkt$prevHigh, Mkt$Close - Mkt$prevHigh, NA), NA)
  results["LongPL"] <- sum(Mkt$Long, na.rm=TRUE)
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$Long<SLoss,SLoss,Mkt$Long)
    results["LongPL"] <- sum(Mkt$Long, na.rm=TRUE)
  }
  
  Mkt$Short <- ifelse(Mkt$opPreLow > OffSet, ifelse(Mkt$Low < Mkt$prevLow, Mkt$prevLow - Mkt$Close, NA), NA)
  results["ShortPL"] <- sum(Mkt$Short, na.rm=TRUE)
  if (SLoss < 0) {
    Mkt$Short <- ifelse(Mkt$Short<SLoss,SLoss,Mkt$Short)
    results["ShortPL"] <- sum(Mkt$Short, na.rm=TRUE)
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
  return(results)
}

setwd("D://Allan//DropBox//MSc//Dissertation//Thesis//RCode")
source("Utils.R")
Mkt <- read.csv("../Data//Dax_2000_d.csv")

ComboSystemBoutQuant90(Mkt, 0, 40, "Dax")
