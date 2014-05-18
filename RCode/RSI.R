MACD_OB <- function(Mkt, SLoss, MktName,lw, up){
  # MACD over-bought/sold system.
  #
  # Args:
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out
  #   lw: value of MACD that signals end of bear runs and rev
  #   up: value of MACD that signals end of bull runs and rev
  #
  # Returns:
  #   profit/loss from trading according to SMA.
  
  results <- 1:10
  names(results) <- c("Mkt", "S Loss", "LongPL","ShortPL", 
                      "L Win %", "Av L Win", "Av L Loss", "S Win %", "Av S Win", "Av S Loss")
  
  #Mkt$prevPL <- c( NA, Mkt$Close[ - length(Mkt$Close) ] - Mkt$Open[ - length(Mkt$Open) ] )
  
  # Break out high
  #browser()
  Mkt$Long <- ifelse(Mkt$macd < lw,Mkt$Close-Mkt$Open,NA)
  results["LongPL"] <- sum(Mkt$Long, na.rm=TRUE)
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$Long<SLoss,SLoss,Mkt$Long)
    results["LongPL"] <- sum(Mkt$Long, na.rm=TRUE)
  }
  
  # Break out low
  Mkt$Short <- ifelse(Mkt$macd > up,Mkt$Open-Mkt$Close,NA)
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


