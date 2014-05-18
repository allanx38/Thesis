MACD_XO <- function(Mkt, SLoss, MktName){
  # MACD cross-over system.
  #
  # Args:
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out
  #
  # Returns:
  #   results vector.
  
  results <- createResultsVector(MktName, SLoss)
  
  # Trade Long
  Mkt$Long <- ifelse(Mkt$macd>Mkt$signal,Mkt$Close-Mkt$Open,NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$macd>Mkt$signal,
                       ifelse((Mkt$Low-Mkt$Open) < SLoss, SLoss, Mkt$Long),
                       Mkt$Long)
    results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  }
  
  # Trade Short
  Mkt$Short <- ifelse(Mkt$macd<Mkt$signal,Mkt$Open-Mkt$Close,NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Short <- ifelse(Mkt$macd<Mkt$signal,
                        ifelse((Mkt$Open-Mkt$High) < SLoss, SLoss, Mkt$Short),
                        Mkt$Short)
    results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  }
  
  Stats <- calcStats(Mkt$Long)
  results[5:7] <- Stats
  
  Stats <- calcStats(Mkt$Short)
  results[8:10] <- Stats
  
  return(results)
}


