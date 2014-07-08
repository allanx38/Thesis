BaseSystem2Bout2 <- function(Mkt, SLoss, MktName){
  # Trading system based on the break out of the previous day's high/low value.
  #
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out
  # Returns:
  #   results vector.
  
  results <- createResultsVector(MktName, SLoss)
  
  Mkt$prevHigh <- c( NA, Mkt$High[ - length(Mkt$High) ] )
  Mkt$prevLow <- c( NA, Mkt$Low[ - length(Mkt$Low) ] )
  #calc open price
  Mkt$T_high <- ifelse(Mkt$Open<Mkt$prevHigh,Mkt$prevHigh,Mkt$Open)
  Mkt$T_low <- ifelse(Mkt$Open>Mkt$prevLow,Mkt$prevLow,Mkt$Open)
  
  # Break out high
  Mkt$Long <- ifelse(Mkt$High>Mkt$T_high,Mkt$Close-Mkt$T_high,NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$T_high>Mkt$prevHigh,
                       ifelse((Mkt$Low-Mkt$Open) < SLoss, SLoss, Mkt$Long),
                       Mkt$Long)
    results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  }
  
  # Break out low
  Mkt$Short <- ifelse(Mkt$Low<Mkt$T_low,Mkt$T_low-Mkt$Close,NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  if (SLoss < 0) {
    Mkt$Short <- ifelse(Mkt$Low<Mkt$T_low,
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


