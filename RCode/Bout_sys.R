BaseSystem2Bout <- function(Mkt, SLoss, MktName){
  # Calculates the profit/loss from a break out system.
  #
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out  
  #
  # Returns:
  #   results vector.
  
  results <- createResultsVector(MktName, SLoss)
  
  Mkt$prevHigh <- c( NA, Mkt$High[ - length(Mkt$High) ] )
  Mkt$prevLow <- c( NA, Mkt$Low[ - length(Mkt$Low) ] )
  
  # Break out high
  Mkt$Long <- ifelse(Mkt$High>Mkt$prevHigh,Mkt$Close-Mkt$prevHigh,NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$High>Mkt$prevHigh,
                       ifelse((Mkt$Low-Mkt$Open) < SLoss, SLoss, Mkt$Long),
                       Mkt$Long)
    results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  }
  
  # Break out low
  Mkt$Short <- ifelse(Mkt$Low<Mkt$prevLow,Mkt$prevLow-Mkt$Close,NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  if (SLoss < 0) {
    Mkt$Short <- ifelse(Mkt$Low<Mkt$prevLow,
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


