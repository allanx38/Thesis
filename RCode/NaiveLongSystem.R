NaiveLongSystem <- function(Mkt, SLoss, MktName){
  # Calculates the profit/loss from simply trading long.
  #
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out
  #
  # Returns:
  #   results vector.
  
  results <- createResultsVector(MktName, SLoss)
  
  # Buy Long
  Mkt$Long <- Mkt$Close - Mkt$Open
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Long <- ifelse((Mkt$Low-Mkt$Open) < SLoss, SLoss, Mkt$Long)
    results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  }
  
  Stats <- calcStats(Mkt$Long)
  results[5:7] <- Stats
    
  return(results)
}


