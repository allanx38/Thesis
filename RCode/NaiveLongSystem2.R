NaiveLongSystem2 <- function(Mkt, SLoss, MktName){
  # Calculates the profit/loss from simply trading long each day. 
  # Opening price is previous day's close price.
  #
  # Args:
  #   Mkt: market data 
  #   SLoss: stop loss
  #   MktName: name of market data
  #
  # Returns:
  #   results vector.
  
  results <- createResultsVector(MktName, SLoss)
  
  Mkt$prevCl <- c(NA,Mkt$Close[ - length(Mkt$Close) ])
  
  # Buy Long
  Mkt$Long <- Mkt$Close - Mkt$prevCl
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


