BaseSystem1SMA <- function(Mkt, sma, SLoss, MktName){
  # Calculates the profit/loss from trading according to SMA.
  #
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out 
  # Returns:
  #   profit/loss from trading according to SMA.
  
  results <- createResultsVector(MktName, SLoss)
  
  sma.value <- SMA(Mkt["Open"], sma)  #create sma vector
  Mkt <- cbind(Mkt, sma.value)        #add sma vector as new col
  
  # Trade Long
  Mkt$Long <- ifelse(Mkt$Open > Mkt$sma.value, Mkt$Close - Mkt$Open, NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=T))
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$Open > Mkt$sma.value,
                       ifelse((Mkt$Low-Mkt$Open) < SLoss, SLoss, Mkt$Long),
                       Mkt$Long)
    results["LongPL"] <- round(sum(Mkt$Long, na.rm=T))
  }
  
  # Trade Short
  Mkt$Short <- ifelse(Mkt$Open < Mkt$sma.value, Mkt$Open - Mkt$Close, NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=T))
  if (SLoss < 0) {
    Mkt$Short <- ifelse(Mkt$Open < Mkt$sma.value,
                               ifelse((Mkt$Open-Mkt$High) < SLoss, SLoss, Mkt$Short),
                               Mkt$Short)
    results["ShortPL"] <- round(sum(Mkt$Short, na.rm=T))
  }
  
  #calculate Long results
  results[5:7] <- calcStats(Mkt$Long)
  
  #calculate Short results
  results[8:10] <- calcStats(Mkt$Short)
  
  if (SLoss == 0){
    results[11] <- paste("SMA",sma)
  } else {
    results[11] <- paste("SMA",sma)
  }
  
  
  return(results)
}
