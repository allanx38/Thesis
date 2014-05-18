BaseSystem3Quant902 <- function(Mkt, SLoss, MktName){
  # Calculates the profit/loss from trading a breakout of a 90% quantile move.
  #
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out  
  #
  # Returns:
  #   results vector.
  
  results <- createResultsVector(MktName, SLoss)
  
  Mkt$OH <- Mkt$High - Mkt$Open
  Mkt$OL <- Mkt$Open - Mkt$Low
  Mkt$mn <- ifelse(Mkt$OH>Mkt$OL,Mkt$OL,Mkt$OH)
  Mkt$mx <- ifelse(Mkt$OH>Mkt$OL,Mkt$OH,Mkt$OL)
  qq <- quantile(Mkt$mn, probs=0.90)
  
  # Trade Long
  Mkt$Long <- ifelse((Mkt$High - Mkt$Open) > qq, Mkt$Close - (Mkt$Open + qq), NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Long <- ifelse((Mkt$High - Mkt$Open) > qq,
                       ifelse((Mkt$Low-Mkt$Open) < SLoss, SLoss, Mkt$Long),
                       Mkt$Long)
    results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  }
  
  # Trade Short
  Mkt$Short <- ifelse((Mkt$Open - Mkt$Low) > qq, (Mkt$Open - qq) - Mkt$Close, NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0){
    Mkt$Short <- ifelse((Mkt$Open - Mkt$Low) > qq,
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
