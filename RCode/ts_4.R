ts_4 <- function(Mkt, SLoss, MktName){
  #   trading system based on prediction from Hybrid/ARIMA models working with categorical
  #   label with valued U or D
  #
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out  
  #
  # Returns:
  #   results vector.
  
  results <- createResultsVector(MktName, SLoss)
  Mkt$p_p <- c( NA, Mkt$pred[ - length(Mkt$pred) ] ) # prev pred
  
  # Trade Long
  Mkt$Long <- ifelse(Mkt$p_p == "U", Mkt$Close - Mkt$Open, NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$p_p == "U",
                       ifelse((Mkt$Low-Mkt$Open) < SLoss, SLoss, Mkt$Long),
                       Mkt$Long)
    results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  }
  
  # Trade Short
  Mkt$Short <- ifelse(Mkt$p_p == "D", Mkt$Open - Mkt$Close, NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0){
    Mkt$Short <- ifelse(Mkt$p_p == "D",
                        ifelse((Mkt$Open-Mkt$High) < SLoss, SLoss, Mkt$Short),
                        Mkt$Short)
    results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  }
  
  Stats <- calcStats2(Mkt$Long)
  results[5:7] <- Stats
  
  Stats <- calcStats2(Mkt$Short)
  results[8:10] <- Stats
  
  return(results)
}
