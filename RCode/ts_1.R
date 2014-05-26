ts_1 <- function(Mkt, SLoss, MktName){
  # Calculates the profit/loss from trading a breakout of a 90% quantile move.
  #
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out  
  #
  # Returns:
  #   results vector.
  
  results <- createResultsVector(MktName, SLoss)
  
  Mkt$p_p <- c( NA, Mkt$p_p[ - length(Mkt$p_p) ] ) # prev prediction
  Mkt$p_c <- c( NA, Mkt$Close[ - length(Mkt$Close) ] ) # prev close
  
  # Trade Long
  Mkt$Long <- ifelse(Mkt$p > Mkt$Close, Mkt$Close - Mkt$Open, NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$p > Mkt$Close,
                       ifelse((Mkt$Low-Mkt$Open) < SLoss, SLoss, Mkt$Long),
                       Mkt$Long)
    results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  }
  
  # Trade Short
  Mkt$Short <- ifelse(Mkt$p < Mkt$Close, Mkt$Open - Mkt$Close, NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0){
    Mkt$Short <- ifelse(Mkt$p < Mkt$Close,
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
