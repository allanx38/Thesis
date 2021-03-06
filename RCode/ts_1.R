ts_1 <- function(Mkt, SLoss, MktName){
  # Trading system using predictions from ARIMA models. Uses relative 
  # value of the forecast with the previous close
  #
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out  
  # Returns:
  #   results vector.
  
  results <- createResultsVector(MktName, SLoss)
  
  Mkt$p_c <- c( NA, Mkt$Close[ - length(Mkt$Close) ] ) # prev close
  Mkt$p_p <- c( NA, Mkt$p[ - length(Mkt$p) ] ) # prev pred
  
  # Trade Long
  Mkt$Long <- ifelse(Mkt$p_p > Mkt$p_c, Mkt$Close - Mkt$Open, NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$p_p > Mkt$p_c,
                       ifelse((Mkt$Low-Mkt$Open) < SLoss, SLoss, Mkt$Long),
                       Mkt$Long)
    results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  }
  
  # Trade Short
  Mkt$Short <- ifelse(Mkt$p_p < Mkt$p_c, Mkt$Open - Mkt$Close, NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0){
    Mkt$Short <- ifelse(Mkt$p_p < Mkt$p_c,
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
