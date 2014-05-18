BaseSystem1SMA <- function(Mkt, sma, SLoss, MktName){
  # Calculates the profit/loss from trading according to SMA.
  #
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out 
  #
  # Returns:
  #   profit/loss from trading according to SMA.
  
  results <- createResultsVector(MktName, SLoss)
  
  sma.value <- SMA(Mkt["Open"], sma)  #create sma vector
  Mkt <- cbind(Mkt, sma.value)        #add sma vector as new col
  
  # Trade Long
  #browser()
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
  
  Stats <- calcStats(Mkt$Long)
  results[5:7] <- Stats
  
  Stats <- calcStats(Mkt$Short)
  results[8:10] <- Stats
  
  results[11] <- sma
  nm <- c("Mkt",          # 1. Name of Mkt
          "S Loss",       # 1. Name of Mkt
          "LongPL",       # 1. Name of Mkt
          "ShortPL",      # 1. Name of Mkt
          "L Win %",      # 1. Name of Mkt
          "L Trades",    # 1. Name of Mkt
          "Av L PL",      # 1. Name of Mkt
          "S Win %",      # 1. Name of Mkt
          "S Trades",    # 1. Name of Mkt
          "Av S PL",
          "SMA") 
  names(results) <- nm
  
  #write.csv(Mkt, 'smatest.csv')
  
  return(results)
}
