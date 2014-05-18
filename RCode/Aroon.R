aroon_sys <- function(Mkt, SLoss, MktName){
  # uses Aroon indicator to trigger rades
  #
  # Args:
  #   Mkt:      Data
  #   SLoss:    Stop Loss (if 0 not used)
  #   MktName:  Name of market
  #
  # Returns:
  #   results vector.
  
  results <- createResultsVector(MktName, SLoss)
  
    # Trade Long
  Mkt$Long <- ifelse(Mkt$aroonUp >= 70,Mkt$Close-Mkt$Open,NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$aroonUp >= 70,
                       ifelse((Mkt$Low-Mkt$Open) < SLoss, SLoss, Mkt$Long),
                       Mkt$Long)
    results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  }
  
  # Trade Short
  Mkt$Short <- ifelse(Mkt$aroonDn >= 70,Mkt$Open-Mkt$Close,NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  if (SLoss < 0) {
      Mkt$Short <- ifelse(Mkt$aroonDn >= 70,
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


