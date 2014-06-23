NaiveFollowPrev <- function(Mkt, SLoss, MktName){
  # Calculates the profit/loss from trading according to a naive idea of trading in the opposite direction to the previous day.
  #
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out
  # Returns:
  #   results vector
  
  results <- createResultsVector(MktName, SLoss)
  
  Mkt$pl <- Mkt$Close - Mkt$Open
  Mkt$prevPL <- c( NA, Mkt$pl[ - length(Mkt$pl) ] )
  
  # Trade Long
  Mkt$Long <- ifelse(Mkt$prevPL<0,Mkt$Close-Mkt$Open,NA)
  results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$prevPL<0,
                       ifelse((Mkt$Low-Mkt$Open) < SLoss, SLoss, Mkt$Long),
                       Mkt$Long)
    
    results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  }
  
  # Trade Short
  Mkt$Short <- ifelse(Mkt$prevPL>0,Mkt$Open-Mkt$Close,NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Short <- ifelse(Mkt$prevPL>0,
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


