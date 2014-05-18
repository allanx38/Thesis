candle_engulf <- function(Mkt, SLoss, MktName){
  # Calculates the profit/loss from trading a based on an Engulfing candelstick.
  #
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out  
  #
  # Returns:
  #   results vector.
  
  results <- createResultsVector(MktName, SLoss)

  Mkt$prev_Bull_Engulf <- c( NA, Mkt$Bull.Engulfing[ - length(Mkt$Bull.Engulfing) ] )
  Mkt$prev_Bear_Engulf <- c( NA, Mkt$Bear.Engulfing[ - length(Mkt$Bear.Engulfing) ] )
  
  # Trade Long
   Mkt$Long <- ifelse(Mkt$prev_Bull_Engulf==TRUE, Mkt$Close-Mkt$Open, NA)
    results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
#   #Adj for SLoss
   if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$prev_Bull_Engulf == TRUE, 
                       ifelse( (Mkt$Low-Mkt$Open) < SLoss, SLoss, Mkt$Long),
                       Mkt$Long)
     results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
   }
   
   # Trade Short
  Mkt$Short <- ifelse(Mkt$prev_Bear_Engulf,Mkt$Open-Mkt$Close,NA)
  results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Short <- ifelse(Mkt$prev_Bear_Engulf,
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
