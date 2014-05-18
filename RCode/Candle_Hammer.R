candle_hammer <- function(Mkt, SLoss, MktName){
  # Calculates the profit/loss from trading a based on candelstick Hammer.
  #
  #   Mkt: market data 
  #   SLoss: stop loss 
  #   MktName: market's name for print out  
  #
  # Returns:
  #   results vector.
  
  results <- createResultsVector(MktName, SLoss)

  Mkt$prev_Hammer   <- c( NA, Mkt$Hammer[ - length(Mkt$Hammer) ] )
  Mkt$prev_Inv_Hammer   <- c( NA, Mkt$InvertedHammer[ - length(Mkt$InvertedHammer) ] )
  #Mkt$prev_Hanging_Man   <- c( NA, Mkt$Hammer[ - length(Mkt$Hammer) ] )
  #Mkt$prev_Shooting_Star   <- c( NA, Mkt$InvertedHammer[ - length(Mkt$InvertedHammer) ] )
  
  # Trade Long
  Mkt$Long <- ifelse(Mkt$prev_Hammer==TRUE | Mkt$prev_Inv_Hammer==TRUE, Mkt$Close-Mkt$Open, NA)
   results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  #Adj for SLoss
  if (SLoss < 0) {
    Mkt$Long <- ifelse((Mkt$prev_Hammer==TRUE | Mkt$prev_Inv_Hammer==TRUE) > 0,
                       ifelse((Mkt$Low-Mkt$Open) < SLoss, SLoss, Mkt$Long),
                       Mkt$Long)
    results["LongPL"] <- round(sum(Mkt$Long, na.rm=TRUE))
  }
  
  # Trade Short
  # Mkt$Short <- ifelse(Mkt$prev_Aroon_UP >= 70, ifelse(Mkt$prev_Hammer=='Shooting Star' | Mkt$Hammer=='Hanging Man', Mkt$Close-Mkt$Open, NA) ,NA)
  # results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  #Adj for SLoss
  #   if (SLoss < 0){
  #     Mkt$Short <- ifelse((Mkt$Open - Mkt$Low) > 0,
  #                         ifelse((Mkt$Open-Mkt$High) < SLoss, SLoss, Mkt$Short),
  #                         Mkt$Short)
  #     results["ShortPL"] <- round(sum(Mkt$Short, na.rm=TRUE))
  #   }
  
  Stats <- calcStats(Mkt$Long)
  results[5:7] <- Stats
  
  # Stats <- calcStats(Mkt$Short)
  # results[8:10] <- Stats

  return(results)
}
