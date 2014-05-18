# in non-trending mkt
# do we go back to mean??

BaseSystem1SMA_Rev <- function(Mkt, sma, SLoss, MktName){
  # Calculates the profit/loss from trading according to SMA.
  #
  # Args:
  #   x: 
  #   y: 
  #
  # Returns:
  #   profit/loss from trading according to SMA.
  
  results <- 1:11
  names(results) <- c("Mkt", "SMA", "S Loss", "LongPL","ShortPL", 
                      "L Win %", "Av L Win", "Av L Loss", "S Win %", "Av S Win", "Av S Loss")
  
  sma.value <- SMA(Mkt["Open"], sma)  #create sma vector
  Mkt <- cbind(Mkt, sma.value)        #add sma vector as new col
  
  Mkt$Long <- ifelse(Mkt$Open > Mkt$sma.value, Mkt$Close - Mkt$Open, NA)
  results["LongPL"] <- sum(Mkt$Long, na.rm=T)
  if (SLoss < 0) {
    Mkt$Long <- ifelse(Mkt$Long < SLoss, SLoss, Mkt$Long)
    results["LongPL"] <- sum(Mkt$Long, na.rm=T)
  }
  
  # revert to mean
  Mkt$LongMR <- ifelse(Mkt$Open < Mkt$sma.value, Mkt$Close - Mkt$Open, NA)
  results["LongPL"] <- sum(Mkt$LongMR, na.rm=T)
  if (SLoss < 0) {
    Mkt$LongMR <- ifelse(Mkt$LongMR < SLoss, SLoss, Mkt$LongMR)
    results["LongPL"] <- sum(Mkt$LongMR, na.rm=T)
  }
  
  Mkt$Short <- ifelse(Mkt$Open < Mkt$sma.value, Mkt$Open - Mkt$Close, NA)
  results["ShortPL"] <- sum(Mkt$Short, na.rm=T)
  if (SLoss < 0) {
    Mkt$Short <- ifelse(Mkt$Short < SLoss, SLoss, Mkt$Short)
    results["ShortPL"] <- sum(Mkt$Short, na.rm=T)
  }
  
  # revert to mean
  Mkt$ShortMR <- ifelse(Mkt$Open > Mkt$sma.value, Mkt$Open - Mkt$Close, NA)
  results["ShortPL"] <- sum(Mkt$ShortMR, na.rm=T)
  if (SLoss < 0) {
    Mkt$ShortMR <- ifelse(Mkt$ShortMR < SLoss, SLoss, Mkt$ShortMR)
    results["ShortPL"] <- sum(Mkt$ShortMR, na.rm=T)
  }
  
  # Long Winner %
  results["L Win %"] <- calcWinPer(Mkt$Long)
  
  # Av Long Win
  results["Av L Win"] <- calcAverageWin(Mkt$Long[Mkt$Long > 0])
  
  # Av Long Loss
  results["Av L Loss"] <- calcAverageWin(Mkt$Long[Mkt$Long < 0])
  
  # Short Winner %
  results["S Win %"] <- calcWinPer(Mkt$Short)
  
  # Av Short Win
  results["Av S Win"] <- calcAverageWin(Mkt$Short[Mkt$Short > 0])
  
  # Av Short Loss
  results["Av S Loss"] <- calcAverageWin(Mkt$Short[Mkt$Short < 0])
  
  results <- round(results)
  results["Mkt"] <- MktName
  results["S Loss"] <- SLoss
  results["SMA"] <- sma
  
  write.csv(Mkt, "D://Allan//DropBox//MSc//Dissertation//Thesis//RCode//SMARev.csv")
  
  return(results)
}
