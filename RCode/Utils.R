createResultsVector <- function(MktName, SLossValue){
  # Function to create results vector
  #
  # Args:
  #   SLoss: stop loss value
  #   MktName: market's name for print out
  #
  # Returns:
  #   results vector.
  
  results <- rep(0,11)
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
          "misc")      # 1. Name of Mkt
  names(results) <- nm
  results["Mkt"] <- MktName
  results["S Loss"] <- SLossValue
  return(results)
}

calcStats <- function(x){
  # Function to calculate trade stats
  #
  # Args:
  #   x - data set
  #
  # Returns:
  #   results vector.
  
  results <- 1:3
  v <- na.omit(x)
  
  # Win %
  wins <- length(v[v>0])
  losses <- length(v[v<0])
  results[1] <- round(wins/(wins+losses)*100)
  
  # Num Trades
  results[2] <- length(v)
  
  # Av Long PL
  results[3] <- round(sum(v) / length(v))
  
  return(results)
}

calcWinPer <- function(x){
  wins <- length(x[x>0])
  losses <- length(x[x<0])
  return(wins/(wins+losses)*100)
}

calcAverageWin <- function(x){
  wins <- length(x)
  winpl <- sum(x, na.rm=T)
  return((winpl/wins))
}

calcNumTrades <- function(x){
  return(length(na.omit(x)))
}

savepdf <- function(file, width=16, height=10)
{
  fname <- paste("../Figures/",file,".pdf",sep="")
  pdf(fname, width=width/2.54, height=height/2.54,
      pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

#savepdf("filename")
# Plotting commands here
#dev.off()

print_xt <- function(dat,dig,cap,lab,al,filname,inclrnam){
  xt <- xtable(
    dat, 
    digits = dig, 
    caption = cap,
    label = lab
  )
  al <- c('l','l')
  al <- c(al, rep('c',ncol(dat)-1))
  align(xt) <- al
  print(xt, 
        file=filname,
        include.rownames=inclrnam, 
        caption.placement = "top",
        hline.after=NULL,
        add.to.row=list(pos=list(-1,0, nrow(xt)),
                        command=c('\\toprule ', '\\midrule ', '\\bottomrule ')))
  
}