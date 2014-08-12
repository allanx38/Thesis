nm <- c("DAX", "CAC", "FTSE", "Dow", "Nikkei", "AORD")

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
  nam <- c("Mkt",          # 1. Name of Mkt
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
  names(results) <- nam
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

calcStats2 <- function(x){
  # Function to calculate trade stats
  #
  # Args:
  #   x - data set
  #
  # Returns:
  #   results vector.
  #browser()
  results <- 1:3
  #v <- na.omit(x)
  v <- x
  
  # Win %
  wins <- sum(v>0,na.rm=T)
  losses <- sum(v<0,na.rm=T)
  results[1] <- round(wins/(wins+losses)*100)
  
  # Num Trades
  results[2] <- wins+losses
  
  # Av Long PL
  results[3] <- round(sum(v,na.rm=T) / (wins+losses))
  
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


# subtract 2 data frames
# df2 from df1
sub_df <- function(df1, df2){
  
  nc <- ncol(df1)
  ln <- nrow(df1)
  dfres <- df1
  
  for(i in 1:ln){
    for(j in 2:nc){
      dfres[i,j] <- as.numeric(df1[i,j]) - as.numeric(df2[i,j])
    }
  }
  return(dfres)
}

# subtract 2 data frames - rtn fewer cols
# df2 from df1
sub_df_av_pl <- function(df1, df2){
  
  nc <- ncol(df1)
  ln <- nrow(df1)
  dfres <- df1
  for(i in 1:ln){
    for(j in 2:nc){
      dfres[i,j] <- as.numeric(df1[i,j]) - as.numeric(df2[i,j])
    }
  }
  dfres <- dfres[,c(1,7,10)]
  colnames(dfres) <- c('Mkt','Diff in Mean Long PL','Diff in Mean Short PL')
  return(dfres)
}


# -----------------------------------------------------------------
# ------------ CHAPTER 4 -----------------------------------------
# -----------------------------------------------------------------

# ------------ Follow Previous -------------
run_NaiveReversePrev <- function(fil,SLoss, nm){
  df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))
  for(i in 1:length(fil)){
    Dax <- read.csv(fil[i],stringsAsFactors=F)
    a <- NaiveReversePrev(Dax, SLoss, nm[i])
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-1,]
  return(df10)
}

# -----------------------------------------------------------------
# ------------ CHAPTER 5 -----------------------------------------
# -----------------------------------------------------------------
#  ------ Arima Ann Predicting Up/Dn - Categorical -----------------
# a. Categorical
ts_4_fnc_ar <- function(fil,SLoss,nm){
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i],stringsAsFactors=F)
    Mkt_p <- Mkt[,c(1,2,3,4,5)]
    Mkt_p$pred <- Mkt$pred
    colnames(Mkt_p) <- c("Date","Open", "High","Low","Close","pred")
    a <- ts_4(Mkt_p, SLoss,nm[i])
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-c(1),]
  return(df10)
}


# -------------------------------------------------
#  ------ Arima Ann Predicting Up/Dn - 01 ---------
ts_3_fnc_ar <- function(fil,nm,ts1){
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i],stringsAsFactors=F)
    Mkt_p <- Mkt[,c(1,2,3,4,5,18)]
    colnames(Mkt_p) <- c("Date","Open", "High","Low","Close","p")
    a <- ts_3(Mkt_p, 0, nm[i])
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-c(1),]
  return(df10)
}

# bit of fiddling for ANN
ts_3a_fnc_ar <- function(fil,nm,ts1){
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i],stringsAsFactors=F)
    Mkt_p <- Mkt[,c(1,2,3,4,5,18)]
    colnames(Mkt_p) <- c("Date","Open", "High","Low","Close","p")
    a <- ts_3a(Mkt_p, 0, nm[i])
    df10 <- rbind(df10, a)
  }
  df.name <- names(a)
  names(df10) <- df.name
  df10 <- df10[-c(1),]
  return(df10)
}