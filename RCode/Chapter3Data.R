library(stargazer)
setwd("D:/Allan/DropBox/MSc/Dissertation/Thesis/RCode")

Dax <- read.csv("..//Data//Dax_2000_d.csv",header=TRUE)

# --------------------- Columns -----------------------
Dax$prevHigh <- c( NA, Dax$High[ - length(Dax$High) ] )
Dax$prevLow <- c( NA, Dax$Low[ - length(Dax$Low) ] )
Dax$OH <- Dax$High - Dax$Open
Dax$OL <- Dax$Open - Dax$Low
Dax$absOC <- abs(Dax$Open - Dax$Close)

# --------------------- Open stats -----------------------
# How many open between Prev HL
length(Dax$Open[Dax$Open<Dax$prevHigh & Dax$Open>Dax$prevLow]) / length(Dax$Open) * 100

# ---------------------- Print Results -------------------
tab <- read.table(file = "clipboard", header=TRUE)
tab <- read.clipboard(header = TRUE)
tab <- readClipboard()

