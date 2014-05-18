library(TTR)

setwd("D://Allan//DropBox//MSc//Dissertation//Thesis//RCode")
source("Utils.R")
source("BaseSystem1SMA_MRevision.R")

mkt <- read.csv("../Data//Dax_2000_d.csv")

#ftse <- read.csv("D://Allan//DropBox//MSc//Dissertation//Thesis//RCode//ftse.csv")
#dax <- read.csv("D://Allan//DropBox//MSc//Dissertation//Thesis//RCode//dax.csv")
BaseSystem1SMA_Rev(mkt, 10, -50, "Dax")



# combo <- merge(dax, ftse, by = "Date")
# head(combo)
# write.csv(combo, "D://Allan//DropBox//MSc//Dissertation//Thesis//RCode//ftsedax.csv")
