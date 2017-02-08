setwd('~/R/')

require(ggplot2)
require(dplyr)
require(reshape2)
require(xtable)

read_data <- function(directory) {
  
  i <- 0
  for(inFile in list.files(path = directory, full.names = TRUE)) {
    dataInFile <- read.table(file = inFile, header = TRUE, sep = ";")
    dataInFile$FOLD <- paste("fold", i, sep = "")
    write.table(dataInFile, file = inFile)
    i <- i+1
  }
  
  allData <- do.call(rbind, lapply(list.files(path = directory, full.names = TRUE), read.table, sep = " "))
  return(allData)
}

alpha0Data <- read_data("./CSVs/a = 0")
alpha05Data <- read_data("./CSVs/a = 0.5")
alpha1Data <- read_data("./CSVs/a = 1")

theFitness <- ggplot(alpha0Data, aes(x = alpha0Data$IT, y = alpha0Data$BEST_F)) +
  geom_point(aes(colour = factor(FOLD))) + stat_summary()
print(theFitness)
