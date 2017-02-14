setwd('~/R/')

require(ggplot2)
require(dplyr)
require(reshape2)
require(xtable)
library(gridExtra)
library(grid)
library(lattice)

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

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

alpha0Data <- read_data("./CSVs/a = 0")
alpha05Data <- read_data("./CSVs/a = 0.5")
alpha1Data <- read_data("./CSVs/a = 1")

Fa0 <- ggplot(alpha0Data, aes(x = alpha0Data$IT, y =BEST_F/47966)) +
  geom_point(aes(colour = factor(FOLD))) + stat_summary() + scale_y_continuous(limits = c(0,0.5)) + theme(legend.position = "bottom")
#theLegend <- get_legend(Fa0)
Fa0 <- Fa0 + theme(legend.position = "none")
Fa05 <- ggplot(alpha05Data, aes(x = alpha05Data$IT, y = alpha05Data$BEST_F/47966)) +
  geom_point(aes(colour = factor(FOLD))) + stat_summary() + scale_y_continuous(limits = c(0,0.5))  + theme(legend.position = "none")
Fa1 <- ggplot(alpha1Data, aes(x = alpha1Data$IT, y = alpha1Data$BEST_F/47966)) +
  geom_point(aes(colour = factor(FOLD))) + stat_summary() + scale_y_continuous(limits = c(0,0.5))  + theme(legend.position = "none")

#grid.arrange(Fa0, Fa05, Fa1, theLegend, ncol = 3, nrow = 2, layout_matrix = cbind(c(1, 4), c(2, 4), c(3, 4)))
grid.arrange(Fa0, Fa05, Fa1, ncol = 3)

             