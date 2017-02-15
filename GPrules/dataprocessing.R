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

allowData <- read_data("./CSVs/allow")
denyData <- read_data("./CSVs/deny")

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

Fallow <- ggplot(allowData, aes(x = allowData$IT, y = allowData$BEST_F/47966)) +
  geom_point(aes(colour = factor(FOLD))) + stat_summary()  + theme(legend.position = "bottom")
Fdeny <- ggplot(denyData, aes(x = denyData$IT, y = denyData$BEST_F/47966)) +
  geom_point(aes(colour = factor(FOLD))) + stat_summary() + theme(legend.position = "bottom")

grid.arrange(Fallow, Fdeny, ncol = 2)

Fallow0 <- ggplot(allowData[allowData$FOLD == "fold0",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny0 <- ggplot(denyData[denyData$FOLD == "fold0",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow1 <- ggplot(allowData[allowData$FOLD == "fold1",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny1 <- ggplot(denyData[denyData$FOLD == "fold1",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow2 <- ggplot(allowData[allowData$FOLD == "fold2",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny2 <- ggplot(denyData[denyData$FOLD == "fold2",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow3 <- ggplot(allowData[allowData$FOLD == "fold3",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny3 <- ggplot(denyData[denyData$FOLD == "fold3",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow4 <- ggplot(allowData[allowData$FOLD == "fold4",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny4 <- ggplot(denyData[denyData$FOLD == "fold4",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow5 <- ggplot(allowData[allowData$FOLD == "fold5",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny5 <- ggplot(denyData[denyData$FOLD == "fold5",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow6 <- ggplot(allowData[allowData$FOLD == "fold6",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny6 <- ggplot(denyData[denyData$FOLD == "fold6",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow7 <- ggplot(allowData[allowData$FOLD == "fold7",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny7 <- ggplot(denyData[denyData$FOLD == "fold7",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow8 <- ggplot(allowData[allowData$FOLD == "fold8",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny8 <- ggplot(denyData[denyData$FOLD == "fold8",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow9 <- ggplot(allowData[allowData$FOLD == "fold9",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny9 <- ggplot(denyData[denyData$FOLD == "fold9",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")

grid.arrange(Fallow0, Fdeny0, Fallow1, Fdeny1, Fallow2, Fdeny2, Fallow3, Fdeny3, Fallow4, Fdeny4, Fallow5, Fdeny5, Fallow6, Fdeny6, Fallow7, Fdeny7, Fallow8, Fdeny8, Fallow9, Fdeny9, ncol = 2, nrow = 10)
