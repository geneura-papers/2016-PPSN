setwd('~/R/')

require(ggplot2)
require(dplyr)
require(reshape2)
require(xtable)
library(gridExtra)
library(grid)
library(lattice)

read_data <- function(directory) {
  allData <- do.call(rbind, lapply(list.files(path = directory, full.names = TRUE), read.table, sep = " "))
  return(allData)
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

treeIndAlpha05Data <- read_data("./lanzar1/CSVs")
treeIndAlpha0Data <- read_data("./lanzar2/CSVs")
treeIndAlpha1Data <- read_data("./lanzar3/CSVs")
treeIndCovData <- read_data("./lanzar4/CSVs")

listIndCovAllowData <- read_data("./lanzar5/150gen GRANTED/CSVs")
listIndCovDenyData <- read_data("./lanzar5/150gen STRONGDENY/CSVs")
listIndAlpha05AllowData <- read_data("./lanzar6/150gen GRANTED/CSVs")
listIndAlpha05DenyData <- read_data("./lanzar6/150gen STRONGDENY/CSVs")
listIndAlpha0AllowData <- read_data("./lanzar7/150gen GRANTED/CSVs")
listIndAlpha0DenyData <- read_data("./lanzar7/150gen STRONGDENY/CSVs")
listIndAlpha1AllowData <- read_data("./lanzar8/150gen GRANTED/CSVs")
listIndAlpha1DenyData <- read_data("./lanzar8/150gen STRONGDENY/CSVs")

Ftreea0 <- ggplot(treeIndAlpha0Data, aes(x = treeIndAlpha0Data$IT, y =BEST_F/47966)) +
  geom_line(aes(colour = factor(FOLD))) +
  stat_summary(geom = "line") + scale_y_continuous(limits = c(0.1,0.5)) +
  xlab("Iterations (α = 0)") +
  ylab("Best Fitness") + theme(legend.position = "bottom")
#theLegend <- get_legend(Ftreea0)
Ftreea0 <- Fa0 + theme(legend.position = "none")
Ftreea05 <- ggplot(treeIndAlpha05Data, aes(x = treeIndAlpha05Data$IT, y = treeIndAlpha05Data$BEST_F/47966)) +
  geom_line(aes(colour = factor(FOLD))) +
  stat_summary(geom = "line") + scale_y_continuous(limits = c(0.1,0.5)) +
  xlab("Iterations (α = 0.5)") +
  ylab("Best Fitness") + theme(legend.position = "none")
Ftreea1 <- ggplot(treeIndAlpha1Data, aes(x = treeIndAlpha1Data$IT, y = treeIndAlpha1Data$BEST_F/47966)) +
  geom_line(aes(colour = factor(FOLD))) +
  stat_summary(geom = "line") + scale_y_continuous(limits = c(0.1,0.5)) +
  xlab("Iterations (α = 1)") +
  ylab("Best Fitness") + theme(legend.position = "none")

#grid.arrange(Ftreea0, Ftreea05, Ftreea1, theLegend, ncol = 3, nrow = 2, layout_matrix = cbind(c(1, 4), c(2, 4), c(3, 4)))
grid.arrange(Ftreea0, Ftreea05, Ftreea1, ncol = 3)

FtreeCov <- ggplot(treeIndCovData, aes(x = treeIndCovData$IT, y =BEST_F)) +
  geom_line(aes(colour = factor(FOLD))) +
  stat_summary(geom = "line") + 
  xlab("Iterations") +
  ylab("Best Fitness (Accuracy)") + theme(legend.position = "bottom")
print(FtreeCov)

Fallow <- ggplot(listIndCovAllowData, aes(x = listIndCovAllowData$IT, y = listIndCovAllowData$BEST_F/47966)) +
  geom_point(aes(colour = factor(FOLD))) + stat_summary()  + theme(legend.position = "bottom")
Fdeny <- ggplot(listIndCovDenyData, aes(x = listIndCovDenyData$IT, y = listIndCovDenyData$BEST_F/47966)) +
  geom_point(aes(colour = factor(FOLD))) + stat_summary() + theme(legend.position = "bottom")

grid.arrange(Fallow, Fdeny, ncol = 2)

Fallow0 <- ggplot(listIndCovAllowData[listIndCovAllowData$FOLD == "fold0",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny0 <- ggplot(listIndCovDenyData[listIndCovDenyData$FOLD == "fold0",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow1 <- ggplot(listIndCovAllowData[listIndCovAllowData$FOLD == "fold1",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny1 <- ggplot(listIndCovDenyData[listIndCovDenyData$FOLD == "fold1",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow2 <- ggplot(listIndCovAllowData[listIndCovAllowData$FOLD == "fold2",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny2 <- ggplot(listIndCovDenyData[listIndCovDenyData$FOLD == "fold2",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow3 <- ggplot(listIndCovAllowData[listIndCovAllowData$FOLD == "fold3",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny3 <- ggplot(listIndCovDenyData[listIndCovDenyData$FOLD == "fold3",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow4 <- ggplot(listIndCovAllowData[listIndCovAllowData$FOLD == "fold4",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny4 <- ggplot(listIndCovDenyData[listIndCovDenyData$FOLD == "fold4",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow5 <- ggplot(listIndCovAllowData[listIndCovAllowData$FOLD == "fold5",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny5 <- ggplot(listIndCovDenyData[listIndCovDenyData$FOLD == "fold5",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow6 <- ggplot(listIndCovAllowData[listIndCovAllowData$FOLD == "fold6",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny6 <- ggplot(listIndCovDenyData[listIndCovDenyData$FOLD == "fold6",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow7 <- ggplot(listIndCovAllowData[listIndCovAllowData$FOLD == "fold7",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny7 <- ggplot(listIndCovDenyData[listIndCovDenyData$FOLD == "fold7",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow8 <- ggplot(listIndCovAllowData[listIndCovAllowData$FOLD == "fold8",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny8 <- ggplot(listIndCovDenyData[listIndCovDenyData$FOLD == "fold8",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fallow9 <- ggplot(listIndCovAllowData[listIndCovAllowData$FOLD == "fold9",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")
Fdeny9 <- ggplot(listIndCovDenyData[listIndCovDenyData$FOLD == "fold9",], aes(x = IT, y = BEST_F/47966)) +
  geom_point() + theme(legend.position = "bottom")

grid.arrange(Fallow0, Fdeny0, Fallow1, Fdeny1, Fallow2, Fdeny2, Fallow3, Fdeny3, Fallow4, Fdeny4, Fallow5, Fdeny5, Fallow6, Fdeny6, Fallow7, Fdeny7, Fallow8, Fdeny8, Fallow9, Fdeny9, ncol = 2, nrow = 10)
