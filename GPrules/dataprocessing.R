setwd('~/Github/2017-ESWA/GPrules/Results')

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

get_data_vector <- function(myresults) {
  x <- c(tail(myresults$BEST_F[myresults$FOLD == "fold0"], n=1),
         tail(myresults$BEST_F[myresults$FOLD == "fold1"], n=1),
         tail(myresults$BEST_F[myresults$FOLD == "fold2"], n=1),
         tail(myresults$BEST_F[myresults$FOLD == "fold3"], n=1),
         tail(myresults$BEST_F[myresults$FOLD == "fold4"], n=1),
         tail(myresults$BEST_F[myresults$FOLD == "fold5"], n=1),
         tail(myresults$BEST_F[myresults$FOLD == "fold6"], n=1),
         tail(myresults$BEST_F[myresults$FOLD == "fold7"], n=1),
         tail(myresults$BEST_F[myresults$FOLD == "fold8"], n=1),
         tail(myresults$BEST_F[myresults$FOLD == "fold9"], n=1))
  return(x)
}

get_stats <- function(myVector) {
  theMedian <- median(myVector)
  theMin <- min(myVector)
  theMax <- max(myVector)
  cat(sprintf("Min: %f, Max: %f, Median: %f", theMin, theMax, theMedian))
}

treeIndAlpha0Data <- read_data("./tree_ind_alpha_0")
treeIndAlpha05Data <- read_data("./tree_ind_alpha_0.5")
treeIndAlpha1Data <- read_data("./tree_ind_alpha_1")
treeIndCovData <- read_data("./tree_ind_coverage")

listIndCovAllowData <- read_data("./list_ind_coverage/150gen GRANTED")
listIndCovDenyData <- read_data("./list_ind_coverage/150gen STRONGDENY")
listIndAlpha05AllowData <- read_data("./list_ind_alpha_0.5/150gen GRANTED")
listIndAlpha05DenyData <- read_data("./list_ind_alpha_0.5/150gen STRONGDENY")
listIndAlpha0AllowData <- read_data("./list_ind_alpha_1/150gen GRANTED")
listIndAlpha0DenyData <- read_data("./list_ind_alpha_1/150gen STRONGDENY")
listIndAlpha1AllowData <- read_data("./list_ind_alpha_0/150gen GRANTED")
listIndAlpha1DenyData <- read_data("./list_ind_alpha_0/150gen STRONGDENY")

get_stats(get_data_vector(treeIndCovData))
get_stats(get_data_vector(treeIndAlpha0Data))
get_stats(get_data_vector(treeIndAlpha05Data))
get_stats(get_data_vector(treeIndAlpha1Data))

get_stats(get_data_vector(listIndCovAllowData))
get_stats(get_data_vector(listIndCovDenyData))
get_stats(get_data_vector(listIndAlpha0AllowData))
get_stats(get_data_vector(listIndAlpha0DenyData))
get_stats(get_data_vector(listIndAlpha05AllowData))
get_stats(get_data_vector(listIndAlpha05DenyData))
get_stats(get_data_vector(listIndAlpha1AllowData))
get_stats(get_data_vector(listIndAlpha1DenyData))

# ---------
# Graphs
# ---------

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

FlistallowCov <- ggplot(listIndCovAllowData, aes(x = listIndCovAllowData$IT, y = listIndCovAllowData$BEST_F)) +
  geom_line(aes(colour = factor(FOLD))) + stat_summary(geom = "line") + 
  xlab("Iterations (class = GRANTED)") +
  ylab("Best Fitness (Accuracy)") + theme(legend.position = "bottom")
theLegend <- get_legend(FlistallowCov)
FlistallowCov <- FlistallowCov + theme(legend.position = "none")
FlistdenyCov <- ggplot(listIndCovDenyData, aes(x = listIndCovDenyData$IT, y = listIndCovDenyData$BEST_F)) +
  geom_line(aes(colour = factor(FOLD))) + stat_summary(geom = "line") + 
  xlab("Iterations (class = STRONGDENY)") +
  ylab("Best Fitness (Accuracy)") + theme(legend.position = "none")

grid.arrange(FlistallowCov, FlistdenyCov, ncol = 2)
grid.arrange(FlistallowCov, FlistdenyCov, theLegend, ncol = 2, nrow = 2, layout_matrix = cbind(c(1, 3), c(2, 3)))

FlistallowAlpha0 <- ggplot(listIndAlpha0AllowData, aes(x = listIndAlpha0AllowData$IT, y = listIndAlpha0AllowData$BEST_F/47966)) +
  geom_line(aes(colour = factor(FOLD))) + stat_summary(geom = "line")+
  xlab("Iterations (α = 0) (class = GRANTED)") +
  ylab("Best Fitness")
theLegend <- get_legend(FlistallowAlpha0)
FlistallowAlpha0 <- FlistallowAlpha0 + theme(legend.position = "none")
FlistdenyAlpha0 <- ggplot(listIndAlpha0DenyData, aes(x = listIndAlpha0DenyData$IT, y = listIndAlpha0DenyData$BEST_F/47966)) +
  geom_line(aes(colour = factor(FOLD))) + stat_summary(geom = "line")+
  xlab("Iterations (α = 0) (class = STRONGDENY)") +
  ylab("Best Fitness") + theme(legend.position = "none")
FlistallowAlpha05 <- ggplot(listIndAlpha05AllowData, aes(x = listIndAlpha05AllowData$IT, y = listIndAlpha05AllowData$BEST_F/47966)) +
  geom_line(aes(colour = factor(FOLD))) + stat_summary(geom = "line")+
  xlab("Iterations (α = 0.5) (class = GRANTED)") +
  ylab("Best Fitness") + theme(legend.position = "none")
FlistdenyAlpha05 <- ggplot(listIndAlpha05DenyData, aes(x = listIndAlpha05DenyData$IT, y = listIndAlpha05DenyData$BEST_F/47966)) +
  geom_line(aes(colour = factor(FOLD))) + stat_summary(geom = "line")+
  xlab("Iterations (α = 0.5) (class = STRONGDENY)") +
  ylab("Best Fitness") + theme(legend.position = "none")
FlistallowAlpha1 <- ggplot(listIndAlpha1AllowData, aes(x = listIndAlpha1AllowData$IT, y = listIndAlpha1AllowData$BEST_F/47966)) +
  geom_line(aes(colour = factor(FOLD))) + stat_summary(geom = "line")+
  xlab("Iterations (α = 1) (class = GRANTED)") +
  ylab("Best Fitness") + theme(legend.position = "none")
FlistdenyAlpha1 <- ggplot(listIndAlpha1DenyData, aes(x = listIndAlpha1DenyData$IT, y = listIndAlpha1DenyData$BEST_F/47966)) +
  geom_line(aes(colour = factor(FOLD))) + stat_summary(geom = "line")+
  xlab("Iterations (α = 1) (class = STRONGDENY)") +
  ylab("Best Fitness") + theme(legend.position = "none")

grid.arrange(FlistallowAlpha0, FlistdenyAlpha0, FlistallowAlpha05, FlistdenyAlpha05, FlistallowAlpha1, FlistdenyAlpha1, ncol = 2, nrow = 3)
grid.arrange(FlistallowAlpha0, FlistdenyAlpha0, FlistallowAlpha05, FlistdenyAlpha05, FlistallowAlpha1, FlistdenyAlpha1, theLegend, ncol = 3, nrow = 3, layout_matrix = rbind(c(1, 2, 7), c(3, 4, 7), c(5, 6, 7)))

# ---------
# Boxplots
# ---------

aBoxplot <- ggplot(treeIndCovData, aes(treeIndCovData$FOLD, treeIndCovData$TIME)) + stat_boxplot() + geom_errorbar(aes())
print(aBoxplot)