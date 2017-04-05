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

get_vector <- function(myresults, attribute) {
  x <- c(tail(myresults[[attribute]][myresults$FOLD == "fold0"], n=1),
         tail(myresults[[attribute]][myresults$FOLD == "fold1"], n=1),
         tail(myresults[[attribute]][myresults$FOLD == "fold2"], n=1),
         tail(myresults[[attribute]][myresults$FOLD == "fold3"], n=1),
         tail(myresults[[attribute]][myresults$FOLD == "fold4"], n=1),
         tail(myresults[[attribute]][myresults$FOLD == "fold5"], n=1),
         tail(myresults[[attribute]][myresults$FOLD == "fold6"], n=1),
         tail(myresults[[attribute]][myresults$FOLD == "fold7"], n=1),
         tail(myresults[[attribute]][myresults$FOLD == "fold8"], n=1),
         tail(myresults[[attribute]][myresults$FOLD == "fold9"], n=1))
  if(attribute == "BEST_VALIDATION") {
    return(x/5330)
  } else if (attribute == "TIME") {
    #return(x)
    return(((x/1000)/60)/60)
  } else {
    return(x) 
  }
}

get_stats <- function(myVector) {
  theMedian <- median(myVector)
  theMin <- min(myVector)
  theMax <- max(myVector)
  theMean <- mean(myVector)
  theSTD <- sd(myVector)
  cat(sprintf("Min: %f, Max: %f, Median: %f, Mean: %f, STDev: %f",
              theMin, theMax, theMedian, theMean, theSTD))
}

add_worst_as_column <- function(myresults, fitness) {
 y <- get_vector(myresults, "BEST_F")
 theMin <- min(y)
 theMax <- max(y)
 if (fitness == "facc") {
   myresults$WorstF <- theMax
 } else {
   myresults$WorstF <- theMin
 }
}

create_dataframe <- function(myresults, experimentName, attribute) {
  BEST_VALUES <- get_vector(myresults, attribute)
  CONF <- c(experimentName,experimentName,
            experimentName,experimentName,
            experimentName,experimentName,
            experimentName,experimentName,
            experimentName,experimentName)
  theDF <- data.frame(BEST_VALUES, CONF)
  return(theDF)
}

treeIndAlpha0Data <- read_data("./tree_ind_alpha_0")
treeIndAlpha0Data$CONF <- "Pitt_FCS_α0"
treeIndAlpha05Data <- read_data("./tree_ind_alpha_0.5")
treeIndAlpha05Data$CONF <- "Pit_FCS_α0.5"
treeIndAlpha1Data <- read_data("./tree_ind_alpha_1")
treeIndAlpha1Data$CONF <- "Pitt_FCS_α1"
treeIndCovData <- read_data("./tree_ind_coverage")
treeIndCovData$CONF <- "Pitt_FAcc"

listIndCovAllowData <- read_data("./list_ind_coverage/150gen GRANTED")
listIndCovAllowData$CONF <- "Mich_FAcc_Allow"
listIndCovDenyData <- read_data("./list_ind_coverage/150gen STRONGDENY")
listIndCovDenyData$CONF <- "Mich_FAcc_Deny"
listIndAlpha0AllowData <- read_data("./list_ind_alpha_0/150gen GRANTED")
listIndAlpha0AllowData$CONF <- "Mich_FCS_α0_Allow"
listIndAlpha0DenyData <- read_data("./list_ind_alpha_0/150gen STRONGDENY")
listIndAlpha0DenyData$CONF <- "Mich_FCS_α0_Deny"
listIndAlpha05AllowData <- read_data("./list_ind_alpha_0.5/150gen GRANTED")
listIndAlpha05AllowData$CONF <- "Mich_FCS_α0.5_Allow"
listIndAlpha05DenyData <- read_data("./list_ind_alpha_0.5/150gen STRONGDENY")
listIndAlpha05DenyData$CONF <- "Mich_FCS_α0.5_Deny"
listIndAlpha1AllowData <- read_data("./list_ind_alpha_1/150gen GRANTED")
listIndAlpha1AllowData$CONF <- "Mich_FCS_α1_Allow"
listIndAlpha1DenyData <- read_data("./list_ind_alpha_1/150gen STRONGDENY")
listIndAlpha1DenyData$CONF <- "Mich_FCS_α1_Deny"

treeIndALL <- rbind(treeIndCovData, treeIndAlpha0Data, treeIndAlpha05Data, treeIndAlpha1Data)
listIndALL <- rbind(listIndCovAllowData, listIndCovDenyData, listIndAlpha0AllowData, listIndAlpha0DenyData,
                    listIndAlpha05AllowData, listIndAlpha05DenyData, listIndAlpha1AllowData, listIndAlpha1DenyData)

bestFTreeCov <- create_dataframe(treeIndCovData, treeIndCovData$CONF[1], "BEST_F")
bestFTreeFCS <- rbind(create_dataframe(treeIndAlpha0Data, treeIndAlpha0Data$CONF[1], "BEST_F"),
                      create_dataframe(treeIndAlpha05Data, treeIndAlpha05Data$CONF[1], "BEST_F"),
                      create_dataframe(treeIndAlpha1Data, treeIndAlpha1Data$CONF[1], "BEST_F"))
bestFListCov <- rbind(create_dataframe(listIndCovAllowData, listIndCovAllowData$CONF[1], "BEST_F"),
                      create_dataframe(listIndCovDenyData, listIndCovDenyData$CONF[1], "BEST_F"))
bestFListFCS <- rbind(create_dataframe(listIndAlpha0AllowData, listIndAlpha0AllowData$CONF[1], "BEST_F"),
                      create_dataframe(listIndAlpha05AllowData, listIndAlpha05AllowData$CONF[1], "BEST_F"),
                      create_dataframe(listIndAlpha1AllowData, listIndAlpha1AllowData$CONF[1], "BEST_F"),
                      create_dataframe(listIndAlpha0DenyData, listIndAlpha0DenyData$CONF[1], "BEST_F"),
                      create_dataframe(listIndAlpha05DenyData, listIndAlpha05DenyData$CONF[1], "BEST_F"),
                      create_dataframe(listIndAlpha1DenyData, listIndAlpha1DenyData$CONF[1], "BEST_F"))
bestFListCovAllow <- create_dataframe(listIndCovAllowData, listIndCovAllowData$CONF[1], "BEST_F")
bestFListFCSAllow <- rbind(create_dataframe(listIndAlpha0AllowData, listIndAlpha0AllowData$CONF[1], "BEST_F"),
                      create_dataframe(listIndAlpha05AllowData, listIndAlpha05AllowData$CONF[1], "BEST_F"),
                      create_dataframe(listIndAlpha1AllowData, listIndAlpha1AllowData$CONF[1], "BEST_F"))
bestFListCovDeny <- create_dataframe(listIndCovDenyData, listIndCovDenyData$CONF[1], "BEST_F")
bestFListFCSDeny <- rbind(create_dataframe(listIndAlpha0DenyData, listIndAlpha0DenyData$CONF[1], "BEST_F"),
                      create_dataframe(listIndAlpha05DenyData, listIndAlpha05DenyData$CONF[1], "BEST_F"),
                      create_dataframe(listIndAlpha1DenyData, listIndAlpha1DenyData$CONF[1], "BEST_F"))

bestVTreeCov <- create_dataframe(treeIndCovData, treeIndCovData$CONF[1], "BEST_VALIDATION")
bestVTreeFCS <- rbind(create_dataframe(treeIndAlpha0Data, treeIndAlpha0Data$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(treeIndAlpha05Data, treeIndAlpha05Data$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(treeIndAlpha1Data, treeIndAlpha1Data$CONF[1], "BEST_VALIDATION"))
bestVListCov <- rbind(create_dataframe(listIndCovAllowData, listIndCovAllowData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndCovDenyData, listIndCovDenyData$CONF[1], "BEST_VALIDATION"))
bestVListFCS <- rbind(create_dataframe(listIndAlpha0AllowData, listIndAlpha0AllowData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndAlpha05AllowData, listIndAlpha05AllowData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndAlpha1AllowData, listIndAlpha1AllowData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndAlpha0DenyData, listIndAlpha0DenyData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndAlpha05DenyData, listIndAlpha05DenyData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndAlpha1DenyData, listIndAlpha1DenyData$CONF[1], "BEST_VALIDATION"))
bestVListCovAllow <- create_dataframe(listIndCovAllowData, listIndCovAllowData$CONF[1], "BEST_VALIDATION")
bestVListFCSAllow <- rbind(create_dataframe(listIndAlpha0AllowData, listIndAlpha0AllowData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndAlpha05AllowData, listIndAlpha05AllowData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndAlpha1AllowData, listIndAlpha1AllowData$CONF[1], "BEST_VALIDATION"))
bestVListCovDeny <- create_dataframe(listIndCovDenyData, listIndCovDenyData$CONF[1], "BEST_VALIDATION")
bestVListFCSDeny <- rbind(create_dataframe(listIndAlpha0DenyData, listIndAlpha0DenyData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndAlpha05DenyData, listIndAlpha05DenyData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndAlpha1DenyData, listIndAlpha1DenyData$CONF[1], "BEST_VALIDATION"))

#latexTable <- rbind(bestFTreeCov, bestFTreeFCS, bestFListCov, bestFListFCS)
#print(xtable(latexTable, digits = c(0, 7, 0)), type = "html")

bestFTreeCov$CONF <- as.factor(bestFTreeCov$CONF)
bestVTreeCov$CONF <- as.factor(bestVTreeCov$CONF)
bestFListCovAllow$CONF <- as.factor(bestFListCovAllow$CONF)
bestFListCovDeny$CONF <- as.factor(bestFListCovDeny$CONF)
bestVListCovAllow$CONF <- as.factor(bestVListCovAllow$CONF)
bestVListCovDeny$CONF <- as.factor(bestVListCovDeny$CONF)

kruskal.test(bestFTreeFCS$BEST_VALUES ~ bestFTreeFCS$CONF, data = bestFTreeFCS)
pairwise.wilcox.test(bestFTreeFCS$BEST_VALUES, bestFTreeFCS$CONF, p.adjust.method = "bonferroni")
pairwise.wilcox.test(bestFTreeFCS$BEST_VALUES, bestFTreeFCS$CONF, p.adjust.method = "holm")
kruskal.test(bestVTreeFCS$BEST_VALUES ~ bestVTreeFCS$CONF, data = bestVTreeFCS)
friedman.test(bestFTreeFCS$BEST_VALUES, bestFTreeFCS$CONF)

kruskal.test(bestFListFCSAllow$BEST_VALUES ~ bestFListFCSAllow$CONF, data = bestFListFCSAllow)
pairwise.wilcox.test(bestFListFCSAllow$BEST_VALUES, bestFListFCSAllow$CONF, p.adjust.method = "holm")
kruskal.test(bestFListFCSDeny$BEST_VALUES ~ bestFListFCSDeny$CONF, data = bestFListFCSDeny)
pairwise.wilcox.test(bestFListFCSDeny$BEST_VALUES, bestFListFCSDeny$CONF, p.adjust.method = "holm")
kruskal.test(bestVListFCSAllow$BEST_VALUES ~ bestVListFCSAllow$CONF, data = bestVListFCSAllow)
kruskal.test(bestVListFCSDeny$BEST_VALUES ~ bestVListFCSDeny$CONF, data = bestVListFCSDeny)

# --------
# Stats
# --------

get_stats(get_vector(treeIndCovData, "BEST_F"))
get_stats(get_vector(treeIndAlpha0Data, "BEST_F"))
get_stats(get_vector(treeIndAlpha05Data, "BEST_F"))
get_stats(get_vector(treeIndAlpha1Data, "BEST_F"))

get_stats(get_vector(listIndCovAllowData, "BEST_F"))
get_stats(get_vector(listIndCovDenyData, "BEST_F"))
get_stats(get_vector(listIndAlpha0AllowData, "BEST_F"))
get_stats(get_vector(listIndAlpha0DenyData, "BEST_F"))
get_stats(get_vector(listIndAlpha05AllowData, "BEST_F"))
get_stats(get_vector(listIndAlpha05DenyData, "BEST_F"))
get_stats(get_vector(listIndAlpha1AllowData, "BEST_F"))
get_stats(get_vector(listIndAlpha1DenyData, "BEST_F"))

get_stats(get_vector(treeIndCovData, "BEST_VALIDATION"))
get_stats(get_vector(treeIndAlpha0Data, "BEST_VALIDATION"))
get_stats(get_vector(treeIndAlpha05Data, "BEST_VALIDATION"))
get_stats(get_vector(treeIndAlpha1Data, "BEST_VALIDATION"))

get_stats(get_vector(listIndCovAllowData, "BEST_VALIDATION"))
get_stats(get_vector(listIndCovDenyData, "BEST_VALIDATION"))
get_stats(get_vector(listIndAlpha0AllowData, "BEST_VALIDATION"))
get_stats(get_vector(listIndAlpha0DenyData, "BEST_VALIDATION"))
get_stats(get_vector(listIndAlpha05AllowData, "BEST_VALIDATION"))
get_stats(get_vector(listIndAlpha05DenyData, "BEST_VALIDATION"))
get_stats(get_vector(listIndAlpha1AllowData, "BEST_VALIDATION"))
get_stats(get_vector(listIndAlpha1DenyData, "BEST_VALIDATION"))

get_stats(get_vector(treeIndCovData, "TIME"))
get_stats(get_vector(treeIndAlpha0Data, "TIME"))
get_stats(get_vector(treeIndAlpha05Data, "TIME"))
get_stats(get_vector(treeIndAlpha1Data, "TIME"))

get_stats(get_vector(listIndCovAllowData, "TIME"))
get_stats(get_vector(listIndCovDenyData, "TIME"))
get_stats(get_vector(listIndAlpha0AllowData, "TIME"))
get_stats(get_vector(listIndAlpha0DenyData, "TIME"))
get_stats(get_vector(listIndAlpha05AllowData, "TIME"))
get_stats(get_vector(listIndAlpha05DenyData, "TIME"))
get_stats(get_vector(listIndAlpha1AllowData, "TIME"))
get_stats(get_vector(listIndAlpha1DenyData, "TIME"))

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

treeBoxplotTime <- ggplot(treeIndALL, aes(treeIndALL$CONF, treeIndALL$TIME)) + stat_boxplot()
listBoxplotTime <- ggplot(listIndALL, aes(listIndALL$CONF, listIndALL$TIME)) + stat_boxplot()
grid.arrange(treeBoxplotTime, listBoxplotTime, ncol = 2)

treeIndALL <- treeIndALL[treeIndALL$CONF != "Pittsburgh_FAcc",]
treeBoxplotWF <- ggplot(treeIndALL, aes(treeIndALL$CONF, treeIndALL$BEST_F)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_log10()
listIndALL <- listIndALL[listIndALL$CONF != "Michigan_FAcc_Allow",]
listIndALL <- listIndALL[listIndALL$CONF != "Michigan_FAcc_Deny",]
listBoxplotWF <- ggplot(listIndALL[listIndALL$CONF != "Michigan_FAcc_Allow" || listIndALL$CONF != "Michigan_FAcc_Deny",], aes(listIndALL$CONF, listIndALL$BEST_F)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_log10()
grid.arrange(treeBoxplotWF, listBoxplotWF, ncol = 2)

BtreeCov <- ggplot(bestFTreeCov, aes(bestFTreeCov$CONF, bestFTreeCov$BEST_F*100)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Experiment configuration") +
  ylab("% correctly classified instances (FAcc*100)") +
  ylim(0, 95)
BtreeFCS <- ggplot(bestFTreeFCS, aes(bestFTreeFCS$CONF, bestFTreeFCS$BEST_F)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Experiment configuration") +
  ylab("Best Fitness, FCS") +
  ylim(6000, 48000)
BlistCov <- ggplot(bestFListCov, aes(bestFListCov$CONF, bestFListCov$BEST_F*100)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Experiment configuration") +
  ylab("% correctly classified instances (FAcc*100)") +
  ylim(0, 95)
BlistFCS <- ggplot(bestFListFCS, aes(bestFListFCS$CONF, bestFListFCS$BEST_F)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Experiment configuration") +
  ylab("Best Fitness, FCS") +
  ylim(6000, 48000)

grid.arrange(BtreeCov, BlistCov, ncol = 2)
grid.arrange(BtreeFCS, BlistFCS, ncol = 2)

ggsave("../img/bestFTreeCov.pdf", plot = BtreeCov, units = "mm", width = 80, height = 100, scale = 1.5)
ggsave("../img/bestFTreeFCS.pdf", plot = BtreeFCS, units = "mm", width = 70, height = 80, scale = 1.5)
ggsave("../img/bestFListCov.pdf", plot = BlistCov, units = "mm", width = 80, height = 100, scale = 1.5)
ggsave("../img/bestFListFCS.pdf", plot = BlistFCS, units = "mm", width = 120, height = 120, scale = 2)
