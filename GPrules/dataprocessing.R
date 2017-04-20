setwd('~/Github/2017-ESWA/GPrules/Results')

require(ggplot2)
require(dplyr)
require(reshape2)
require(xtable)
require(multcomp)

library(gridExtra)
library(grid)
library(lattice)
library(Cairo)

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
  if (attribute == "X") {
    x <- c(as.character(tail(myresults[[attribute]][myresults$FOLD == "fold0"], n=1)),
           as.character(tail(myresults[[attribute]][myresults$FOLD == "fold1"], n=1)),
           as.character(tail(myresults[[attribute]][myresults$FOLD == "fold2"], n=1)),
           as.character(tail(myresults[[attribute]][myresults$FOLD == "fold3"], n=1)),
           as.character(tail(myresults[[attribute]][myresults$FOLD == "fold4"], n=1)),
           as.character(tail(myresults[[attribute]][myresults$FOLD == "fold5"], n=1)),
           as.character(tail(myresults[[attribute]][myresults$FOLD == "fold6"], n=1)),
           as.character(tail(myresults[[attribute]][myresults$FOLD == "fold7"], n=1)),
           as.character(tail(myresults[[attribute]][myresults$FOLD == "fold8"], n=1)),
           as.character(tail(myresults[[attribute]][myresults$FOLD == "fold9"], n=1)))
  } else {
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
      return(x)
      #return(((x/1000)/60)/60)
    } else {
      return(x) 
    }
  }
  return(x)
}

get_vector2 <- function(myresults1, myresults2, attribute) {
  x1 <- c(tail(myresults1[[attribute]][myresults1$FOLD == "fold0"], n=1),
         tail(myresults1[[attribute]][myresults1$FOLD == "fold1"], n=1),
         tail(myresults1[[attribute]][myresults1$FOLD == "fold2"], n=1),
         tail(myresults1[[attribute]][myresults1$FOLD == "fold3"], n=1),
         tail(myresults1[[attribute]][myresults1$FOLD == "fold4"], n=1),
         tail(myresults1[[attribute]][myresults1$FOLD == "fold5"], n=1),
         tail(myresults1[[attribute]][myresults1$FOLD == "fold6"], n=1),
         tail(myresults1[[attribute]][myresults1$FOLD == "fold7"], n=1),
         tail(myresults1[[attribute]][myresults1$FOLD == "fold8"], n=1),
         tail(myresults1[[attribute]][myresults1$FOLD == "fold9"], n=1))
  x2 <- c(tail(myresults2[[attribute]][myresults2$FOLD == "fold0"], n=1),
          tail(myresults2[[attribute]][myresults2$FOLD == "fold1"], n=1),
          tail(myresults2[[attribute]][myresults2$FOLD == "fold2"], n=1),
          tail(myresults2[[attribute]][myresults2$FOLD == "fold3"], n=1),
          tail(myresults2[[attribute]][myresults2$FOLD == "fold4"], n=1),
          tail(myresults2[[attribute]][myresults2$FOLD == "fold5"], n=1),
          tail(myresults2[[attribute]][myresults2$FOLD == "fold6"], n=1),
          tail(myresults2[[attribute]][myresults2$FOLD == "fold7"], n=1),
          tail(myresults2[[attribute]][myresults2$FOLD == "fold8"], n=1),
          tail(myresults2[[attribute]][myresults2$FOLD == "fold9"], n=1))
  x <- x1+x2
  if(attribute == "BEST_VALIDATION") {
    return(x/5330)
  } else if (attribute == "TIME") {
    return(x)
    #return(((x/1000)/60)/60)
  } else {
    return(x/47966) 
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

# Reading the CSVs

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

bestVTree <- rbind(create_dataframe(treeIndCovData, treeIndCovData$CONF[1], "BEST_VALIDATION"),
                   create_dataframe(treeIndAlpha0Data, treeIndAlpha0Data$CONF[1], "BEST_VALIDATION"),
                   create_dataframe(treeIndAlpha05Data, treeIndAlpha05Data$CONF[1], "BEST_VALIDATION"),
                   create_dataframe(treeIndAlpha1Data, treeIndAlpha1Data$CONF[1], "BEST_VALIDATION"))
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
bestVListAllow <- rbind(create_dataframe(listIndCovAllowData, listIndCovAllowData$CONF[1], "BEST_VALIDATION"),
                        create_dataframe(listIndAlpha0AllowData, listIndAlpha0AllowData$CONF[1], "BEST_VALIDATION"),
                        create_dataframe(listIndAlpha05AllowData, listIndAlpha05AllowData$CONF[1], "BEST_VALIDATION"),
                        create_dataframe(listIndAlpha1AllowData, listIndAlpha1AllowData$CONF[1], "BEST_VALIDATION"))
bestVListCovAllow <- create_dataframe(listIndCovAllowData, listIndCovAllowData$CONF[1], "BEST_VALIDATION")
bestVListFCSAllow <- rbind(create_dataframe(listIndAlpha0AllowData, listIndAlpha0AllowData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndAlpha05AllowData, listIndAlpha05AllowData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndAlpha1AllowData, listIndAlpha1AllowData$CONF[1], "BEST_VALIDATION"))
bestVListDeny <- rbind(create_dataframe(listIndCovDenyData, listIndCovDenyData$CONF[1], "BEST_VALIDATION"),
                       create_dataframe(listIndAlpha0DenyData, listIndAlpha0DenyData$CONF[1], "BEST_VALIDATION"),
                       create_dataframe(listIndAlpha05DenyData, listIndAlpha05DenyData$CONF[1], "BEST_VALIDATION"),
                       create_dataframe(listIndAlpha1DenyData, listIndAlpha1DenyData$CONF[1], "BEST_VALIDATION"))
bestVListCovDeny <- create_dataframe(listIndCovDenyData, listIndCovDenyData$CONF[1], "BEST_VALIDATION")
bestVListFCSDeny <- rbind(create_dataframe(listIndAlpha0DenyData, listIndAlpha0DenyData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndAlpha05DenyData, listIndAlpha05DenyData$CONF[1], "BEST_VALIDATION"),
                      create_dataframe(listIndAlpha1DenyData, listIndAlpha1DenyData$CONF[1], "BEST_VALIDATION"))

bestTTree <- rbind(create_dataframe(treeIndCovData, treeIndCovData$CONF[1], "TIME"),
                   create_dataframe(treeIndAlpha0Data, treeIndAlpha0Data$CONF[1], "TIME"),
                   create_dataframe(treeIndAlpha05Data, treeIndAlpha05Data$CONF[1], "TIME"),
                   create_dataframe(treeIndAlpha1Data, treeIndAlpha1Data$CONF[1], "TIME"))
bestTTreeCov <- create_dataframe(treeIndCovData, treeIndCovData$CONF[1], "TIME")
bestTTreeFCS <- rbind(create_dataframe(treeIndAlpha0Data, treeIndAlpha0Data$CONF[1], "TIME"),
                      create_dataframe(treeIndAlpha05Data, treeIndAlpha05Data$CONF[1], "TIME"),
                      create_dataframe(treeIndAlpha1Data, treeIndAlpha1Data$CONF[1], "TIME"))
bestTListCov <- rbind(create_dataframe(listIndCovAllowData, listIndCovAllowData$CONF[1], "TIME"),
                      create_dataframe(listIndCovDenyData, listIndCovDenyData$CONF[1], "TIME"))
bestTListFCS <- rbind(create_dataframe(listIndAlpha0AllowData, listIndAlpha0AllowData$CONF[1], "TIME"),
                      create_dataframe(listIndAlpha05AllowData, listIndAlpha05AllowData$CONF[1], "TIME"),
                      create_dataframe(listIndAlpha1AllowData, listIndAlpha1AllowData$CONF[1], "TIME"),
                      create_dataframe(listIndAlpha0DenyData, listIndAlpha0DenyData$CONF[1], "TIME"),
                      create_dataframe(listIndAlpha05DenyData, listIndAlpha05DenyData$CONF[1], "TIME"),
                      create_dataframe(listIndAlpha1DenyData, listIndAlpha1DenyData$CONF[1], "TIME"))
bestTListAllow <- rbind(create_dataframe(listIndCovAllowData, listIndCovAllowData$CONF[1], "TIME"),
                        create_dataframe(listIndAlpha0AllowData, listIndAlpha0AllowData$CONF[1], "TIME"),
                        create_dataframe(listIndAlpha05AllowData, listIndAlpha05AllowData$CONF[1], "TIME"),
                        create_dataframe(listIndAlpha1AllowData, listIndAlpha1AllowData$CONF[1], "TIME"))
bestTListCovAllow <- create_dataframe(listIndCovAllowData, listIndCovAllowData$CONF[1], "TIME")
bestTListFCSAllow <- rbind(create_dataframe(listIndAlpha0AllowData, listIndAlpha0AllowData$CONF[1], "TIME"),
                           create_dataframe(listIndAlpha05AllowData, listIndAlpha05AllowData$CONF[1], "TIME"),
                           create_dataframe(listIndAlpha1AllowData, listIndAlpha1AllowData$CONF[1], "TIME"))
bestTListDeny <- rbind(create_dataframe(listIndCovDenyData, listIndCovDenyData$CONF[1], "TIME"),
                       create_dataframe(listIndAlpha0DenyData, listIndAlpha0DenyData$CONF[1], "TIME"),
                       create_dataframe(listIndAlpha05DenyData, listIndAlpha05DenyData$CONF[1], "TIME"),
                       create_dataframe(listIndAlpha1DenyData, listIndAlpha1DenyData$CONF[1], "TIME"))
bestTListCovDeny <- create_dataframe(listIndCovDenyData, listIndCovDenyData$CONF[1], "TIME")
bestTListFCSDeny <- rbind(create_dataframe(listIndAlpha0DenyData, listIndAlpha0DenyData$CONF[1], "TIME"),
                          create_dataframe(listIndAlpha05DenyData, listIndAlpha05DenyData$CONF[1], "TIME"),
                          create_dataframe(listIndAlpha1DenyData, listIndAlpha1DenyData$CONF[1], "TIME"))

bestIndTreeCov <- create_dataframe(treeIndCovData, treeIndCovData$CONF[1], "X")
bestIndTreeFCS <- rbind(create_dataframe(treeIndAlpha0Data, treeIndAlpha0Data$CONF[1], "X"),
                      create_dataframe(treeIndAlpha05Data, treeIndAlpha05Data$CONF[1], "X"),
                      create_dataframe(treeIndAlpha1Data, treeIndAlpha1Data$CONF[1], "X"))
bestIndListCov <- rbind(create_dataframe(listIndCovAllowData, listIndCovAllowData$CONF[1], "X"),
                      create_dataframe(listIndCovDenyData, listIndCovDenyData$CONF[1], "X"))
bestIndListFCS <- rbind(create_dataframe(listIndAlpha0AllowData, listIndAlpha0AllowData$CONF[1], "X"),
                      create_dataframe(listIndAlpha05AllowData, listIndAlpha05AllowData$CONF[1], "X"),
                      create_dataframe(listIndAlpha1AllowData, listIndAlpha1AllowData$CONF[1], "X"),
                      create_dataframe(listIndAlpha0DenyData, listIndAlpha0DenyData$CONF[1], "X"),
                      create_dataframe(listIndAlpha05DenyData, listIndAlpha05DenyData$CONF[1], "X"),
                      create_dataframe(listIndAlpha1DenyData, listIndAlpha1DenyData$CONF[1], "X"))

latexTable <- rbind(bestIndTreeCov, bestIndTreeFCS, bestIndListCov, bestIndListFCS)
print(xtable(latexTable, digits = c(0, 7, 0)), type = "html")

# ---------
# FP + FN
# ---------

treeIndCovFP <- c(400, 413, 400, 388, 444, 380, 383, 358, 406, 418)
treeIndCovFN <- integer(10)
treeIndAlpha0FP <- c(148, 4133, 365, 240, 444, 217, 384, 385, 406, 415)
treeIndAlpha0FN <- c(121, 0, 1, 0, 0, 35, 1, 0, 0, 5)
treeIndAlpha05FP <- c(400, 413, 385, 379, 444, 380, 384, 385, 365, 419)
treeIndAlpha05FN <- c(0, 0, 5, 10, 0, 0, 0, 0, 23, 0)
treeIndAlpha1FP <- c(337, 413, 387, 389, 392, 377, 347, 299, 406, 419)
treeIndAlpha1FN <- integer(10)

listIndCovAllowFP <- c(70, 74, 121, 157, 219, 90, 97, 192, 92, 125)
listIndCovAllowFN <- integer(10)
listIndCovDenyFP <- integer(10)
listIndCovDenyFN <- c(1550, 1573, 1723, 1874, 899, 1699, 2337, 1731, 1131, 1841)
listIndAlpha0AllowFP <- c(69, 208, 135, 70, 177, 77, 150, 88, 80, 81)
listIndAlpha0AllowFN <- integer(10)
listIndAlpha0DenyFP <- integer(10)
listIndAlpha0DenyFN <- c(49, 21, 57, 21, 33, 22, 60, 8, 2, 10)
listIndAlpha05AllowFP <- c(152, 67, 44, 36, 109, 164, 163, 118, 80, 88)
listIndAlpha05AllowFN <- integer(10)
listIndAlpha05DenyFP <- integer(10)
listIndAlpha05DenyFN <- c(17, 111, 4, 45, 43, 59, 17, 16, 67, 9)
listIndAlpha1AllowFP <- c(165, 138, 162, 36, 136, 97, 124, 172, 178, 34)
listIndAlpha1AllowFN <- integer(10)
listIndAlpha1DenyFP <- integer(10)
listIndAlpha1DenyFN <- c(31, 1, 21, 5, 17, 91, 18, 15, 23, 0)

bestVTree <- data.frame(bestVTree, c(treeIndCovFP, treeIndAlpha0FP, treeIndAlpha05FP, treeIndAlpha1FP))
bestVTree <- data.frame(bestVTree, c(treeIndCovFN, treeIndAlpha0FN, treeIndAlpha05FN, treeIndAlpha1FN))
colnames(bestVTree) <- c("BEST_VALUES", "CONF", "FP", "FN")
bestVListAllow <- data.frame(bestVListAllow, c(listIndCovAllowFP, listIndAlpha0AllowFP, listIndAlpha05AllowFP, listIndAlpha1AllowFP))
bestVListAllow <- data.frame(bestVListAllow, c(listIndCovAllowFN,listIndAlpha0AllowFN, listIndAlpha05AllowFN, listIndAlpha1AllowFN))
colnames(bestVListAllow) <- c("BEST_VALUES", "CONF", "FP", "FN")
bestVListDeny <- data.frame(bestVListDeny, c(listIndCovDenyFP, listIndAlpha0DenyFP, listIndAlpha05DenyFP, listIndAlpha1DenyFP))
bestVListDeny <- data.frame(bestVListDeny, c(listIndCovDenyFN, listIndAlpha0DenyFN, listIndAlpha05DenyFN, listIndAlpha1DenyFN))
colnames(bestVListDeny) <- c("BEST_VALUES", "CONF", "FP", "FN")

# -------------------
# Statistical tests
# -------------------

#----------------------------------------- FITNESS

shapiro.test(bestFTreeFCS$BEST_VALUES)
shapiro.test(bestFListFCSAllow$BEST_VALUES)
shapiro.test(bestFListFCSDeny$BEST_VALUES)

# Besause not normal distribution:

kruskal.test(bestFTreeFCS$BEST_VALUES ~ bestFTreeFCS$CONF, data = bestFTreeFCS)
pairwise.wilcox.test(bestFTreeFCS$BEST_VALUES, bestFTreeFCS$CONF, p.adjust.method = "holm")
kruskal.test(bestFListFCSAllow$BEST_VALUES ~ bestFListFCSAllow$CONF, data = bestFListFCSAllow)         # ***
pairwise.wilcox.test(bestFListFCSAllow$BEST_VALUES, bestFListFCSAllow$CONF, p.adjust.method = "holm")  # ***
kruskal.test(bestFListFCSDeny$BEST_VALUES ~ bestFListFCSDeny$CONF, data = bestFListFCSDeny)
pairwise.wilcox.test(bestFListFCSDeny$BEST_VALUES, bestFListFCSDeny$CONF, p.adjust.method = "holm")

#----------------------------------------- FP + FN

shapiro.test(bestVTree$FP)
shapiro.test(bestVTree$FN)
shapiro.test(bestVListDeny$FN)

# Besause not normal distribution:

kruskal.test(bestVTree$FP ~ bestVTree$CONF, data = bestVTree)
pairwise.wilcox.test(bestVTree$FP, bestVTree$CONF, p.adjust.method = "holm")
kruskal.test(bestVTree$FN ~ bestVTree$CONF, data = bestVTreeFCS)
pairwise.wilcox.test(bestVTree$FN, bestVTree$CONF, p.adjust.method = "holm")
kruskal.test(bestVListDeny$FN ~ bestVListDeny$CONF, data = bestVListDeny)
pairwise.wilcox.test(bestVListDeny$FN, bestVListDeny$CONF, p.adjust.method = "holm")

shapiro.test(bestVListAllow$FP)
qqnorm(bestVListAllow$FP)

# Besause normal distribution:

anova(lm(bestVListAllow$FP ~ bestVListAllow$CONF, bestVListAllow))
TukeyHSD(aov(bestVListAllow$FP ~ bestVListAllow$CONF, bestVListAllow))

# Double check with Dunnett:

Group <- bestVListFCSAllow$CONF
Value <- bestVListFCSAllow$FP
data <- data.frame(Group, Value)
fit <- aov(Value ~ Group, data)
set.seed(20140123)
Dunnet <- glht(fit, linfct=mcp(Group="Dunnett"))
summary(Dunnet)

# Triple check with kruskal:

kruskal.test(bestVListFCSAllow$FP ~ bestVListFCSAllow$CONF, data = bestVListFCSAllow)
pairwise.wilcox.test(bestVListFCSAllow$FP, bestVListFCSAllow$CONF, p.adjust.method = "holm")

#----------------------------------------- VALIDATION

shapiro.test(bestVTree$BEST_VALUES)

# Besause not normal distribution:

kruskal.test(bestVTree$BEST_VALUES ~ bestVTree$CONF, data = bestVTree)
pairwise.wilcox.test(bestVTree$BEST_VALUES, bestVTree$CONF, p.adjust.method = "holm")

shapiro.test(bestVListAllow$BEST_VALUES)
qqnorm(bestVListAllow$BEST_VALUES)
shapiro.test(bestVListDeny$BEST_VALUES)
qqnorm(bestVListDeny$BEST_VALUES)

# Besause normal distribution:

anova(lm(bestVListAllow$BEST_VALUES ~ bestVListAllow$CONF, bestVListAllow))
TukeyHSD(aov(bestVListAllow$BEST_VALUES ~ bestVListAllow$CONF, bestVListAllow))
anova(lm(bestVListDeny$BEST_VALUES ~ bestVListDeny$CONF, bestVListDeny))
TukeyHSD(aov(bestVListDeny$BEST_VALUES ~ bestVListDeny$CONF, bestVListDeny))

# Double check with Dunnett:

Group1 <- bestVListFCSAllow$CONF
Value1 <- bestVListFCSAllow$BEST_VALUES
Group2 <- bestVListFCSDeny$CONF
Value2 <- bestVListFCSDeny$BEST_VALUES
data1 <- data.frame(Group1, Value1)
data2 <- data.frame(Group2, Value2)
fit1 <- aov(Value1 ~ Group1, data1)
fit2 <- aov(Value2 ~ Group2, data2)
set.seed(20140123)
Dunnet1 <- glht(fit1, linfct=mcp(Group1="Dunnett"))
Dunnet2 <- glht(fit2, linfct=mcp(Group2="Dunnett"))
summary(Dunnet1)
summary(Dunnet2)

# Triple check with kruskal:

kruskal.test(bestVListFCSAllow$BEST_VALUES ~ bestVListFCSAllow$CONF, data = bestVListFCSAllow)
pairwise.wilcox.test(bestVListFCSAllow$BEST_VALUES, bestVListFCSAllow$CONF, p.adjust.method = "holm")
kruskal.test(bestVListFCSDeny$BEST_VALUES ~ bestVListFCSDeny$CONF, data = bestVListFCSDeny)
pairwise.wilcox.test(bestVListFCSDeny$BEST_VALUES, bestVListFCSDeny$CONF, p.adjust.method = "holm")

#----------------------------------------- TIME

shapiro.test(bestTTree$BEST_VALUES)
qqnorm(bestTTree$BEST_VALUES)

# Besause normal distribution:

anova(lm(bestTTree$BEST_VALUES ~ bestTTree$CONF, bestTTree))
TukeyHSD(aov(bestTTree$BEST_VALUES ~ bestTTree$CONF, bestTTree))

# Double check with Dunnett:

Group <- bestTTree$CONF
Value <- bestTTree$BEST_VALUES
data <- data.frame(Group, Value)
fit <- aov(Value ~ Group, data)
set.seed(20140123)
Dunnet <- glht(fit, linfct=mcp(Group="Dunnett"))
summary(Dunnet)


# Triple check with kruskal:

kruskal.test(bestTTree$BEST_VALUES ~ bestTTree$CONF, data = bestTTree)
pairwise.wilcox.test(bestTTree$BEST_VALUES, bestTTree$CONF, p.adjust.method = "holm")

shapiro.test(bestTListAllow$BEST_VALUES)
shapiro.test(bestTListDeny$BEST_VALUES)

# Besause not normal distribution:

kruskal.test(bestTListAllow$BEST_VALUES ~ bestTListAllow$CONF, data = bestTListAllow)
pairwise.wilcox.test(bestTListAllow$BEST_VALUES, bestTListAllow$CONF, p.adjust.method = "holm")
kruskal.test(bestTListDeny$BEST_VALUES ~ bestTListDeny$CONF, data = bestTListDeny)
pairwise.wilcox.test(bestTListDeny$BEST_VALUES, bestTListDeny$CONF, p.adjust.method = "holm")

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

get_stats(get_vector2(listIndCovAllowData, listIndCovDenyData, "TIME"))
get_stats(get_vector2(listIndAlpha0AllowData, listIndAlpha0DenyData, "TIME"))
get_stats(get_vector2(listIndAlpha05AllowData, listIndAlpha05DenyData, "TIME"))
get_stats(get_vector2(listIndAlpha1AllowData, listIndAlpha1DenyData, "TIME"))

get_stats(treeIndCovFP/5330)
get_stats(treeIndCovFN/5330)
get_stats(treeIndAlpha0FP/5330)
get_stats(treeIndAlpha0FN/5330)
get_stats(treeIndAlpha05FP/5330)
get_stats(treeIndAlpha05FN/5330)
get_stats(treeIndAlpha1FP/5330)
get_stats(treeIndAlpha1FN/5330)

get_stats(listIndCovAllowFP/5330)
get_stats(listIndCovAllowFN/5330)
get_stats(listIndCovDenyFP/5330)
get_stats(listIndCovDenyFN/5330)
get_stats(listIndAlpha0AllowFP/5330)
get_stats(listIndAlpha0AllowFN/5330)
get_stats(listIndAlpha0DenyFP/5330)
get_stats(listIndAlpha0DenyFN/5330)
get_stats(listIndAlpha05AllowFP/5330)
get_stats(listIndAlpha05AllowFN/5330)
get_stats(listIndAlpha05DenyFP/5330)
get_stats(listIndAlpha05DenyFN/5330)
get_stats(listIndAlpha1AllowFP/5330)
get_stats(listIndAlpha1AllowFN/5330)
get_stats(listIndAlpha1DenyFP/5330)
get_stats(listIndAlpha1DenyFN/5330)

# ---------
# Graphs
# ---------

Ftreea0 <- ggplot(treeIndAlpha0Data, aes(x = treeIndAlpha0Data$IT, y =BEST_F/47966)) +
  geom_line(aes(colour = factor(FOLD))) +
  stat_summary(geom = "line") + scale_y_continuous(limits = c(0,0.5)) +
  xlab("Iterations (α = 0)") +
  ylab("Best Fitness") + theme(legend.position = "bottom")
theLegend <- get_legend(Ftreea0)
Ftreea0 <- Ftreea0 + theme(legend.position = "none")
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

grid.arrange(Ftreea0, Ftreea05, Ftreea1, theLegend, ncol = 3, nrow = 2, layout_matrix = cbind(c(1, 4), c(2, 4), c(3, 4)))
#grid.arrange(Ftreea0, Ftreea05, Ftreea1, ncol = 3)

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

itvsFTreeCov <- ggplot(subset(treeIndCovData[,c("IT","BEST_F","FOLD")],IT%%5==0),aes(x=IT,y=BEST_F,group=IT)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(from=0,to=150,by=10)) +
  xlab("Iterations") +
  ylab("FAcc") +
  theme(axis.title.y = element_text(size = 8))
itvsFTreeA0 <- ggplot(subset(treeIndAlpha0Data[,c("IT","BEST_F","FOLD")],IT%%5==0),aes(x=IT,y=BEST_F/47966,group=IT)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(from=0,to=150,by=10)) +
  xlab("Iterations") +
  ylab("FCS (\u03B1 = 0)") +
  theme(axis.title.y = element_text(size = 8))
itvsFTreeA05 <- ggplot(subset(treeIndAlpha05Data[,c("IT","BEST_F","FOLD")],IT%%5==0),aes(x=IT,y=BEST_F/47966,group=IT)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(from=0,to=150,by=10)) +
  xlab("Iterations") +
  ylab("FCS (\u03B1 = 0.5)") +
  theme(axis.title.y = element_text(size = 8))
itvsFTreeA1 <- ggplot(subset(treeIndAlpha1Data[,c("IT","BEST_F","FOLD")],IT%%5==0),aes(x=IT,y=BEST_F/47966,group=IT)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(from=0,to=150,by=10)) +
  xlab("Iterations") +
  ylab("FCS (\u03B1 = 1)") +
  theme(axis.title.y = element_text(size = 8))

pittsburghItvsF <- grid.arrange(itvsFTreeCov, itvsFTreeA0, itvsFTreeA05, itvsFTreeA1, ncol = 1, nrow = 4)
cairo_pdf("../img/pittsburghItvsF.pdf", width = 7, height = 6)
grid.draw(pittsburghItvsF)
dev.off()

itvsFListCovAllow <- ggplot(subset(listIndCovAllowData[,c("IT","BEST_F","FOLD")],IT%%5==0),aes(x=IT,y=BEST_F,group=IT)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(from=0,to=150,by=10)) +
  xlab("Iterations") +
  ylab("FAcc") +
  theme(axis.title.y = element_text(size = 8))
itvsFListA0Allow <- ggplot(subset(listIndAlpha0AllowData[,c("IT","BEST_F","FOLD")],IT%%5==0),aes(x=IT,y=BEST_F/47966,group=IT)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(from=0,to=150,by=10)) +
  xlab("Iterations") +
  ylab("FCS (\u03B1 = 0)") +
  theme(axis.title.y = element_text(size = 8))
itvsFListA05Allow <- ggplot(subset(listIndAlpha05AllowData[,c("IT","BEST_F","FOLD")],IT%%5==0),aes(x=IT,y=BEST_F/47966,group=IT)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(from=0,to=150,by=10)) +
  xlab("Iterations") +
  ylab("FCS (\u03B1 = 0.5)") +
  theme(axis.title.y = element_text(size = 8))
itvsFListA1Allow <- ggplot(subset(listIndAlpha1AllowData[,c("IT","BEST_F","FOLD")],IT%%5==0),aes(x=IT,y=BEST_F/47966,group=IT)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(from=0,to=150,by=10)) +
  xlab("Iterations") +
  ylab("FCS (\u03B1 = 1)") +
  theme(axis.title.y = element_text(size = 8))

pittsburghItvsF2 <- grid.arrange(itvsFListCovAllow, itvsFListA0Allow, itvsFListA05Allow, itvsFListA1Allow, ncol = 1, nrow = 4)
cairo_pdf("../img/michiganItvsF_allow.pdf", width = 7, height = 6)
grid.draw(pittsburghItvsF2)
dev.off()

itvsFListCovDeny <- ggplot(subset(listIndCovDenyData[,c("IT","BEST_F","FOLD")],IT%%5==0),aes(x=IT,y=BEST_F,group=IT)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(from=0,to=150,by=10)) +
  xlab("Iterations") +
  ylab("FAcc") +
  theme(axis.title.y = element_text(size = 8))
itvsFListA0Deny <- ggplot(subset(listIndAlpha0DenyData[,c("IT","BEST_F","FOLD")],IT%%5==0),aes(x=IT,y=BEST_F/47966,group=IT)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(from=0,to=150,by=10)) +
  xlab("Iterations") +
  ylab("FCS (\u03B1 = 0)") +
  theme(axis.title.y = element_text(size = 8))
itvsFListA05Deny <- ggplot(subset(listIndAlpha05DenyData[,c("IT","BEST_F","FOLD")],IT%%5==0),aes(x=IT,y=BEST_F/47966,group=IT)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(from=0,to=150,by=10)) +
  xlab("Iterations") +
  ylab("FCS (\u03B1 = 0.5)") +
  theme(axis.title.y = element_text(size = 8))
itvsFListA1Deny <- ggplot(subset(listIndAlpha1DenyData[,c("IT","BEST_F","FOLD")],IT%%5==0),aes(x=IT,y=BEST_F/47966,group=IT)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(from=0,to=150,by=10)) +
  xlab("Iterations") +
  ylab("FCS (\u03B1 = 1)") +
  theme(axis.title.y = element_text(size = 8))

pittsburghItvsF3 <- grid.arrange(itvsFListCovDeny, itvsFListA0Deny, itvsFListA05Deny, itvsFListA1Deny, ncol = 1, nrow = 4)
cairo_pdf("../img/michiganItvsF_deny.pdf", width = 7, height = 6)
grid.draw(pittsburghItvsF3)
dev.off()
