setwd('~/R/')

#Execute this script just once!

add_fold_column <- function(directory) {
  
  i <- 0
  for(inFile in list.files(path = directory, full.names = TRUE)) {
    dataInFile <- read.table(file = inFile, header = TRUE, sep = ";")
    dataInFile$FOLD <- paste("fold", i, sep = "")
    write.table(dataInFile, file = inFile)
    i <- i+1
  }
}

treeIndAlpha05Data <- add_fold_column("./lanzar1/CSVs")
treeIndAlpha0Data <- add_fold_column("./lanzar2/CSVs")
treeIndAlpha1Data <- add_fold_column("./lanzar3/CSVs")
treeIndCovData <- add_fold_column("./lanzar4/CSVs")

listIndCovAllowData <- add_fold_column("./lanzar5/150gen GRANTED/CSVs")
listIndCovDenyData <- add_fold_column("./lanzar5/150gen STRONGDENY/CSVs")
listIndAlpha05AllowData <- add_fold_column("./lanzar6/150gen GRANTED/CSVs")
listIndAlpha05DenyData <- add_fold_column("./lanzar6/150gen STRONGDENY/CSVs")
listIndAlpha0AllowData <- add_fold_column("./lanzar7/150gen GRANTED/CSVs")
listIndAlpha0DenyData <- add_fold_column("./lanzar7/150gen STRONGDENY/CSVs")
listIndAlpha1AllowData <- add_fold_column("./lanzar8/150gen GRANTED/CSVs")
listIndAlpha1DenyData <- add_fold_column("./lanzar8/150gen STRONGDENY/CSVs")