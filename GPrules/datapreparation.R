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

add_fold_column("./lanzar1/CSVs")
add_fold_column("./lanzar2/CSVs")
add_fold_column("./lanzar3/CSVs")
add_fold_column("./lanzar4/CSVs")

add_fold_column("./lanzar5/150gen GRANTED/CSVs")
add_fold_column("./lanzar5/150gen STRONGDENY/CSVs")
add_fold_column("./lanzar6/150gen GRANTED/CSVs")
add_fold_column("./lanzar6/150gen STRONGDENY/CSVs")
add_fold_column("./lanzar7/150gen GRANTED/CSVs")
add_fold_column("./lanzar7/150gen STRONGDENY/CSVs")
add_fold_column("./lanzar8/150gen GRANTED/CSVs")
add_fold_column("./lanzar8/150gen STRONGDENY/CSVs")