setwd('~/Github/2017-ESWA/Results')

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

add_fold_column("./tree_ind_alpha_0")
add_fold_column("./tree_ind_alpha_0.5")
add_fold_column("./tree_ind_alpha_1")
add_fold_column("./tree_ind_coverage")

add_fold_column("./list_ind_alpha_0/150gen GRANTED")
add_fold_column("./list_ind_alpha_0/150gen STRONGDENY")
add_fold_column("./list_ind_alpha_0.5/150gen GRANTED")
add_fold_column("./list_ind_alpha_0.5/150gen STRONGDENY")
add_fold_column("./list_ind_alpha_1/150gen GRANTED")
add_fold_column("./list_ind_alpha_1/150gen STRONGDENY")
add_fold_column("./list_ind_coverage/150gen GRANTED")
add_fold_column("./list_ind_coverage/150gen STRONGDENY")