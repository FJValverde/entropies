#' A function to get an interface to datasets ready to be worked with. 
#' 
#' Returns a tibble with several fields:
#' - name: a standard name for the dataset
#' - className: a vector of names for the columns acting as class variables
#' - idNumber: columns to be excised, main reason, because they are identifiers
#' - K: cardinality of class
#' - m: number of instances in the dataset.
#' - n: number of variables in the dataset.
#' @import tidyverse
#' @import tibble
#' @export
#' @example edf <- getDatasets()
getDatasets <- function(){
    # the inventory of databases you can access through this interface
    #library(datasets)
    # TODO: import these datasets from the corresponding 
    dsNames <- c("Ionosphere", "iris", "Glass", "Arthritis", "BreastCancer", "Sonar", "Wine") # 
    # The following are vectors, so that multilabel datasets are allowed!
    className <- c(c("Class"),c("Species"), c("Type"), c("Improved"), 
                   c("Class"),c("Class"),c("Cultivar"))  # Name of class attribute
    classVar <- #c(35, 5, 10, 5, 11, 61, 1)   # ordinal of the class attribute
                c(c(35),c(5),c(10),c(5),c(11),c(61),c(1))# ordinals of the label attributes
    idNumber <- c(NaN, NaN, NaN, 1, 1, NaN, NaN) # Other attributes to dispose of: mainly identifiers.
    
    K <- c(2, 3, 7, 3, 2, 2, 3)  
        # No. of classes in the class variable, this can actually be obtained from the dataset
    m <- sapply(dsNames, function(n){nrow(evalDataset(n))}) # no. of instances in the dataset
    n <- sapply(dsNames, function(n){ncol(evalDataset(n))}) - 1 - as.numeric(!is.nan(idNumber)) # no. of features in the dataset.
    datasets <- tibble(name=dsNames, 
                           className, 
                           idNumber=idNumber, 
                           K=as.integer(K), 
                           n=as.integer(n), 
                           m=as.integer(m))
    # #To select the #of column of the classc
    # whichClass <- function(ds, className){which(colnames(evalDatasset(ds))==className)}
    # #whichNumVar <-  function(r){whichClass(evalDataset(r$name), r$className)}
    # cardinalClass <- function(ds, className){
    #     length(unique(evalDataset(ds)[,className]))
    # }
    # classVar <-  mapply(whichClass, datasets$name, datasets$className)
    # K <- mapply(cardinalClass, datasets$name, classVar)
    # library(dplyr)
    # datasets <- data.frame(name,className, classVar, K)
    return(datasets)
}
    