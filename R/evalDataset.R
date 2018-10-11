#' A function to select a dataset by name
#' Caveat: you have to ensure that the containing package has been attached.
#' 
#' @param dsName A database name for a dataset present in the environment.
#' @export
# @example ds <- evalDataset("iris")
evalDataset <- function(dsName){
    dsName <- as.character(dsName)
    switch(dsName, #TODO: improve this way of "invoking" the dataset.
           "iris" =         {data(iris); iris},
           "Ionosphere" =   {data(Ionosphere); Ionosphere},
           "Glass" =        {data(Glass); Glass},
           "Arthritis" =    {data(Arthritis); Arthritis},
           "BreastCancer" = {data(BreastCancer); BreastCancer},
           "Sonar" =        {data(Sonar); Sonar},
           "Wine" =         {data(Wine); Wine}
    ) #This value "FALLS THROUGH"
}
