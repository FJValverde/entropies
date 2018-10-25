#' A function to select a Dataset by name
#' Caveat: you have to ensure that the containing package has been attached.
#' 
#' @param dsName A database name for a Dataset present in the environment.
# FVA: Do not import the Datasets. Make them only a Suggests:(ion)
# @importFrom Datasets iris
# @importFrom mlbench Ionosphere Glass BreastCancer Sonar 
# @importFrom vcd Arthritis
# @importFrom candisc Wine
#' @export
#' @example ds <- evalDataset("iris")
evalDataset <- function(dsName){
    dsName <- as.character(dsName)
    switch(dsName, #TODO: improve this way of "invoking" the Dataset.
           "iris" =         {loadDataset(dsName, "datasets")},
           "Ionosphere" =   {loadDataset(dsName, "mlbench")},
           "Glass" =        {loadDataset(dsName, "mlbench")},
           "Arthritis" =    {loadDataset(dsName, "vcd")},
           "BreastCancer" = {loadDataset(dsName, "mlbench")},
           "Sonar" =        {loadDataset(dsName, "mlbench")},
           "Wine" =         {loadDataset(dsName, "candisc")}
    ) #This value "FALLS THROUGH"
}
# evalDataset <- function(dsName){
#     dsName <- as.character(dsName)
#     switch(dsName, #TODO: improve this way of "invoking" the Dataset.
#            "iris" =         {data(iris); iris},
#            "Ionosphere" =   {data(Ionosphere); Ionosphere},
#            "Glass" =        {data(Glass); Glass},
#            "Arthritis" =    {data(Arthritis); Arthritis},
#            "BreastCancer" = {data(BreastCancer); BreastCancer},
#            "Sonar" =        {data(Sonar); Sonar},
#            "Wine" =         {data(Wine); Wine}
#     ) #This value "FALLS THROUGH"
# }
