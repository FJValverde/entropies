#' A function to load programmatically a dataset from a package by name. 
#' 
#' Caveat: you have to ensure that the containing package has been attached.
#' Code and ideas from
#'  https://stackoverflow.com/questions/3408943/a-function-that-returns-a-dataset
#' @param name The name of the dataset
#' @param pkg The name of the package to fetch it from
#' @example loadDataset("iris", "datasets")# Not exported so not needed. 
#' @export
loadDataset <- function(name, pkg) {
    do.call("data", list(name,package=pkg))
    return(get(name))#use "invisible" to return but make value invisible
}
