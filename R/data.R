#' @title datasets
#' @description A function to get an interface to datasets ready to be worked with in the vignettes for the package.
#' @format A data frame with 7 rows and 9 variables:
#' \describe{
#'   \item{\code{name}}{character Dataset published name}
#'   \item{\code{packName}}{character Dataset published package}
#'   \item{\code{className}}{character Name of class attribute}
#'   \item{\code{classVar}}{integer Number of class attribute}
#'   \item{\code{idNumber}}{integer Number of identity attribute if present, otherwise NaN}
#'   \item{\code{K}}{integer Number of classes reported by dataset}
#'   \item{\code{Kp}}{integer Number of classes induced from dataset}
#'   \item{\code{n}}{integer Number of columns in dataset}
#'   \item{\code{m}}{integer Number of instances in dataset} 
#'}
# @source \url{https://github.com/FJValverde/entropies}
"datasets"

#' A function to get an interface to datasets ready to be worked with in the vignettes.  
#' 
#' Returns a tibble with several fields:
#' - name: a standard name for the dataset
#' - className: a vector of names for the columns acting as class variables
#' - idNumber: columns to be excised, main reason, because they are identifiers
#' - K: cardinality of class
#' - m: number of instances in the dataset.
#' - n: number of variables in the dataset.
# @importFrom tibble tibble
#getDatasets <- function(){
