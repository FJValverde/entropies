#' A function to get the source entropies of a dataset ready to be passed onto 
#' a source entropy triangle. 
#' 
#' @param ds A database considered in the "datasets" data frame.
#' @param dsName The name of the database being considered to be added in a column. 
#' Defaults to "NULL".
#' @param idNumber An identifier field (to be excised from the database). Defaults to NULL.
#' @param withClass A logical whether to consider the class as part of the database
#' or not. Defaults to not. 
#' @param type Whether to use the total + dual total correlations (type="total") 
#' or just the dual  total (type= "dual") correelation.
#' @import tidyverse
#' @import tibble
#' @export
#' @example edf <- getSourceEntropies(iris, className="Species", withClass=TRUE, type="dual")
getDatasetSourceEntropies <- function(
    ds, #the dataset to be analysed
    dsName=NULL, #The database name in case it is provided.
    className=c("Class"), #Name of class, a sensible default
    idNumber=NULL, #Name of id, a sensible default
    withClass=TRUE,#Whether correlation with the class is required
    type="total",#whether "total" or "dual" correlation requested
    ...
){
    # 1. Parameter analysis
    # 1.1. Find the number of unique classes
    theseNames <- names(ds)#list of features
    thisClass <- which(className == theseNames)#partition into labels or not
    #print("The class is number:", thisClass)
    if (thisClass == 0)
        stop("Unknown column variable:",className)
    K <- nrow(unique(dplyr::select_(ds, thisClass)))# Obtain the actual number of classes
    # 1.2. Wipeout any possible identifiers (They have artificial info about the class)
    if (is.numeric(idNumber) & !(is.nan(idNumber)))
        ds <- dplyr::select_(ds, -idNumber)
    # 1.3 Record whether to analyze with the Class or not
    if (withClass)
        withClasses <- c(TRUE,FALSE)
    else
        withClasses <- c(FALSE)
    # 1.4 Discretization. Should not be attempted on an already discrete database!
    if (!all(sapply(ds, is.integer) | sapply(ds, is.factor))){
        warning("Discretizing data before entropy calculation!")
        ds <- infotheo::discretize( # Controlled discretization of the database.
                                    ds, 
                                    nbins=max(ceiling(nrow(ds)^(1/2)), K)# At least the classes.
                                )
    }
    # 2. Data analysis
    edf <- tibble()#TODO: decide whether working with tibbles is advantageous.
    for(withClass in withClasses){# we profit from the fact what we go over TRUE first
        if (!withClass)
            ds <- dplyr::select(ds, -thisClass)
        edf <- rbind(edf,
                     sentropies(ds, type,...) %>% 
                         mutate(withClass,
                                isClass = (className == as.character(name))
                         )
        )
    }
    # Return with a name, if available
    if (is.null(dsName))
        return(edf)
    else
        return(mutate(edf, dsName))
}
