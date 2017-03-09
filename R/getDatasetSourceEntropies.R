#' A function to get the source entropies of a dataset ready to be passed onto 
#' a source entropy triangle. 
#' 
#' @param ds A database considered in the "datasets" data frame.
#' @param dsName The name of the database being considered to be added in a column. 
#' Defaults to "Class".
#' @param idName An identifier field (to be excised from the database). Defaults to NULL.
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
    className="Class", #Name of class, a sensible default
    idName=NULL, #Name of id, a sensible default
    withClass=TRUE,#Whether correlation with 
    type="total"#whether "total" or "dual"
){
    # Parameter analysis
    theseNames <- names(ds)
    thisClass <- which(className == theseNames)
    #print("The class is number:", thisClass)
    if (thisClass == 0)
        stop("Unknown column variable:",className)
    #K <- length(unique(ds[,thisClass]))
    K <- nrow(unique(dplyr::select_(ds, thisClass)))
    # 1.2. Wipeout any possible identifiers
    if (!is.null(idName))
        ds <- dplyr::select_(ds, -idName)
    # 1.3 Decide whether to analyze with the Class or not
    if (!withClass)
        withClasses <- c(FALSE)
    else
        withClasses <- c(TRUE,FALSE)
    # Data analysis
    ds <- infotheo::discretize( # Controlled discretization of the database.
        ds, 
        nbins=max(ceiling(nrow(ds)^(1/2)), K)# At least the classes.
    )
    edf <- tibble()#TODO: decide whether working with tibbles is advantageous.
    for(withClass in withClasses){# we profit from the fact what we go over TRUE first
        if (!withClass)
            ds <- dplyr::select(ds, -thisClass)
        edf <- rbind(edf,
                     sentropies(ds, type) %>% 
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
