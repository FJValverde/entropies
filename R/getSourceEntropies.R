#' A function to get the source entropies of a dataset ready to be passed onto 
#' a source entropy triangle. 
#' 
#' @import tidyverse
#' @import tibble
#' @export
getSourceEntropies <- function(
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
    print("The class is number:", thisClass)
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
    for(withClass in withClasses){# we profit from the fact what we go over TRUE first
        if (!withClass)
            ds <- dplyr::select(ds, -thisClass)
    }
    edf <- tibble()#TODO: decide whether working with tibbles is advantageous.
    edf <- rbind(edf,
                 sentropies(ds, type) %>% 
                     mutate(withClass = withClass,
                            isClass = (className == as.character(name))
                     )
    )
    # Return with a name, if available
    if (is.null(dsName))
        return(edf)
    else
        return(mutate(edf, dsName))
}
