#' Conditional entropy decomposition of dataframes, similar to entropies
#' 
#' Returns several different flavours of entropies depending on the structure 
#' the data is provided to the function. There are specialized versions for
#' (contingency) tables, confusion matrices and data frames.
#' @param X The input random vector
#' @param Y The conditioning random vector
#' @return  A dataframe with the entropies of the marginals
#' @details Unless specified by the user explicitly, this function uses base 2 
#'   logarithms for the entropies.
#' @seealso \code{\link[entropy]{entropy}, \link[infotheo]{entropy}}
# @import dplyr
#' @export
condentropies <- function(X, Y, ...) UseMethod("condentropies") 

#' Entropy decomposition of a data frame (multivariate decomposition)
#' 
#' @return Another dataframe with the main entropy coordinates of every variable Xi
#'   in the original conditioned on the datatables Y, which are now the rows of the returned data.frame.
#' @export
#' @importFrom infotheo discretize condentropy
#' @importFrom dplyr mutate
condentropies.data.frame <- function(X, Y, ...){
    if (ncol(X) == 0 || nrow(X) == 0 ) 
        stop("Can only work with non-empty data.frames X!")
    if (ncol(Y) == 0 || nrow(Y) == 0 )
        stop("Can only condition on non-empty data.frame Y! ")
    if (nrow(X) != nrow(Y) )
        stop("Can only condition on variables with the same number of instances!")
    if (!all(unlist(lapply(colnames(X), function(name){is.factor(X[,name])})))){
        warning("Discretizing data before entropy calculation!")
        X <- infotheo::discretize(X, disc="equalwidth", ...) # infotheo::str(dfdiscretize generates ints, not factors.
    }
    if (!all(unlist(lapply(colnames(Y), function(name){is.factor(Y[,name])})))){
        warning("Discretizing conditioning data before entropy calculation!")
        Y <- infotheo::discretize(Y, disc="equalwidth", ...) # infotheo::str(dfdiscretize generates ints, not factors.
    }
    # suppose the dataframe is categorical
    # Find simple entropies, divergences and entropies of the uniform marginals. 
    name <-  colnames(X)
    edf <- data.frame(
        name = name, # After an idyosincracy of dplyr, the rownames donot survive a mutate.
        H_Uxi = unlist(lapply(X, function(v){log2(length(unique(v)))})),
        H_Pxi = unlist(lapply(X, function(v){natstobits(infotheo::entropy(v))})),
        stringsAsFactors = FALSE #Keep the original variable names as factors!
    ) %>% dplyr::mutate(DeltaH_Pxi = H_Uxi - H_Pxi) 
    #M_Pxi = H_Pxi - VI_Pxi)
    if (ncol(X) == 1){
        warning("Single variable: providing only entropy")
    } else {
        VI_Pxi <- vector("numeric", length(name))
        for(i in 1:length(name)){
            VI_Pxi[i] <- natstobits(condentropy(X=X[,i], Y=cbind(X[,-i], Y)))
        }
        edf <- mutate(edf, VI_Pxi, M_Pxi = H_Pxi - VI_Pxi)
    }
    return(edf)
}
