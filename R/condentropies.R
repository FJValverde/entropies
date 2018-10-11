#' Conditional entropy decomposition of dataframes, similar to entropies
#' 
#' Returns several different flavours of entropies depending on the structure 
#' the data is provided to the function. There are specialized versions for
#' (contingency) tables, confusion matrices and data frames.
#' @param data The data being provided to the function. 
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
#' @importFrom infotheo discretize
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
        X <- infotheo::discretize(df, disc="equalwidth", ...) # infotheo::str(dfdiscretize generates ints, not factors.
    }
    if (!all(unlist(lapply(colnames(Y), function(name){is.factor(Y[,name])})))){
        warning("Discretizing conditioning data before entropy calculation!")
        Y <- infotheo::discretize(df, disc="equalwidth", ...) # infotheo::str(dfdiscretize generates ints, not factors.
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
        #         edf <- data.frame(
        #             name = colnames(df),
        #             H_Uxi = log2(length(unique(df[,1]))),
        #             H_Pxi = infotheo::entropy(df[,1])
        #         ) %>% mutate(DeltaH_Pxi = H_Uxi - H_Pxi)
    } else {
        #entropyNames <- c("name", "H_Uxi", "H_Pxi", "VI_Pxi", "DeltaH_Pxi","M_Pxi")
        #colnames(edf) <- entropyNames
        #name <- colnames(df) # get the colnames once and for all
        #nn <- length(name)
        #H_Uxi <-  unlist(lapply(df, function(v){log2(length(unique(v)))}))
        #H_Pxi <- unlist(lapply(df, function(v){natstobits(infotheo::entropy(v))}))
        #VI_Pxi <- sapply(name, function(n){infotheo::condentropy(df[,n], Y=df[,setdiff(name, n)])})
        #         edf <- data.frame(
        #             name = colnames(df), # After an idyosincracy of dplyr, the rownames donot survive a mutate.
        #             H_Uxi = unlist(lapply(df, function(v){log2(length(unique(v)))})),
        #             H_Pxi = unlist(lapply(df, function(v){natstobits(infotheo::entropy(v))})), 
        #             VI_Pxi = sapply(name, function(n){infotheo::condentropy(df[,n], Y=df[,setdiff(name, n)])})
        #         ) %>% 
        #             mutate(DeltaH_Pxi = H_Uxi - H_Pxi, 
        #                    M_Pxi = H_Pxi - VI_Pxi)
        VI_Pxi <- vector("numeric", length(name))
        for(i in 1:length(name)){
            VI_Pxi[i] <- natstobits(condentropy(X=X[,i], Y=cbind(X[,-i], Y)))
        }
        edf <- mutate(edf, VI_Pxi, M_Pxi = H_Pxi - VI_Pxi)
        #         edf <- mutate(edf,
        #                       VI_Pxi = sapply(name, function(x){natstobits(infotheo::condentropy(df[,x], df[, setdiff(name, x)]))}),
        #                       M_Pxi = H_Pxi - VI_Pxi
        #                       )
    }
    return(edf)
}
