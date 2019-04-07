#' Multivariate joint entropy decomposition of random vectors
#' 
#' Returns several different flavours of entropies depending on the structure 
#' the data is provided to the function. There are specialized versions for
#' (contingency) tables, confusion matrices and data frames.
#' 
#' @param X One random vector being provided to the function. 
#' @param Y Another random vector being provided to the function. 
#' @return  A dataframe with the entropies of the marginals
#' @details This function uses base 2 logarithms for the entropies.
#' @seealso \code{\link[entropy]{entropy::entropy}, \link[infotheo]{infotheo::entropy}, \link{sentropies}}
#' @export
jentropies <- function(X, Y, ...) UseMethod("jentropies") 

#' Multivariate Joint Entropy decomposition of two data frames
#' 
#' @describeIn jentropies
#' @return Another dataframe with the main entropy coordinates of every variable Xi
#'   in the original conditioned on the datatables Y, which are now the rows of the returned data.frame.
#' @export
#' @importFrom infotheo discretize condentropy entropy
# @importFrom infotheo condentropy
# @importFrom infotheo entropy
#' @importFrom dplyr mutate
#' @importFrom purrr map_lgl 
jentropies.data.frame <- function(X, Y, ...){
    if (ncol(X) == 0 | nrow(X) == 0 ) 
        stop("Can only work with non-empty data.frames X!")
    if (ncol(Y) == 0 | nrow(Y) == 0 )
        stop("Can only condition on non-empty data.frame Y! ")
    if (nrow(X) != nrow(Y) )
        stop("Can only condition on data frames with the same number of rows!")
    if (!all(sapply(X, is.integer) | sapply(X, is.factor))){
        warning("Discretizing primary data before entropy calculation!")
        X <- infotheo::discretize(X, disc="equalwidth", ...) # infotheo::str(dfdiscretize generates ints, not factors.
    }
    if (!all(sapply(Y, is.integer)  | sapply(Y, is.factor))){
        warning("Discretizing secondary data before entropy calculation!")
        Y <- infotheo::discretize(Y, disc="equalwidth", ...) # infotheo::str(dfdiscretize generates ints, not factors.
    }
    # suppose the dataframe is categorical
    # Find simple entropies, divergences and entropies of the uniform marginals. 
    VI_P <- natstobits(c(condentropy(X,Y), condentropy(Y,X)))
    edf <- data.frame(
        type = c("X", "Y"), # After an idyosincracy of dplyr, the rownames do not survive a mutate.
        H_U = c(
            log2(prod(sapply(X, n_distinct))),
            #sum(sapply(X, function(v){log2())})),
            log2(prod(sapply(X, n_distinct)))
            #sum(sapply(Y, function(v){log2(length(unique(v)))}))
        ),
        # TODO: check the "other" way to worl out H_U
        H_P = natstobits(c(infotheo::entropy(X), infotheo::entropy(Y))),
        stringsAsFactors = FALSE #Keep the original variable names as factors!
    ) %>% dplyr::mutate(
        DeltaH_P = H_U - H_P, 
        M_P = H_P - VI_P,
        VI_P = VI_P
    ) 
    # Add the joint balance equations
    return(rbind(edf,cbind(type="XY", as.data.frame(lapply(edf[,2:6], sum)))))
    #return(edf)
}

#' Entropy decomposition of a possibly higher-order contingency matrix
#' 
#' Given a contingency matrix, provide one row of entropy coordinates. 
#' NOTE: the reference variable has to index the ROWS of the table, while the predicted
#' variable indexes the columns, unlike, e.g. \code{\link[caret]{confusionMatrix}}. 
#' That is the entropies are obtained for the first two margins. If other margins are 
#' needed you will need to reorder them.
#' 
#' @describeIn jentropies
#' 
#' @param Nxy An n-contingency matrix where n > 2
#' @param unit The logarithm to be used in working out the sentropies as per 
#' \code{\link[entropy]{entropy}}. Defaults to "log2".
#' @export
## @example library(datasets)
## @example jentropies(datasets::UCBAdmissions, unit="log10") # Entropies in dB
## @example jentropies(datasets::Titanic) # Entropies in bits
jentropies.table <- function(X, ...){
    # 0. Parameter checking
    Nxy <- as.table(X) # is this necessary? Can it be called without this?
    dims <- dim(Nxy)
    if (length(dims) < 2)
        stop("Cannot process joint entropies for tables with less than 2 dimensions.")
    # if (length(dims) > 2)
    #     stop("Cannot process joint entropies for tables of more than 2 dimensionss")
    #    if (length(dims) < 2 | length(dims) > 3)
    #        stop("Cannot process tables with more than 3 dimensions or less than 2 dimensions.")
    if (dims[1] < 2 | dims[2] < 2)
        stop("jentropies are not defined for distributions with a singleton domain.")
    # 1. Start processing: this is a candidate por jentropies_raw
    #unless otherwise specified, we use log2 logarithms
    # CAVEAT: use a more elegant kludge
    vars <- list(...);
    #    if (!("unit" %in% names(vars)))
    if (is.null(vars$unit)) vars$unit <- "log2"
    if (length(dims)==2){ # N is a plain contingency on X and Y
        edf <- jentropies2d.table(Nxy, vars)
    } else {  # N is a dim > 2 multiway table: we analyze on the first two margins, but store the rest
        ctsColnames <- colnames(expand.grid(dimnames(Nxy)))
        if (length(dim(Nxy)) == 3){#a special case, e.g. Nxy <- datasets::UCBAdmissions
            cts <- as.matrix(apply(Nxy, 3, jentropies2d.table, vars))
            ctsGrid <- as.data.frame(dimnames(cts)[[1]], colnames=ctsColnames[3])
            colnames(ctsGrid) <- ctsColnames[3]
        } else{# N > 3, e.g. Nxy <- datasets::Titanic
            cts <- apply(Nxy, 3:length(dim(Nxy)), jentropies2d.table, vars)#the vars carry the rest of the params
            # cts is both a matrix and a list of X dims(factors) elements. 
            ctsGrid <- expand.grid(dimnames(cts))
        }
        edf <- data.frame()#accumulatorfor rows
        for(i in 1:length(cts)){#cts a list: add to entropies the remaining factors
            edf <- rbind(edf, cbind(cts[[i]], ctsGrid[i,]))
        }
    } 
    return(edf)
}

#' Entropy decomposition of a 2-d contingency table
#' 
#' Given a 2-d contingency table, provide a single vector of joint entropies. 
#' @param Nxy An n-contingency matrix where n > 2
#' @param unit The logarithm to be used in working out the sentropies as per 
#' \code{\link[entropy]{entropy}}. Defaults to "log2".
#' @export
#' @importFrom entropy entropy
#' @importFrom dplyr mutate
jentropies2d.table<- function(Nxy, ...){
    Nx <- apply(Nxy, 1, sum) # to be transformed into a probability
    #N <- sum(Nx)
    #H_x <- sapply(Nx, function(n){- n/N * log2(n/N)})
    H_x <- entropy::entropy(Nx, unit="log2")
    #Hx <- do.call(entropy, c(list(y=Nx), vars)) #entropy(Nx,vars)
    Ny <- apply(Nxy, 2, sum)
    #H_y <- sum(sapply(Ny, function(n){- n/N * log2(n/N)}))
    H_y <- entropy::entropy(Ny, unit="log2")
    #H_xy <- sum(sum((Nxy/N)*log2((Nxy * N)/(Nx %*% t(Ny)))))
    #H_xy <- sum(sum(-(Nxy/N)*log2(Nxy/N)))
    H_xy <- entropy::entropy(Nxy, unit="log2")
    #Hy <- do.call(entropy, c(list(y=Ny), vars)) #entropy(Ny, vars)
    Ux <- log2(dim(Nxy)[1]) #entropy(rep(1/dims[1],dims[1]),unit="log2",...)
    Uy <- log2(dim(Nxy)[2]) #entropy(rep(1/dims[2],dims[2]),unit="log2",...)
    #Hxy <- do.call(entropy, c(list(y=Nxy), vars)) #entropy(Nxy, vars) 
    VI_P <- c(H_xy - H_y, H_xy - H_x)
    edf <- data.frame(
        type = c("X", "Y"), # After an idyosincracy of dplyr, the rownames do not survive a mutate.
        H_P = c(H_x, H_y), #natstobits(c(infotheo::entropy(X), infotheo::entropy(Y))),
        H_U = c(Ux, Uy),
        stringsAsFactors = FALSE #Keep the original variable names as factors!
    ) %>% dplyr::mutate(
        DeltaH_P = H_U - H_P, 
        M_P = H_P - VI_P,
        VI_P = VI_P #The ordering of the fields is important for exploratory purposes.s
    ) 
    return(rbind(edf,cbind(type="XY", as.data.frame(lapply(edf[,2:6], sum)))))
}    