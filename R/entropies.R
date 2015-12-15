#' Entropy decomposition of a contingency matrix or dataframe.
#' 
#' Returns several different flavours of entropies depending on the structure 
#' the data is provided to the function. There are specialized versions for
#' (contingency) tables, confusion matrices and data frames.
#' @param data The data being provided to the function. 
#' @return  A dataframe with the entropies of the marginals
#' @details Unless specified by the user explicitly, this function uses base 2 
#'   logarithms for the entropies.
#' @seealso \code{\link[entropy]{entropy}, \link[infotheo]{entropy}}
#' @import dplyr
#' @export
entropies <- function(data, ...) UseMethod("entropies")

#' Entropy decomposition of a contingency matrix
#' 
#' Given a contingency matrix, provide one row of entropy coordinates. 
#' NOTE: the reference variable has to index the ROWS of the table, while the predicted
#' variable indexes the columns, unlike, e.g. \code{\link[caret]{contingencyTable}}
#' @param Nxy An n-contingency matrix where n > 2
#' @param unit The logarithm to be used in working out the entropies as per 
#' \code{entropy}. Defaults to "log2".
#' @export
#' @importFrom entropy entropy
# @importFrom dplyr left_join
# @example entropies(UCBAdmissions)
entropies.table <- function(Nxy, ...){
    # 0. Parameter checking
    Nxy <- as.table(Nxy) # is this necessary?
    dims <- dim(Nxy)
    if (length(dims) < 2)
        stop("Cannot process tables with less than 2 dimensions.")
#    if (length(dims) < 2 | length(dims) > 3)
#        stop("Cannot process tables with more than 3 dimensions or less than 2 dimensions.")
    if (dims[1] < 2 | dims[2] < 2)
        stop("Entropies are not defined for distributions with a singleton domain.")
    # 1. Start processing: this is a candidate por entropies_raw
    #require(entropy)
    #unless otherwise specified, we use log2 logarithms
    # CAVEAT: use a more elegant kludge
    vars <- list(...);
#    if (!("unit" %in% names(vars)))
    if (is.null(vars$unit))
        vars$unit <- "log2"
    if (length(dims)==2){ # N is a plain contingency on X and Y
        Nx <- apply(Nxy, 1, sum); 
        Hx <- do.call(entropy, c(list(y=Nx), vars)) #entropy(Nx,vars)
        Ny <- apply(Nxy, 2, sum); 
        Hy <- do.call(entropy, c(list(y=Ny), vars)) #entropy(Ny, vars)
        Ux <- log2(dims[1]) #entropy(rep(1/dims[1],dims[1]),unit="log2",...)
        Uy <- log2(dims[2]) #entropy(rep(1/dims[2],dims[2]),unit="log2",...)
        Hxy <- do.call(entropy, c(list(y=Nxy), vars)) #entropy(Nxy, vars) 
        df <- data.frame(Ux = Ux, Uy = Uy, Hx = Hx, Hy = Hy, Hxy = Hxy)
    } else {  # N is a multiway table: we analyze on the first two margins, but store the second
        Nx <- margin.table(Nxy, c(1,3:length(dims)))
        Hx <- apply(Nx, c(2:length(dim(Nx))), function(x) {do.call(entropy, c(list(y=x), vars)) })
        #Ux <- apply(Nx, 2, function(x) { log2(length(x))})
        Ux <- apply(Nx, c(2:length(dim(Nx))), function(x) { log2(dims[1])})
        Ny <- margin.table(Nxy, c(2,3:length(dims)))
        Hy <- apply(Ny, c(2:length(dim(Ny))), function(x) {do.call(entropy, c(list(y=x), vars)) })
        #Uy <- apply(Ny, 2, function(x) { log2(length(x))})
        Uy <- apply(Ny, c(2:length(dim(Nx))), function(x) { log2(dims[1])})
        Hxy <- apply(Nxy, 3:length(dims), function(x) {do.call(entropy, c(list(y=x), vars))})
        #df <- data.frame(Ux = Ux, Uy = Uy, Hx = Hx, Hy = Hy, Hxy = Hxy)
        THx <- as.data.frame.table(as.table(Hx), responseName = "Hx")
        TUx <- as.data.frame.table(as.table(Ux), responseName = "Ux")
        THy <- as.data.frame.table(as.table(Hy), responseName = "Hy")
        TUy <- as.data.frame.table(as.table(Uy), responseName = "Uy")
        THxy <- as.data.frame.table(as.table(Hxy), responseName = "Hxy")
        df <- left_join(left_join(left_join(TUx, TUy), left_join(THx, THy)), THxy)
        #df <- data.frame(Ux = Ux, Uy = Uy, Hx = Hx, Hy = Hy, Hxy = Hxy)
        #df <- cbind(df, dimnames(Nxy)[3:length(dims)])# Keep the third  and greater dimension's names
        #name <- colnames(N[1,,]) # This is a hack to manifest the values in the 3rd dimension
    } 
    return(df)
}

#' Entropy decomposition of a confusion table
#' 
#' @export
#' @importFrom caret confusionMatrix
entropies.confusionMatrix <- function(ct, ...){
    return(entropies(t(ct$table), ...))
}

#' Entropy decomposition of a data frame (multivariate decomposition)
#' 
#' @return Another dataframe with the main entropy coordinates of every variable
#'   in the original, which are now the rows of the returned data.frame.
#' @export
#' @import infotheo
#' @import dplyr
entropies.data.frame <- function(df, ...){
    if (ncol(df) == 0 || nrow(df) == 0)
        error("Can only work with non-empty data.frames!")
    if (!all(unlist(lapply(colnames(df), function(name){is.factor(df[,name])})))){
        warning("Discretizing data before entropy calculation!")
        df <- infotheo::discretize(df, disc="equalwidth") # infotheo::discretize generates ints, not factors.
    }
    # suppose the dataframe is categorical
    # Find simple entropies, divergences and entropies of the uniform marginals. 
    name <-  colnames(df)
    edf <- data.frame(
        name = name, # After an idyosincracy of dplyr, the rownames donot survive a mutate.
        H_Uxi = unlist(lapply(df, function(v){log2(length(unique(v)))})),
        H_Pxi = unlist(lapply(df, function(v){natstobits(infotheo::entropy(v))}))
        ) %>% dplyr::mutate(DeltaH_Pxi = H_Uxi - H_Pxi) 
               #M_Pxi = H_Pxi - VI_Pxi)
    if (ncol(df) == 1){
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
        edf <- mutate( edf,
                      VI_Pxi = sapply(name, function(n){infotheo::condentropy(df[,n], Y=df[, setdiff(name, n)])}),
                      M_Pxi = H_Pxi - VI_Pxi
                      )
    }
    return(edf)
}