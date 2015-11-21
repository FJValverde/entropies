#' Entropy decomposition of a contingency matrix
#' 
#' Given a contingency matrix, provide one row of entropy coordinates. 
#' NOTE: the reference variable has to index the ROWS of the table, while the predicted
#' variable indexes the columns, unlike, e.g. \code{\link[caret]{contingencyTable}}
#' @param N An n-contingency matrix where n > 2
#' @param unit The logarithm to be used in working out the entropies as per 
#' \code{entropy}. Defaults to "log2".
#' @return  A dataframe with the entropies of the marginals
#' @details Unless specified by the user explicitly, this function uses base 2 
#' logarithms for the entropies.
#' @seealso \code{\link[entropy]{entropy}}
#' @export
entropies <- function(data, ...) UseMethod("entropies")

#' Entropy decomposition of a contingency matrix
#' 
#' Given a contingency matrix, provide one row of entropy coordinates. 
#' @export
#' @importFrom entropy entropy
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
    require(entropy)
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
        Hx <- apply(Nx, 2, function(x) {do.call(entropy, c(list(y=x), vars)) })
        Ny <- margin.table(Nxy, c(2,3:length(dims)))
        Hy <- apply(Ny, 2, function(x) {do.call(entropy, c(list(y=x), vars)) })
        Ux <- apply(Nx, 2, function(x) { log2(length(x))})
        Uy <- apply(Ny, 2, function(x) { log2(length(x))})
        Hxy <- apply(Nxy, 3, function(x) {do.call(entropy, c(list(y=x), vars))})
        df <- data.frame(Ux = Ux, Uy = Uy, Hx = Hx, Hy = Hy, Hxy = Hxy)
        df <- cbind(df, dimnames(Nxy)[3:])# Keep the third  and greater dimension's names
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