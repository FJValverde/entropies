#' An algorithm to build a uniform grid on a random vector
#' 
#' CAVEAT: If \code{nBins} and the dimension of the vector \code{ncol(X)}
#' are too large, this results in a combinatorial explosion of the points in
#' the grid. This grid cannot be managed, e.g. if it is the maximum integer size
#' \code{.Machine$integer.max}.
#' 
#' @param X The data to build the grid over
#' @param nBins The number of points per dimension to build the grid over
#' @param type The type of grid. Recognised values are "uniform", for
#' a uniform grid, an analogue concept to \link[infotheo]{infotheo::discretize}
#' "equalwidth".
#' @seealso \link[infotheo]{infotheo::discretize}
#' @export
multivariate.grid <- function(X,nBins, type=c("uniform")) 
    UseMethod("multivariate.grid")

#' @describeIn multivariate.grid An algorithm to build a uniform grid on a random vector as a data frame
#' 
#' @inheritParams multivariate.grid
#' @importFrom purrrlyr dmap
#' @export
multivariate.grid.data.frame <- function(X, nBins=floor(nrow(X)^(1/3)), type){
    stopifnot(nBins^ncol(X) < .Machine$integer.max)
    switch (type,
            uniform = points <- X %>% # map into a daframe of columns
                map_dfc(
                    function(c){
                        r <- range(c,finite=TRUE,na.rm=TRUE)
                        seq(from=r[1], to=r[2],length.out=nBins)
                    }),
            stop(sprintf("Unimplemented grid type: %s", type))
    )
    pointsList <- as.list(points)
    return(expand.grid(as.data.frame(points)))
}

#' @describeIn multivariate.grid An algorithm to build a uniform grid on a random vector as a matrix
#' 
#' @inheritParams multivariate.grid
#' @importFrom purrrlyr dmap
#' @export
multivariate.grid.matrix <- function(X, nBins=floor(nrow(X)^(1/3)), type){
    switch (type,
            # A uniform distribution of cut points in the finite
            # ranges of the cols of X.
            uniform = points <- 
                purrrlyr::dmap(X, 
                    function(c){
                        r <- range(c,finite=TRUE,na.rm=TRUE)
                        seq(from=r[1], to=r[2],length.out=nBins)
                    }),
            stop(sprintf("Unimplemented grid type: %s", type))
    )
    return(expand.grid(points))
}
