#' Basic joint entropy decomposition for pairs of random vectors with 
#' potentially mixed continuous and discrete components.
#' 
#' @param X The first mode of data
#' @param Y The second mode of data
#' @return A data frame of exactly three rows with the main entropies of the 
#'    random vectors with colums
#'  \itemize{
#'    \item \code{type} With values "X", "Y" or "XY" to distinguish the origin
#'    of the entropic coordinates in a row. 
#' 
#'    \item \code{H_P} Double, describing the entropy of the multivariate 
#'    distribution
#'  
#'    \item \code{H_U} Double, describing the entropy of an equivalent uniform 
#'    distribution.
#'  
#'    \item \code{DeltaH_P} Double, describing the decrement in entropy in the CMET.
#'  
#'    \item \code{M_P} Double, the total correlation in the CMET. 
#'  
#'    \item \code{VI_P} Double, describing, for the row with \code{type="XY"} the
#'    variation of information, and for the rows with \code{type in c('X', 'Y')}
#'    the remanent information of each random vector. 
#'  }
#' @export
raw.jentropies <- function(X, Y, ...) UseMethod("raw.jentropies") 

#' @describeIn raw.entropies Raw multivariate Joint Entropy decomposition 
#' of two  data frames encapsulating random vectors. 
#' @export
raw.jentropies.data.frame <- function(X,Y){
    if (ncol(X) == 0 | nrow(X) == 0 ) 
        stop("Can only work with non-empty data.frames X!")
    if (ncol(Y) == 0 | nrow(Y) == 0 )
        stop("Can only condition on non-empty data.frame Y! ")
    if (nrow(X) != nrow(Y) )
        stop("Can only condition on variable lists with the same number of instances!")
    # 1. Obtain the base entropies
    Xentropies <- raw.entropies(X)#licenced by the first condition above
    Yentropies <- raw.entropies(Y)#licenced by the second condition above
    # prevent the cbind from failing
    colnames(X) <- map_chr(colnames(X), function(x) sprintf("x%s", x))
    colnames(Y) <- map_chr(colnames(Y), function(y) sprintf("y%s", y))
    XYentropies <- raw.entropies(cbind(X,Y))#licenced by the third condition
    H_XY <- sum(XYentropies$H_P_Xd, XYentropies$H_P_Xc, na.rm=TRUE)
    # 2. Build the overall data frame building the coordinates
    edf <- data.frame(
        type = c("X", "Y"),
        H_U = c(sum(Xentropies$H_U_Xd, Xentropies$H_U_Xc, na.rm=TRUE),
                sum(Yentropies$H_U_Xd, Yentropies$H_U_Xc, na.rm=TRUE)),
        H_P = c(sum(Xentropies$H_P_Xd, Xentropies$H_P_Xc, na.rm=TRUE),
                sum(Yentropies$H_P_Xd, Yentropies$H_P_Xc, na.rm=TRUE))
    ) %>% dplyr::mutate(
        DeltaH_P = H_U - H_P,
        VI_P = rev(H_XY - H_P),# FVA: think this formula carefully.
        M_P = H_P - VI_P
    )
}