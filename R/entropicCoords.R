#' Transform entropies into entropic coordinates
#' 
#'   Generate the entropic coordinates from the component entropies of a joint 
#'   count distribution or a contingency matrix.
#' - with split=TRUE, generate independen coordinates for X and Y distributions.
#' @param df A data frame containing values for the entropies related to a joint
#' distribution or sets of them.
#' @param split=FALSE A flag to select obtaining split coordinates.
#' @param purge=FALSE A flag to purge the entropies in \code{df}. If \code{purge=FALSE}
#' the new entropies are added to the input entropies, otherwise they are purged.
#' @param norm=TRUE A flag to request normalized coordinates. If norm=TRUE, the 
#' coordinates, either split or not, are normalized and ready to plot in the 
#' entropy triangle. 
#' @return A data frame with the entropy points for 
#' @seealso \code{entropies}
#' @importFrom dplyr %>% one_of
#' @export
entropicCoords <- function(df, split=FALSE, purge=FALSE, norm = TRUE){
    # 0. signature checking for df. 
    if (!hasSimpleEntropies(df))
        stop("Cannot work out entropic coordinates: some basic entropies are missing")
    # 1. real operation
    if (norm){
        if (split){
            normx <- df$Ux
            normy <- df$Uy
        }else
            norm <- df$Ux + df$Uy
    } else {
        normx <- 1; normy <- 1; norm <- 1
    }
    newDF <- df
    if (purge) newDF <- df %>% select(-(one_of(simpleEntropies)))
    if (split){
        newDF <- newDF %>%
            mutate( Ux = Ux, Uy = Uy,
                    DeltaHx = (Ux - Hx)/normx,
                    DeltaHy = (Uy - Hy)/normy,
                    MIxyX = (Hx + Hy - Hxy)/normx, # May be different due to normalization
                    MIxyY = (Hx + Hy - Hxy)/normy, # May be different due to normalization
                    VIx = (Hxy - Hy)/normx,
                    VIy = (Hxy - Hx)/normy
            )
    }else{
        newDF <- newDF %>%
            mutate(
                Uxy = Ux + Uy,
                DeltaHxy = (Ux - Hx + Uy - Hy)/norm,
                MIxy2 = 2*(Hx + Hy - Hxy)/norm,
                VIxy = (Hxy + Hxy - Hx - Hy)/norm
            )
            
    }
    return(newDF)
}
#coords <- entropicCoords(entropies(N, unit="log2"))
#splitCoords <- entropicCoords(entropies(N, unit="log2"), split=TRUE)
