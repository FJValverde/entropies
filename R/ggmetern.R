#' @title Grammar-of-Graphics Multivariate entropic ternary plot constructor 
#' 
#' @description 
#' This function creates a ternary plot on entropic coordinates whether 
#' split or not for multivariate sets of random variables.
#' 
#'  @details The axes of the plot are changed according to whether the three
#' aggregate variables "DeltaH_Px", "M_Px" and "VI_Px" are defined (not split) or
#' the split variables "DeltaH_Pxi", "DeltaH_Pxi","M_Pxi" in the dataframe passed as parameter. 
#'   Note that no geometry is imposed on the plot.
#' @details Only the plot and aesthetics are generated, the geometry is not decided. A further 
#' column, including the name of the original variables may be used to annotate the plot. 
#' @param experiments A dataframe with the data to be plotted, typically obtained with \code{entropicCordinates}.
#' To plot the *split entropy triangle* this dataframe must have  at least the following variables \code{c("DeltaHx", "DeltaHx","MIxyX","MIxyY", "VIx", "VIy")}.
#' To plot the *entropy triangle* this dataframe must have  either of the two configurations. 
# # @param split A toggle whether we want to use the split entropy diagram. Defaults to FALSE
#' @return A plot object in the ggplot2 class of plots ready to be plotted.
#' @seealso \code{\link{ggtern}}, \code{\link{entropies}}, \code{\link{entropicCoordinates}}
#' @export
#' @importFrom ggtern ggtern
#' @examples
#' data(UCBAdmissions)
#' experiments <- entropicCoords(entropies(UCBAdmissions)) # Non-split data
#' eT <- ggentropytern(experiments) + geom_point(aes(VIxy,MIxy2,DeltaHxy, colour=Dept, shape=Dept))
#' eT + ggtitle("UCB admissions by department. X=admittance status, Y=gender") # non-split entropy triangle
#' 
# splitExperiments <- entropicCoords(entropies(UCBAdmissions), split=TRUE) # Non-split data
# splitET <- ggentropytern(splitExperiments)
# splitET # split entropy triangle
ggmetern <- function(data, ...) {
    vars <- list(...)
    # If we are to build the split triangle, we have to massage the data:
    if (hasSplitSmetCoords(data)){
        # Create the plot
        ep <- ggtern::ggtern(data, aes(x=VI_Pxi, y=M_Pxi, z=DeltaH_Pxi), vars) +
            theme_rotate(degrees=-60)#Source Entropy Diagrams are upside down!
        # Node labels for the split triangle
        #TlabExp <- expression({italic(M)^{symbol("\242")}}["Xi"])
        TlabExp <- "$\\textit{M'}_{P_{X_i}}"
        #RlabExp <- expression(paste(Delta, "", {italic(H)^{symbol("\242")}}["Xi"]))
        RlabExp <- "$\\Delta\\textit{H'}_{P_{X_i}}"
        #LlabExp <- expression({italic(VI)^{symbol("\242")}}["Xi|Xi"^c])
        LlabExp <-  "$\\textit{VI'}_{P_{X_i}}"
        titleExp <- "Source split entropies"
        # Otherwise, check that it has multivariate source data, then plot
    } else if (hasMultiEntropicCoords(data)){
        ep <- ggtern(data, aes(x=VI_Px, y=M_Px, z=DeltaH_Px), vars) #+ geom_point(...)
        # vertex labels for the non-split multivariate source triangle
        #TlabExp <- expression({italic(M)^{symbol("\242")}}["X"])
        TlabExp <- "$\\textit{M'}_{P_X}"
        #RlabExp <- expression(paste(Delta, "", {italic(H)^{symbol("\242")}}["X"]))
        RlabExp <- "$\\Delta\\textit{H'}_{P_X}"
        #LlabExp <- expression({italic(VI)^{symbol("\242")}}["X"])
        #LlabExp <- "$\\textit{H}_{P_{{X_i | X_i^c}}}"
        LlabExp <-  "$\\textit{VI'}_{P_{X}}"
        titleExp <- "Source entropies"
    } else if (hasCmetEntropicCoords((data))) {
        ep <- ggtern(data, aes(x=VI_P, y=M_P, z=DeltaH_P), vars) #+ geom_point(...)
        # Vertex labels for the channel multivariate entropy triangle
        # TlabExp <- expression({italic(M)^{symbol("\242")}}["XY"])
        # RlabExp <- expression(paste(Delta, "", {italic(H)^{symbol("\242")}}["XY"]))
        # LlabExp <- expression({italic(VI)^{symbol("\242")}}["XY"])
        TlabExp <- "$\\textit{M'}_{P_{XY}}"
        RlabExp <- "$\\Delta\\textit{H'}_{P_{XY}}"
        LlabExp <-  "$\\textit{VI'}_{P_{XY}"
        titleExp <- "Channel entropies"
    } else {
        stop("Non-appropiate data")
    }
    # A basic plot and theme
    ep <- ep + 
        geom_mask() + #<<<<< Puts the mask below any layers to follow, charm by N. Hamilton
        #scale_shape_manual(values=c(1:nrow(data))) + 
        ggtern::theme_rgbw() + 
        ggtern::theme_custom(col.T="forestgreen",col.L="red",col.R="orange") + 
        ggtern::theme(complete=FALSE, 
                       tern.axis.text.show=TRUE,
                       tern.axis.arrow.show=TRUE,
                       tern.axis.clockwise=FALSE) +
        ggtern::Tlab(TlabExp) + 
        ggtern::Rlab(RlabExp) + 
        ggtern::Llab(LlabExp) #+
        #ggtitle(titleExp)
        #ggtern::theme_showarrows() + # This theme overrides later adjustments
    if (hasMultiSplitEntropicCoords(data)){#Source Entropy Diagrams are upside down!
        ep <- ep + theme_rotate(degrees=-60)
    }
    return(ep)
}