#' @title Grammar-of-Graphics Multivariate entropic ternary plot constructor 
#' 
#' @description 
#' This function creates a ternary plot on entropic coordinates whether 
#' split or not for multivariate sets of random variables.
#' 
#' @details The axes of the plot are changed according to whether the three
#' aggregate variables "DeltaH_Px", "M_Px" and "VI_Px" are defined (not split) or
#' the split variables "DeltaH_Pxi", "DeltaH_Pxi","M_Pxi" in the dataframe passed as parameter. 
#'   Note that no geometry is imposed on the plot.
#' @details Only the plot and aesthetics are generated, the geometry is not decided. A further 
#' column, including the name of the original variables may be used to annotate the plot. 
#' @param experiments A dataframe with the data to be plotted, typically obtained with \code{entropicCordinates}.
#' To plot the *split entropy triangle* this dataframe must have  at least the following variables \code{c("DeltaHx", "DeltaHx","MIxyX","MIxyY", "VIx", "VIy")}.
#' To plot the *entropy triangle* this dataframe must have  either of the two configurations. 
# # @param split A toggle whether we want to use the split entropy diagram. Defaults to FALSE
#' @param fancy A switch whether to use a fancier (coloured) or terser (B&W) \code{\link{ggplot2::theme}} in the plot.
#' Defaults to fancy=TRUE, for exploratory analysis. fancy=FALSE is better for printed matter. 
#' @return A plot object in the ggplot2 class of plots ready to be plotted.
#' @seealso \code{\link{ggtern}}, \code{\link{entropies}}, \code{\link{entropicCoordinates}}
#' @export
#' @import ggtern
#' @import latex2exp
# @examples
# data(UCBAdmissions)
# experiments <- entropicCoords(entropies(UCBAdmissions)) # Non-split data
# eT <- ggmetern(experiments) + geom_point(aes(VIxy,MIxy2,DeltaHxy, colour=Dept, shape=Dept))
# eT + ggtitle("UCB admissions by department. X=admittance status, Y=gender") # non-split entropy triangle
# 
# splitExperiments <- entropicCoords(entropies(UCBAdmissions), split=TRUE) # Non-split data
# splitET <- ggentropytern(splitExperiments)
# splitET # split entropy triangle
ggmetern <- function(data, fancy=TRUE, ...) {
    vars <- list(...)
    if (hasSplitSmetCoords(data)) {
        # Create the plot
        ep <- ggtern::ggtern(data, aes(x=VI_Pxi, y=M_Pxi, z=DeltaH_Pxi), vars) #+
        # Node labels for the split triangle
        TlabExp <- "$\\textit{M'}_{P_{X_i}}$"
        RlabExp <- "$\\Delta\\textit{H'}_{P_{X_i}}$"
        LlabExp <-  "$\\textit{H'}_{P_{X_i|X_i^c}}$"
        titleExp <- "Source Multivariate split entropies"
    # Otherwise, check that it has multivariate source data, then plot
    } else if (hasAggregateSmetCoords(data)){
        ep <- ggtern(data, aes(x=VI_Px, y=M_Px, z=DeltaH_Px), vars) #+ geom_point(...)
        # vertex labels for the non-split multivariate source triangle with total correlation
        TlabExp <- "$\\textit{M'}_{P_X}"
        RlabExp <- "$\\Delta\\textit{H'}_{\\Pi_{X}}"#\\overline X does not work!
        LlabExp <-  "$\\textit{VI'}_{P_{X}}"
        titleExp <- "Aggregate Source Multivariate  entropies"
    } else if (hasDualAggregateSmetCoords(data)){
        ep <- ggtern(data, aes(x=VI_Px, y=D_Px, z=DeltaH_Px), vars) #+ geom_point(...)
        # vertex labels for the non-split multivariate source triangle
        TlabExp <- "$\\textit{D'}_{P_X}"
        RlabExp <- "$\\Delta\\textit{H'}_{P_X}"#\\overline X does not work!
        LlabExp <-  "$\\textit{VI'}_{P_{X}}"
        titleExp <- "Dual Aggregate Source Multivariate entropies"
    } else if (hasCmetEntropicCoords((data))) {
        ep <- ggtern(data, aes(x=VI_P, y=M_P, z=DeltaH_P), vars) #+ geom_point(...)
        # Vertex labels for the channel multivariate entropy triangle
        TlabExp <- "\\textit{I'}_{P_{XY}}"#FVA 24/07/17: Changed text to reflect theoretical work.
        RlabExp <- "$\\Delta\\textit{H'}_{P_{XY}}$"
        LlabExp <-  "$\\textit{VI'}_{P_{XY}$"
        titleExp <- "Aggregate Channel Multivariate entropies"
    } else {
        stop("Non-appropiate data")
    }
    ep <- ep + geom_mask() #<<<<< Puts the mask below any layers to follow, charm by N. Hamilton
    if (fancy){ # A theme for interactive visualization & exploration
        ep <- ep + ggtern::theme_rgbw() + 
            ggtern::theme_custom(col.T="forestgreen",col.L="red",col.R="orange")
    }else{# A basic plot and theme for publishing
        ep <- ep + ggtern::theme_bw()
    }
    ep <- ep + 
        ggtern::theme(complete=FALSE, 
                       tern.axis.text.show=TRUE,
                       tern.axis.arrow.show=TRUE,
                       tern.axis.clockwise=FALSE) +
        ggtern::theme_latex(TRUE) + #global switch to latex: ON by default
        ggtern::Tlab(TlabExp) + 
        ggtern::Rlab(RlabExp) + 
        ggtern::Llab(LlabExp) #+
        #ggtitle(titleExp) #otherwise this would be too intrusive here!
        #ggtern::theme_showarrows() + # This theme HERE overrides later adjustments
    if (hasSplitSmetCoords(data) || 
        hasAggregateSmetCoords(data) || 
        hasDualAggregateSmetCoords(data)){#Source Entropy Diagrams are upside down!
        ep <- ep + theme_rotate(degrees=-60)
    }
    ep <- ep + ggtern::theme_showarrows() # last thing to do.
    return(ep)
}