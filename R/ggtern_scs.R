#' @title Grammar-of-Graphics scaled comparison structure ternary plot constructor
#' 
#' @description This is a function to conform a ggtern into a Scaled Comparison Structure
#' triangle.
#' 
#' @details The left axis is a dissimilariyy, the right axis is a similarity and the 
#' bottom axis quantifies the scale. 
#' 
#' @param scst Scaled comparison structure triangle object.
#' @param fancy A switch whether to use a fancier (coloured) or terser (B&W) \code{\link[ggplot2]{theme}} in the plot.
#' Defaults to fancy=TRUE, for exploratory analysis. fancy=FALSE is better for printed matter. 
#' @param is.source A boolean indicating if it is a source triangle, in which case
#' it is rotated 60 degrees, 
#' 
#' @seealso \pkg{ggtern}, \pkg{entropies}, \pkg{entropicCoordinates}
#' @export
#' @importFrom ggtern ggtern theme_bw theme_rgbw theme_custom 
#' @importFrom ggtern theme_latex Tlab Rlab Llab theme_showarrows 
#' @importFrom ggtern theme_rotate geom_mask
ggtern_scs <- function(scst, fancy=FALSE, is.source=FALSE, TlabExp, RlabExp, LlabExp){
    #vars <- list(...)#This passes the rest of the parameters as 
    #listVars <- as.list(vars)
    scst <- scst + geom_mask() #<<<<< Puts the mask below any layers to follow, charm by N. Hamilton
    if (fancy){ # A theme for interactive visualization & exploration
        scst <- scst + ggtern::theme_rgbw() + 
            ggtern::theme_custom(tern.plot.background="white",
                                 tern.panel.background="white",
                                 col.T="forestgreen",col.L="red",col.R="orange")
    }else{# A basic plot and theme for publishing
        scst <- scst + ggtern::theme_light()#bw()
    }
    scst <- scst + 
        ggtern::theme_anticlockwise() +
        # labels by default in latex
        ggtern::theme_latex(TRUE) + #global switch to latex: ON by default
        labs( x = LlabExp, #Tlab and xlab are equivalent (from ggtern_labels)
              y = TlabExp, #Llab and ylab are equivalent
              z = RlabExp  #Rlab and zlab are equivalent
        )
    if (is.source){
        scst <- scst + theme_rotate(degrees=-60)
    }
    scst <- scst + ggtern::theme_showarrows() # last thing to do.
    return(scst)
}
