#' @title Entropic ternary plot constructor 
#' 
#' @description 
#' This function creates a basic ternary plot on entropic coordinates whether 
#' split or not.
#' 
#'  @details The axes of the plot are changed according to whether the three
#' aggregate variables "DeltaHxy", "MIxy2" and "VIxy" are defined (not split) or
#' the split variables "DeltaHx", "DeltaHx","MIxyX", "MIxyY", "VIx", "VIy" are defined in
#' the dataframe passed as parameters. 
#'   Note that no geometry is imposed on the plot.
#' @details Only the plot and aesthetics are generated, the geometry is not decided. 
#' @param experiments A dataframe with the data to be plotted, typically obtained with \code{entropicCoordinates}.
#' To plot the *split entropy triangle* this dataframe must have  at least the following variables \code{c("DeltaHx", "DeltaHx","MIxyX","MIxyY", "VIx", "VIy")}.
#' To plot the *entropy triangle* this dataframe must have  at least the following variables: 
#' \code{c("DeltaHxy", "MIxy2", "VIxy")}.
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
ggentropytern <- function(data, ...) {
    require(ggtern)
    #varNames <- names(data)
    #isSplitET <- all(c("DeltaHx", "DeltaHx","MIxyX", "MIxyY", "VIx", "VIy") %in% varNames)
    #isNonSplitET <- all(c("DeltaHxy", "MIxy2", "VIxy") %in% varNames)
    if (!(hasDerivedSplitEntropies(data) | hasDerivedNonSplitEntropies(data))){
        stop("Non-appropiate data")
    }
    #vars <- list(...)
    # If we are to build the split triangle, we have to massage the data:
    if (hasDerivedSplitEntropies(data)){
        vars <- list(...) #; vars$limit <- limit
        #Ensure the same columns exist prior to merging.
        limit <- !is.null(vars$limit)&(vars$limit)
        newData <- gatherCoords(data, limit) #do.call(gatherCoords, data, vars) #gatherCoords(data, ...)
        newData$linetype <- factor(newData$linetype)
        #print(newData)
        # Create the plot
        ep <- ggtern(data=newData[which(newData$Var=="X" | newData$Var=="Y"), ], 
                     aes(VIxy,MIxy2,DeltaHxy))
        # Plot the points
        ep <- ep + geom_point(size=2, aes(shape=Var), ...) +
            scale_shape_manual(values=c(2, 6)) # 2 is upwards triangle, 6 is downwards
        # Plot the paths
        ep <- ep + geom_segment(data=newData[which(newData$Var=="Path"), ],
                                aes(xend=VIEnd, yend=MIEnd, zend=DeltaEnd),
#                                    linetype=factor(2)),
                                size=0.25, ...) 
        if (limit){
            ep <- ep + 
                geom_segment(data=newData[which(newData$Var=="Limit"), ],
                             aes(xend=VIEnd, yend=MIEnd, zend=DeltaEnd),
                             #                                    linetype=factor(2)),
                             size=0.25, colour="grey")
        }
    } else {
        ep <- ggtern(data, aes(VIxy,MIxy2,DeltaHxy)) +
            geom_point(...)
    }
    # A basic plot and theme
    ep <- ep + 
        ggtern::theme_rgbw() + 
        ggtern::theme_custom(col.T="forestgreen",col.L="red",col.R="blue") + 
        ggtern::theme(complete=FALSE, 
                      axis.tern.showlabels=FALSE,
                      axis.tern.showarrows=TRUE,
                      axis.tern.clockwise=FALSE)
    if (hasDerivedNonSplitEntropies(data)){
        ep <- ep + 
            Tlab(expression(2*{italic(MI)^{symbol("\242")}}[XY])) +
            Rlab(expression(paste(Delta, "", {italic(H)^{symbol("\242")}}[XY]))) +
            Llab(expression({italic(VI)^{symbol("\242")}}[XY]))
    } else { # Split triangle!
        ep <- ep + 
            Tlab(expression({italic(MI)^{symbol("\242")}}[XY])) +
            Rlab(expression(paste(Delta, "", {italic(H)^{symbol("\242")}}[X], 
                                  ",",
                                  Delta, "", {italic(H)^{symbol("\242")}}[Y]))) +
            Llab(expression(paste({italic(H)^{symbol("\242")}}[X||Y],
                                  ",",
                                  {italic(H)^{symbol("\242")}}[Y|X])))
    }
    return(ep)
}