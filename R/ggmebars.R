#' @title Grammar-of-Graphics Multivariate entropy stacked bar constructor
#' 
#' @description 
#' This function creates a stacked bar graph of entropies issued from \code{\link{sentropies}}.
#' 
#'  @details The \code{\link{geom_bar}} geometry of \code{\link{ggplot2}} is imposed on the 
#'  
#' @param smdef A source multisplit entropy dataframe to be plotted, typically obtained with \code{\link{sentropies}.
#' @return A plot object in the ggplot2 class of plots ready to be plotted.
#' @seealso \code{\link{ggtern}}, \code{\link{ggmeter}}, \code{\link{ggplot2::geom_bar}}
#' @export
#' @importFrom tidyr gather
#' @import dplyr ggplot
#' @examples
#' data(iris)
#' sme <- sentropies(iris) # Considers class just another feature
#' ggmebars(sme[-6,])
# First get the proper tall data, modifying the recipe in p. 34. "R graphics..."
library(tidyr)
ggmebars <- function(smedf, excludeAggregate=FALSE, proportional=FALSE){
    # 1. THe recipe to build a stacked bar graph
    if (!hasMultiSplitEntropicCoords(smedf))# Needs to be split source entropy!!
        stop("Data do not represent source multivariate entropies")
    if (excludeAggregate){
        smedf <- filter(smedf, name != "@AGGREGATE")
    } else {
        totalRow <- which(smedf$name == "@AGGREGATE") # find aggregate rows
        if (length(totalRow) > 1)
            stop("Cannot handle more than one triangle decomposition")
    }
    if(proportional){
        #The recipe to build a proportional stacked bar graph
        smedf <- smedf %>%
            mutate(DeltaH_Pxi = DeltaH_Pxi/H_Uxi * 100,
                   M_Pxi=M_Pxi/H_Uxi * 100,
                   VI_Pxi=VI_Pxi/H_Uxi * 100)
        
    }
    smedfTall <- smedf %>% 
        gather(component, sme, VI_Pxi, M_Pxi, DeltaH_Pxi)
    # Choose the ordering for the bars
    ordering <- smedf$name[sort(smedf$VI_Pxi + smedf$M_Pxi, 
                                decreasing=TRUE, 
                                index.return=TRUE)$ix]
    ## Now create the plot:
    p <- ggplot(smedfTall, aes(x=name, y=sme, fill=component)) +
        geom_bar(stat="identity") +
        scale_x_discrete(limits=ordering) +
        scale_fill_manual(values=c("#FFFF88", "#99FF33", "#FF3300")) 
    #scale_fill_manual(values=c("yellow", "green", "red")) 
    #Colours from: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
    #Q: why does it not parallel the gather order above?
    return(p)
}
