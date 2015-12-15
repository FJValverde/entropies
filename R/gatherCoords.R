#' A function to massage the split coordinates back to non-split with an indication of origin
#'
#' @param data The data in splitEntropies format.  
#' @export
#' @importFrom dplyr mutate
#' @importFrom dplyr select
gatherCoords <- function(data, limit=TRUE){
    if (!(hasDerivedSplitEntropies(data))){
        stop("Non-appropiate data")
    }
    xData <- data  %>% 
        mutate(DeltaHxy=DeltaHx, MIxy2 = MIxyX, VIxy = VIx, Var="X",
               DeltaEnd=NA, MIEnd=NA, VIEnd=NA, linetype=NA) %>%
        select(-one_of(derivedSplitEntropies))
    yData <- data %>%
        mutate(DeltaHxy=DeltaHy, MIxy2 = MIxyY, VIxy = VIy, Var="Y",
               DeltaEnd=NA, MIEnd=NA, VIEnd=NA, linetype=NA) %>%
        select(-one_of(derivedSplitEntropies))
    pathData <- data %>% 
        mutate(DeltaHxy=DeltaHx, DeltaEnd=DeltaHy,MIxy2=MIxyX, MIEnd=MIxyY, 
               VIxy=VIx, VIEnd=VIy, Var="Path", linetype=2) %>%
        select(-one_of(derivedSplitEntropies))
    if (limit){
        limitData <- data %>%
            mutate(DeltaHxy=DeltaHx, MIxy2=0, VIxy=1-DeltaHxy, 
                   DeltaEnd=DeltaHxy, MIEnd=1-DeltaHxy, VIEnd=0, 
                   Var="Limit", linetype=1) %>%
            select(-one_of(derivedSplitEntropies))
        pathData <- rbind(pathData, limitData)
    }
    return(rbind(xData, yData, pathData))
}