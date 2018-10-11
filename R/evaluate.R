#' A function to evaluate confusion matrices and contingency tables. 
#' 
#' Provides also the EMA and NIT over that provided by perplexities. 
#' 
#' @param data Either a \code{\link[base]{table}} working as an n-way contingency 
#' table or a \code{\link[caret]{confusionMatrix}}
#' @return At least, the concatenation of the entropies (whether split or not), the \code{EMA}
#'  and \code{NIT} rate as explained in \cite{val:pel:14a}.
#' @export
evaluate <- function(data, ...) UseMethod("evaluate")

#' A function to evaluate the entropic coordinates of a distribution
#' @export
evaluate.data.frame <- function(data, ...){
    if(hasCmetEntropicCoords(data)){
        return(perplexities(data) %>% ##filter(type!="XY") %>% 
                   mutate(EMA=1/kx_y, NIT=muxy/k))
    } else
        stop("Missing fields in entropy data to work out performance indicators.")
}

#' A function to evaluate a confusion matrix
#' 
#' @description  The criteria evaluated at present are:
#' \itemize{
#' \item  entropic quantities, as issued from \code{jentropies}
# \item accuracies and kappa (and their intervals), e.g. as issued from 
# \code{\link[caret]{confusionMatrix}}.
#' \item perplexities, as issued from \code{\link{perplexities}}
#' \item Expected Modified Accuracy (EMA) and Normalized Information Transfer (NIT) rate
#' }
#' @param cm A confusion matrix as per \code{\link[caret]{confusionMatrix}}
##' @param split=FALSE Whether to evaluat the split entropies or not.
#' @return The concatenation of the entropies (whether split or not), the
#'  \code{cm$overall} information from confusion matrix cm, and the \code{EMA}
#'  and \code{NIT} rate as explained in \cite{val:pel:14a}.
#' @importFrom caret confusionMatrix
#' @export
evaluate.confusionMatrix <- function(cm, ...){
    #require(caret) # for class "confusionMatrix" # Do NOT use require in packages!
    # if (class(cm) != "confusionMatrix")
    #     stop("evaluate.confusionMatrix: not a confusion matrix")
    #vars <- list(...) # in case we have to split
    #cmEntropies <- entropies(cm, vars)
    #FVA: obsolete cmEntropicCoords <- entropicCoords(entropies(cm), ...)
    #cmEntropicCoordds <- jentropies2d.table(cm, ...)
    #cmPerplexities <- perplexities(cmEntropicCoords)
    # return(cbind(data.frame(as.list(cm$overall)),
    #              cmEntropicCoords, 
    #              cmPerplexities,
    #              EMA = 1/cmPerplexities$kx_y, 
    #              NIT = cmPerplexities$muxy/cmPerplexities$k
    #              )
    # )
    #return(perplexities(data) %>% mutate(EMA=1/kx_y, NIT=muxy/k))
    return(evaluate(jentropies(cm)))
}

#' A primitive to evaluate a contingency table
#' 
#' @description  The criteria evaluated at present are:
#' \itemize{
#' \item  entropic quantities, as issued from \code{entropies} and \code{entropicCoords}.
#' \item perplexities, as issued from \code{\link{perplexities}}
#' }
#' @param cm A contingency table as per \code{\link[base]{table}}
##' @param split=FALSE Whether to evaluat the split entropies or not.
#' @examples evaluate.table(UCBAdmissions) # from package datasets
#' @export
evaluate.table <- function(cm, ...){
    # #data <- as.table(cm) # Is this necessary after dispatch?
    # dataEntropies <- jentropies(data,...)
    # dataPerp <- perplexities(dataEntropies)
    return(evaluate(jentropies(cm)))
    # return(perplexities(data) %>% mutate(EMA=1/kx_y, NIT=muxy/k))
    # return(cbind(dataEntropies, 
    #              dataPerp, 
    #              EMA = 1/dataPerp$kx_y, 
    #              NIT = dataPerp$muxy/dataPerp$k
    #              )
    #        )
}