#' Shifted Renyi entropy calculations. 
#'
#' Calculate the shifted Renyi entropy of a vector of orders. Recall r=0 is 
#' Shannon's! By default, orders -Inf, -1, 0, 1 and Inf are included. 
#' 
#' Uses the relationship between the shifted entropy for distributions of probability
#' and mass to avoid overflow.
#' 
#' @param x A vector of positive numbers, possibly a probability mass function.
#' @param orders The shifted orders. Recall Shannon's is r=0. If a vector of orders are given, they are all returned in the same orders.
#'
#' @return Two coindexed vectors $entropies in base 2 and $orders at which the entropies appear. 
#' @export
#'
#' @examples x <- 1:5 #An unbalanced distribution. 
#' @examples results <- sRenyiEntropy(x,c()) #Return the "5" summary spectrum
#' @examples 2^(-results$entropies) #This is the equivalent probability function.
# Todo: check that all are not zero!
# Todo: check what happens when some of them are infinite.
sRenyiEntropy <- function(m,orders=c(),base=2){
    mass <- sum(m)
    m <- m/mass#q_0(m) escort probability to avoid overflow
    #w <- x/mass#this is already done by the weighted mean!
    holder.wp.means <- function(p){#p cannot be funny: -Inf, 0, or Inf
        return((weighted.mean(x=m^p,m))^(1/p))#weights normalized internally
    }
    if (!is_empty(orders)){
        orders <- orders[orders != 0]#Dispose of zeroes
        orders <- orders[is.finite(orders)]#Dispose of infinities
    }
    orders <- sort(unique(c(orders, -1, +1)))
    porders <- (orders > 0)#non-null
    norders <- (orders < 0)#non-null
    h.means <- sapply(orders, holder.wp.means)
    h.means = c(min(m), 
                  h.means[norders], #neg. order entropies
                  prod(m^(m)), # geometric, for Shannon's
                  h.means[porders],#pos. order entropies
                  max(m))
    return(list(#If the base is 2, the res is prepared for return
        logmass = -log(mass, base),
        entropies = -log(h.means,base) - log(mass, base),
        # Always add at beg and end the max and min entropies.
        orders = c(-Inf, orders[norders], 0, orders[porders], Inf)
    ))
    # if (base!=2){
    #     res$entropies <- res$entropies/log2(base)
    #     res$logmass <- res$logmass/log2(base)
    # }
    # return(res)
}