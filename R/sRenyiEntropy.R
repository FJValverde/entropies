#' Shifted Renyi entropy calculations. 
#'
#' Calculate the shifted Rényi entropy of a vector of orders. Recall r=0 is 
#' Shannon's! By default, orders -Inf, -1, 0, 1 and Inf are included. 
#' 
#' @param x A vector of positive numbers, possibly a probability mass function.
#' @param orders The shifted orders. Recall Shannon's is r=0. If a vector of orders are given, they are all returned in the same orders.
#'
#' @return Two coindexed vectors $entropies in base 2 and $orders at which the entropies appear. 
#' @export
#'
#' @examples x <- 1:5 #An unbalanced distribution. 
#' @examples rsults <- sRenyiEntroopy(x,c()) #Return the "5" summary spectrum
#' @examples 2^(-results$entropies) #This is the equivalent probability function.
sRenyiEntropy <- function(x,orders=c(),base=2){
    w <- x/sum(x)
    wp.means <- function(p){#p cannot be funny: -Inf, 0, or Inf
        return((weighted.mean(x=x^p,w))^(1/p))
    }
    # Todo: check that all are not zero!
    # Todo: check what happens when some of them are infinite.
    porders <- sort(unique(c(orders[orders > 0], 1)))
    norders <- sort(unique(c(orders[orders < 0], -1)))
    #orders <- sort(unique(c(orders,c(-1,0,1)))) # Special values always gets added
    nentropies <-  sapply(norders, wp.means)
    pentropies <-  sapply(porders, wp.means)
    # Go over orders working out the entropies using the weighted means on the numbers.
    # for(i in 1:length(orders)){
    #     entropies[i] <- weighted.mean(x^orders[i], w=x)#second arguments are the weights
    # }
    # Always add at beg and end the max and min entropies.
    entropies <- c(min(x), 
                   nentropies, 
                   prod(x^w), # geometric, for Shannon's
                   pentropies,
                   max(x))
    orders <- c(-Inf, norders, 0, porders, Inf)
    if (base==2)
        return(list(
            entropies=-log2(entropies),
            orders=orders))
    else#base != 2
        return(list(
            entropies=-log2(entropies)/log2(base),
            orders=orders))
}