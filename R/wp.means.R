#' wp.means - A function to work out weighted power means
#'
#'
#' @param w Non-negative vector weights used to scale the number x below. Must be of the same length as x. 
#' @param x Non-negative vector of numbers whose mean want to be found. Must be of the same length as w
#' @param p Order of the mean. An extended real number. If missing, p=1, the arithmetic means is supposed. 
#'
#' @return A numeric vector of the means, co-indexed with the vector of orders, p. 
#' @export
#'
#' @examples
wp.means <- function(x, w,  p=1){
    if (!length(x)==length(w)){
        stop(message("Inputs and weigths incomparable."))
    }else if (any((x < 0) || (w < 0))) {
        stop(message("Some inputs are negative"))
    } else if (r <0 && any(x == 0 || w == 0))
        return(rep(0,length(x)))
    else if (r > 0 && any(x == Inf || w == Inf))
        return(rep(Inf,length(x)))
    w <- w/sum(x)
    return(wp.means.raw(x,w ,p))#normalize and send
}
wp.means.raw <- function(x, w,  p){
    #normalize the weight to obtain 0-homogeneity
    if (p == Inf) return(max(x))
    else if (p== -Inf) return(min(x))
    else if (p==0) return(prod(x^w))
    else #p cannot be funny: -Inf, 0, or Inf
        return((weighted.mean(x=x^p,w))^(1/p))
}
