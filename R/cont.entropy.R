#' Calculates the continuous entropy of data (generic)
#' 
#' @param d The data structure to obtain the entropy from. This has to 
#'          be ALL continuous data.
#' @seealso \code{\link[entropy]{entropy::entropy}, 
#' \link[infotheo]{infotheo::entropy}} for discrete data, and 
#' \code{\link[entropies]{raw.entropies}} for mixed data. 
#' @export
cont.entropy <- function(d, ...) UseMethod("cont.entropy") 

#' @describeIn cont.entropy Calculates the continuous entropy of data matrices by the KNN 
#' method
#' 
#' These matrices are supposed to have one observation per row and
#' one feature per column.
#' 
#' CAVEAT: if the columns of the matrix are not continuous it may
#' return a nonsensical result.
#' @inheritParams cont.entropy
#' @param k The number of neighbours to test the algorithm with. 
#' @param disType A parameter distinguishing the object of estimation: 
#'   \itemize{
#'   \item With \code{"P_X"} (default), the continuous entropy 
#'   of the data \code{d} is requested. 
#'   
#'   \item With \code{"U_X_KNN"} the KNN estimator
#'   used on a uniform distribution of points obtained from the ranges 
#'   of \code{d}.
#'   
#'   \item With \code{"U_X_discrete"} the (arbitray) entropy of a discretization carried out
#'   by \code{\link[infotheo]{infotheo::discretize}} is carried out.
#'   
#'   \item WIth \code{"U_X_ranges"} the sum of the logs of the ranges of the 
#'   different RVs. This may be negative, as the differential entropy happens to be sometimes. 
#'   }
#'    
#' @importFrom FNN entropy
#' @importFrom IndepTest KLentropy L2OptW
#' @importFrom purrr map_int
#' @importFrom infotheo discretize 
#' @export
cont.entropy.matrix <- function(d, k, disType="P_X"){
    if (nrow(d) == 1)
        return(0.0)#just one class possible
    else{
        match.arg(disType, choices=c("P_X",
                                     "U_X_sample",
                                     "U_X_generate", 
                                     "U_X_ranges",
                                     "U_X_discrete"))
        # Q.FVA: See what alternatives are possible
        #FVA: FNN::entropy seems to be in Hartleys, and so is IndepTest::entropy
        value <- 
            switch (disType,
                    P_X = {
                        # and we want to believe that knn.dist uses "kd_tree"
                        est <- 
                            IndepTest::KLentropy(
                                d, k#, weights = TRUE, stderror=TRUE
                             )
                        #)$Estimate#returns nonsense when there are Inf's in est.
                        # FNN::entropy(
                        #     d, 
                        #     k, #obtains k measures, so returns a vector
                        #     algorithm="kd_tree"
                        # )
                        weights <- L2OptW(k, ncol(d)) 
                        maskInf <- is.infinite(est$Unweighted)
                        if (any(maskInf)){
                            warning("Some entropies in IndepTest::KLentropy were infinite!")
                            #value <- (weights[mask])/sum(weights[mask]) %*% est[mask]
                            #est[maskInf] <- 0
                            weights[maskInf] <- 0#Remove the coefficients
                        }
                        # Renormalize weights and estimate
                        sum((weights/sum(weights)) * est$Unweighted, na.rm=TRUE)
                    },
                    U_X_ranges = {
                        r <- sapply(d, function(x) range(x, finite=TRUE))
                        sum(log(abs(r[2,] - r[1,])), na.rm=TRUE)
                    },
                    U_X_sample = {
                        #sample from a multivariate uniform
                        # Go over every range generating nrow(d) samples
                        #r <- sapply(X, function(x) range(x, finite=TRUE))
                        points <- apply(d, 2, function(x){
                            r <- range(x, finite=TRUE)
                            runif(nrow(d), min=r[1], max=r[2])
                            # FVA: it looks this increases with increasing 
                        })
                        # Work out the KLentropy of the sample
                        # FVA: the following does not work
                        #IndepTest::KLentropy(points, k, weights=TRUE)$Estimate
                        # FVA: the next one is an interim estimate, pending a more thorough
                        # exploration of the issue
                        IndepTest::KLentropy(points, k)$Unweighted[k]
                    },
                    # # The following fails long tables with many bins
                    U_X_generate =
                        IndepTest::KLentropy(
                            multivariate.grid(d,nBins=2,type="uniform"), 
                            k=1
                        )$Unweighted[1]
                    ,
                    # Rationale for below:
                    # Obtain a discretization, then just count the actual bins
                    # Pity: There is no reduce for data.frames!
                    # Suppposing Equalfreq is the analogue of KNN in discretizatinon
                    U_X_discretize =
                        log(reduce(
                            infotheo::discretize(d,
                                                 #nbins=nrow(d)^(1/3),#default bins
                                                 disc = "equalfreq"),#<----------
                            function(acc,v) acc*n_distinct(v),
                            .init=1.0)
                        ),
                    # UU_X = return(log2(prod(
                    #     map_int(
                    #         infotheo::discretize(d,
                    #                              #nbins=nrow(d)^(1/3),#default bins
                    #                              #disc = "equalwidth"),
                    #                              disc = "equalfreq"), #seems to be the analogue
                    #         n_distinct)
                    #     ))),
                    stop(sprintf("Unknown distribution type %s", disType))
            )#switch to assign to value
        return(value/log(2))
    }
}

#' @describeIn cont.entropy Calculates the continuous entropy of a data frame using the KNN
#' estimate.
#' 
#' The rows in the data frame index observations and the columns 
#' features. 
#' 
#' CAVEAT: if the columns of the matrix are not continuous it may
#' return a nonsensical result. Always check for this with e.g. 
#' \code{all(map_lgl(d,is.numeric))}
#' @inheritParams cont.entropy
#' @export
cont.entropy.data.frame <- function(d,...){
    if (nrow(d) != 1)
        return(cont.entropy(as.matrix(d),...))
    else
        return(0.0)
}
