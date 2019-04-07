#' Basic entropy estimation for a random vector with discrete and
#' continuous variables.
#' 
#' @param X A data structure with columns for random variables and rows for
#'    observations/instances of the random vector. 
#' @return Will decompose the entropies due to the discrete part and the
#'    continuous part by returning a single-row data.frame:
#'    \itemize{
#'      \item If there is a discrete component, it will return the discrete
#'      entropy of this part as two columns:
#'      \enumerate{
#'      \item \code{H_P_Xd} For the entropy of the discrete distribution formed
#'      by the values of the observations in the discrete variables. 
#'      
#'      \item \code{H_U_Xd} The uniform entropy of the discrete variables, as obtained
#'      from the logarithm of their (discrete) range measures (distinct values.)
#'      }
#'      If there is no discrete component, it will return \code{NA} in those 
#'      columns. 
#'      
#'      \item If there is a continuous component, it will use an estimator 
#'      of the entropy either conditioned on the discrete part, if there is one, 
#'      or unconditioned, if there is none. In any case, it will try to return two 
#'      components also:
#'      \enumerate{
#'      \item \code{H_P_Xc} For the entropy of the continuous part estimated 
#'      accordingly (difficult)
#'      
#'      \item \code{H_U_Xc} For the entropy of a  variable more-or-less "uniform"
#'      over the range of \code{Xc}. In some cases this might be impossible due to
#'      combinatorial effects and only an estimate will be returned.
#'      }
#'      If there is no continuous component,  it will return \code{NA} in those 
#'      columns. 
#'    }
#' @export
raw.entropies <- function(X, ...) UseMethod("raw.entropies")

#' @describeIn raw.entropies For random vector encoded as a data.frame with one observation
#'   per row and one column per random variable. 
#' @importFrom rlang .data
#' @importFrom dplyr select group_by
#' @importFrom purrr map_lgl map_int
#' @importFrom tools assertCondition
#' @export
raw.entropies.data.frame <- function(X, k=ncol(X)){
    #TODO: add an option .method for estimating continuous entropy.
    # "discretize" : bad option, but sometimes the only available
    # "knn": better for continuous entropy, but then we have to decide
    # on the \code{k} parameter.
    if (ncol(X) == 0 | nrow(X) == 0 )
        stop("Can only work with non-empty data.frames X!")
    thisResult <- data.frame(
        H_P_Xd=NA_real_, H_U_Xd=NA_real_, 
        H_P_Xc=NA_real_, H_U_Xc=NA_real_)
    if(nrow(X) == 1){
        warning(sprintf("calling raw.entropis on data.frame with one row!"))
    }
    # Detect the continuous part 
    cPart <- map_lgl(X,is.double)
    # Detect the discrete part
    # FVA: The eternal problem with R: this function is !is.double(x)
    #dPart <- map_lgl(X,function(x) is.integer(x) || is.factor(x))
    #dPart <- map_lgl(X,function(x) !is.double(x))
    #dPart <- sapply(X, function(x) is.integer(x) || is.factor(x))
    # The following logic shows that the discrete part is pivotal
    if (all(cPart)){#Random vector composed only of continuous RV
        #assert_that(all(cPart). msg="Continuous RV!")
        #thisResult$H_U_Xd <- NA_real_# No discrete component
        #thisResult$H_P_Xd <- NA_real_
        thisResult$H_P_Xc <- cont.entropy(X, k, disType="P_X")
        #TODO: FVA: estimate of uniform missing.
        # U_x <- multivariate.grid(X,type="uniform")
        thisResult$H_U_Xc <- cont.entropy(X, k, disType="UU_X")
    }else{#There is a discrete component, since X is not 0-column
        #assertCondition(any(dPart))#, .exprString="Could not find discrete part!")
        #assert_that(any(dPart),
        #             msg="STOP trying to find discrete entropy!")
        discrete <- which(!cPart)
        Xd <- dplyr::select(X,discrete)
        thisResult$H_U_Xd <- log2(prod(map_int(Xd,n_distinct)))
            #log2(reduce(Xd,function(acc,v) acc*n_distinct(v),.init=1.0))
        # Early termination: one single value of range
        if (thisResult$H_U_Xd <= 0.0)#One single class in R.V.!
            thisResult$H_P_Xd <- 0.0#Don't bother working out the H_P
        else # Work out its entropy with the plug in estimator.
            thisResult$H_P_Xd <- infotheo::entropy(Xd)/log(2)
        # The second driving force is to avoid unnecessary copying
        # if (all(!cPart)){#Random vector composed only of discrete RV
        #     thisResult$H_U_Xc <- NA_real_#no continuous component
        #     thisResult$H_P_Xc <- NA_real_
        # } else 
        if (any(cPart)){# There exists also a cont. part, conditioned
            #    Return an KNN estimate of the continuous part.
            continuous <- which(cPart)
            #    Work out the conditional entropy of the continuous part
            dvarnames <- names(discrete)# discrete var names
            cvarnames <- names(continuous)# continuous var names 
            #varnames <- enquos(varnames)
            #Xc <- X %>% select(X , !!!syms(cvarnames))#Select continuous
            thisResult$H_U_Xc <- 
                cont.entropy(select(X , !!!syms(cvarnames)), k, disType="UU_X")
            # Not try to estimate the entropy using the KNN approx.
            # 1. Condition the numeric vars on the categoricals
            ###########
            Xc <- X %>% 
                group_by(!!!syms(dvarnames),.drop=TRUE) %>%
                nest(!!!syms(cvarnames)) #Make a list of continuous tibbles
            # TODO: find a more dplyr-like way to do the following
            counts <- sapply(Xc$data,nrow) + 0.0
            H_P_Xc <- sapply(
                Xc$data, 
                function(X) 
                    cont.entropy(as.data.frame(X),k, disType="P_X")
                )
            thisResult$H_P_Xc <- (H_P_Xc %*% counts)/sum(counts)
        }#else both discrete and continuous
    }
    return(thisResult)
}


