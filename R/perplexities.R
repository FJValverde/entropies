#' Obtaining the perplexities (inverse entropies) of a joint distribution.
#' 
#' @description There are primitives to obtain this for \code{data.frame} and
#' \code{\link[caret]{confusionMatrix}}.
#' @param data The data to evaluate for perplexity. 
#' @return A dataframe with the perplexities associated to the data
#' @export
perplexities <- function(data, ...) UseMethod("perplexities")



#' A function to work out the perplexities of a data frame with entropic information in it.
#'
#' @param A dataframe with the entropic variables needed to work out the perplexities, 
#' namely "Ux", "Uy", "Hx", "Hy" and "Hxy".
#' @return A dataframe with the perplexities associated to the data
#' @export
#' @importFrom dplyr mutate
#' @importFrom dplyr transmute
perplexities.data.frame<- function(data, ...){
    if(hasCmetEntropicCoords(data)){
        return(
            data %>% ##filter(type!="XY") %>% 
                mutate(k = 2^H_U, kx = 2^H_P, muxy= 2^M_P, kx_y = kx/muxy)
        )
    } else if (hasSimpleEntropies(data)){
        warning("This entropic coordinates are obsolete.")
        return(data %>%
                   mutate(MIxy = Hx + Hy - Hxy) %>% 
                   transmute(
                       k=2^Ux, 
                       m=2^Uy, 
                       kx = 2^Hx, 
                       my = 2^Hy,
                       muxy=2^MIxy, 
                       kx_y = kx/muxy, 
                       ky_x=my/muxy
                   )
        )
    } else
        stop("Missing fields in entropy data to work out performance indicators.")
}

#' A function to obtain the perplexities of 2- and 3-way tables
#' 
#' @description If data is a 2-way table, a single line of perplexities is obtained, 
#' but if data is a 3-way table, then we obtain as many rows as instances in the third
#' dimension and an extra factor variable on that dimension.
#' @param data A 2- or 3-way table. 
#' @examples perplexities(UCBAdmissions)
#' @export
perplexities.table <- function(data, ...){
    return(perplexities(jentropies(data, ...), ...))
}


#' A function to obtain perplexities on a \code{\link[caret]{confusionMatrix}}
#' @description Obtains the perplexities in a confusion matrix by calculating, first, 
#' the entropies, and then the entropic coordinates for it.
#' @param data A confusion matrix from an evaluation as in \code{\link[caret]{confusionMatrix}}
#' @importFrom caret confusionMatrix
#' @export
perplexities.confusionMatrix <- function(data, ...){
    return(perplexities(jentropies(data, ...), ...))
}


# FROM THE ORIGINAL MATLAB DEFINITION
# [H_Pxy, H_Px,H_Py, EMI_Pxy]=entropies(Pxy);
# n_Px = 2.^(H_Px);
# m_Py = 2.^(H_Py);
# %The mutual information gain
# if nargout < 3, return; end
# g_MIxy = 2.^EMI_Pxy;
# if nargout < 4, return; end
# %the remanent perplexities
# H_Py_x = H_Py - EMI_Pxy;
# H_Px_y = H_Px - EMI_Pxy;
# n_Px_y = 2.^(H_Px_y);
# m_Py_x = 2.^(H_Py_x);
# %The maximal perplexities are just the dimensions of the matrix
# if nargout < 6, return; end
# [n, m] = size(Pxy);
# %The losses from the uniform distributions
# if nargout < 8, return; end
# g_Px = n ./ n_Px;
# g_Py = m ./ m_Py;
