#' Multivariate Source Entropy decomposition of a random vector
#'
#' Returns several different flavours of sentropies (source entropies) depending on the structure 
#' of the data provided to the function. There are specialized versions for
#' (contingency) tables, confusion matrices and data frames.
#' @param dat The data being provided to the function. 
#' @param type The type of analysis being requested. If type=="total" (default) it will perform 
#' a normal analysis providing total + dual total correlation. If "dual" is used, then
#' only the dual total correlation will be provided. In this case, no individual split equations 
#' will be output-only a single aggregate-, and the names of the columns will be changed accordingly. 
#' @param unit The logarithm to be used in working out the sentropies as per 
#' \code{entropy}. Defaults to "log2".
#' @param r The order of the shifted Renyi entropy being calculated. r=0 is Shannon's.
#' @return  A dataframe with the sentropies of the marginals. If type="dual" then \code{hasAggregateSmetCoords(sentropies(dat, type="dual"))}, 
#' otherwise it has the default type="total", \code{hasSplitSmetCoords(sentropies(dat, typt="total"))}.
#' @details Unless specified by the user explicitly, this function uses base 2 
#'   logarithms for the sentropies. 
#' @seealso \code{\link[entropy]{entropy}, \link[infotheo]{entropy}}
#' @export
sentropies <- function(dat, type="total", r=0, ...) UseMethod("sentropies")

#' Entropy decomposition of a contingency matrix
#' 
#' Given a contingency matrix, provide one row of entropy coordinates. 
#' NOTE: the reference variable has to index the ROWS of the table, while the predicted
#' variable indexes the columns, unlike, e.g. \code{\link[caret]{confusionMatrix}}
#' @param Nxy An n-contingency matrix where n > 2
#' @param unit The logarithm to be used in working out the sentropies as per 
#' \code{entropy}. Defaults to "log2".
#' @export
#' @importFrom entropy entropy
# @example sentropies(UCBAdmissions)
sentropies.table <- function(dat, type="total", ...){
    # 0. Parameter checking
    Nxy <- as.table(dat) # is this necessary? Not really. 
    dims <- dim(Nxy)
    if (length(dims) < 2)
        stop("Cannot process tables with less than 2 dimensions.")
#    if (length(dims) < 2 | length(dims) > 3)
#        stop("Cannot process tables with more than 3 dimensions or less than 2 dimensions.")
    if (dims[1] < 2 | dims[2] < 2)
        stop("sentropies are not defined for distributions with a singleton domain.")
    # 1. Start processing: this is a candidate por sentropies_raw
    #unless otherwise specified, we use log2 logarithms
    # CAVEAT: use a more elegant kludge
    theseVars <- list(...);
    if (is.null(theseVars$unit))#force entropy in bits unles otherwise selected
        theseVars$unit <- "log2"
    if (length(dims)==2){ # N is a plain contingency on X and Y
        Nx <- apply(Nxy, 1, sum); 
        Hx <- do.call(entropy::entropy, c(list(y=Nx), theseVars)) #entropy(Nx,theseVars)
        Ny <- apply(Nxy, 2, sum); 
        Hy <- do.call(entropy::entropy, c(list(y=Ny), theseVars)) #entropy(Ny, theseVars)
        Ux <- log2(dims[1]) #entropy(rep(1/dims[1],dims[1]),unit="log2",...)
        Uy <- log2(dims[2]) #entropy(rep(1/dims[2],dims[2]),unit="log2",...)
        Hxy <- do.call(entropy, c(list(y=Nxy), theseVars)) #entropy(Nxy, theseVars) 
        df <- data.frame(Ux = Ux, Uy = Uy, Hx = Hx, Hy = Hy, Hxy = Hxy)
    } else {  # N is a multiway table: we analyze on the first two margins, but store the second
        Nx <- margin.table(Nxy, c(1,3:length(dims)))
        Hx <- apply(Nx, c(2:length(dim(Nx))), function(x) {do.call(entropy::entropy, c(list(y=x), theseVars)) })
        #Ux <- apply(Nx, 2, function(x) { log2(length(x))})
        Ux <- apply(Nx, c(2:length(dim(Nx))), function(x) { log2(dims[1])})
        Ny <- margin.table(Nxy, c(2,3:length(dims)))
        Hy <- apply(Ny, c(2:length(dim(Ny))), function(x) {do.call(entropy::entropy, c(list(y=x), theseVars)) })
        #Uy <- apply(Ny, 2, function(x) { log2(length(x))})
        Uy <- apply(Ny, c(2:length(dim(Nx))), function(x) { log2(dims[1])})
        Hxy <- apply(Nxy, 3:length(dims), function(x) {do.call(entropy::entropy, c(list(y=x), theseVars))})
        #df <- data.frame(Ux = Ux, Uy = Uy, Hx = Hx, Hy = Hy, Hxy = Hxy)
        THx <- as.data.frame.table(as.table(Hx), responseName = "Hx")
        TUx <- as.data.frame.table(as.table(Ux), responseName = "Ux")
        THy <- as.data.frame.table(as.table(Hy), responseName = "Hy")
        TUy <- as.data.frame.table(as.table(Uy), responseName = "Uy")
        THxy <- as.data.frame.table(as.table(Hxy), responseName = "Hxy")
        df <- left_join(left_join(left_join(TUx, TUy), left_join(THx, THy)), THxy)
        #df <- data.frame(Ux = Ux, Uy = Uy, Hx = Hx, Hy = Hy, Hxy = Hxy)
        #df <- cbind(df, dimnames(Nxy)[3:length(dims)])# Keep the third  and greater dimension's names
        #name <- colnames(N[1,,]) # This is a hack to manifest the values in the 3rd dimension
    } 
    return(df)
}

#' Entropy decomposition of a confusion table from caret
#' 
#' @inherit sentropies.table
#' @export
#' @importFrom caret confusionMatrix
sentropies.confusionMatrix <- function(dat, type="total", r=0, ...){
    return(sentropies(t(dat$table), type, r, ...))
}

#' Multivariate source entropy decomposition of a data frame
#' 
#' @return Another dataframe with the main entropy coordinates of every variable
#'   in the original, which are now the rows of the returned data.frame. If the columns have no
#'   names, artificial ones are returned based in pre prefix "x" and their column number. 
#'   The entropies are always in bits.
#' @param dat The data being provided to the function. 
# @param unit The logarithm to be used in working out the sentropies as per 
# \code{entropy}. Defaults to "log2".
# @details Unless specified by the user explicitly, this function uses base 2 
#   logarithms for the sentropies.
#' @export
#' @importFrom infotheo natstobits condentropy entropy discretize
#' @importFrom dplyr mutate
sentropies.data.frame <- function(dat, type="total", r = 0, ...){
    if (ncol(dat) == 0 || nrow(dat) == 0)
        stop("Can only work with non-empty data.frames!")
    if (!all(sapply(dat, is.integer) | sapply(dat, is.factor))){
        warning("Discretizing data before entropy calculation!")
        # The following takes the parameter 'disc' from infotheo::discretize
        dat <- infotheo::discretize(dat, ...) # infotheo::str(dfdiscretize generates ints, not factors.
    }
    # suppose the dataframe is categorical
    switch(type, #TODO: improve this way of "invoking" the dataset.
        "dual" =        {TOTAL <- FALSE},
        {TOTAL <- TRUE} #catch-all for total processing
    ) #This value "FALLS THROUGH"
    theseVars <- list(...)
    # if (!is.null(theseVars$type) && theseVars$type == "dual"){# Source decomposition with TOTAL C_P_X and D_P_X
    #     TOTAL <- FALSE
    # }# else {#Source decomposition with ONLY D_P_X, to align it with the CMEBE
    # #    TOTAL <- FALSE
    # #}
    # WARNING: Manipulating the ellipsis is tricky. The following KLUDGE is to 
    # include by itself a parameter 'method' in the list of vars. 
    newVars <- list()
    if ((length(newVars) == 0) | !is.null(theseVars$method)){#Kludge. THis should actually be passed from top.
        newVars$method <- "emp"
    }
    # FVA: There is a problem here: how to deal with deterministic signals?
    # Solution: 1. We set the H_Uxi for at least at two values, 
    #           2. The entropy for deterministic is zero.
    #           3. Likewise for VI_Pxi
    #           4. The SMET coordinates are worked out from these correctly. 
    H_Uxi <- sapply(dat, function(v){log2(max(2,length(unique(v))))})
    #H_Uxi <- sapply(dat, function(v){log2(length(unique(v)))})
    #H_Uxi <- unlist(lapply(df, function(v){log2(length(unique(v)))}))
    # H_Pxi <- unlist(lapply(dat, 
    #                        function(v){natstobits(infotheo::entropy(v,newVars))}))
    H_Pxi <- sapply(dat, function(v){natstobits(infotheo::entropy(v,newVars))})
    if (ncol(dat) == 1){
        warning("Single variable: providing only entropy")
        VI_Pxi <- H_Pxi # All of the entropy is non-redundant
    } else {
        VI_Pxi <- vector("numeric", ncol(dat))
        for(i in 1:ncol(dat)){
            VI_Pxi[i] <- natstobits(condentropy(dat[,i], dat[,-i],newVars))
        }
    }
    if(TOTAL){#return the TOTAL decomposition and aggregates
        if (is.null(names(dat))){
            warning("No names for columns: providing dummy names!")
            names(dat) <- paste0("x",1:ncol(dat))
        }
        # Find simple sentropies, divergences and sentropies of the uniform marginals. 
        edf <- data.frame(
            name = names(dat), # After an idiosyncrasy of dplyr, the rownames do not survive a mutate.
            H_Uxi = H_Uxi,
            H_Pxi = H_Pxi,
            stringsAsFactors = FALSE #Keep the original variable names as factors!
        ) %>% dplyr::mutate(
            DeltaH_Pxi = H_Uxi - H_Pxi, 
            M_Pxi = H_Pxi - VI_Pxi, 
            VI_Pxi)# This is placed here so that the last 3 columns read the ET coordinates
        edf <- rbind(edf,cbind(name="@AVERAGE", as.data.frame(lapply(edf[,2:6], sum))))
    } else {#return only an aggregate with the DUAL total correlation D_Px
        H_Px <-  natstobits(entropy(dat,theseVars)) #a single number!
        VI_Px <-  sum(VI_Pxi)
        edf <- data.frame(name="@AVERAGE", H_Ux = sum(H_Uxi), H_Px) %>% 
            dplyr::mutate(
                DeltaH_Px = H_Ux - H_Px, 
                D_Px = H_Px - VI_Px, 
                VI_Px)
    }
    return(edf)
}