# PACKAGE-WIDE auxiliary functions and variables NOT EXPORTED.

#' A characterization of the variables in a data.frame needed to define the perplexities.
simpleEntropies <- c("Ux", "Uy", "Hx", "Hy", "Hxy")

#' A caracterization of the variables in a data.frame of derived non-split entropies.
derivedNonSplitEntropies <- c("Uxy", "DeltaHxy", "MIxy2", "VIxy")

#' A caracterization of the variables in a data.frame of derived non-split entropies.
derivedSplitEntropies <- c("DeltaHx", "DeltaHy", "MIxyX", "MIxyY", "VIx", "VIy")

#'a function to detect if the simple entropies are present.
hasSimpleEntropies <- function(df){all(simpleEntropies %in% names(df))}

#' A function to detect if the derived split entropies are present.
hasDerivedSplitEntropies <- function(df){all(derivedSplitEntropies %in% names(df))}

#' A function to detect if the derived, non-split entropies are present. 
hasDerivedNonSplitEntropies <- function(df) {all(derivedNonSplitEntropies %in% names(df))}
