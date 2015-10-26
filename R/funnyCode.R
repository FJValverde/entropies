
ent <- function(N,...){
    vars <- list(...)
    if (!("unit" %in% names(vars)))
        vars <- c(list(N=N), list(vars, unit="log2"))
    do.call("print", vars)
}