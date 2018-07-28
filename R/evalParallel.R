integrateArgs <- function(f, args){
    form <- formals(f)
    if(!is.null(form))
        for(i in seq_along(form))
            assign(names(form)[i], form[[i]])
    if(!is.null(args))
        for(i in seq_along(args))
            assign(names(args)[i], args[[i]])
    ff <- function(){}
    parent.env(environment(ff)) <- .GlobalEnv
    body(ff) <- body(f)
    if(any(names(form) == "..."))
        formals(ff) <- form[names(form) == "..."]
    ff
}

getFunctions <- function(f, args, firstArg, parnames){
    if(is.vector(firstArg))
        firstArg <- matrix(data=firstArg)
    lapply(seq_len(ncol(firstArg)), function(x){
        fa <- firstArg[,x]
        names(fa) <- parnames
        args[[names(formals(f))[1]]] <- fa
        integrateArgs(f=f, args=args)
    })
}

#' @importFrom parallel parLapply
evalParallel <- function(cl, f, args, firstArg, parnames){
    funlist <- getFunctions(f=f, args=args, firstArg=firstArg, parnames=parnames)
    parallel::parLapply(cl=cl, X=funlist, fun=function(x) x())
}
