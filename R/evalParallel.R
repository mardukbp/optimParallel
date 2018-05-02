integratArgs <- function(f, args){
    form <- formals(f)
    if(!is.null(form))
        for(i in seq_along(form))
            assign(names(form)[i], form[[i]])

    if(!is.null(args))
        for(i in seq_along(args))
            assign(names(args)[i], args[[i]])
    ff <- function(){}
    body(ff) <- body(f)
    if(any(names(form) == "..."))
        formals(ff) <- form[names(form) == "..."]
    ff
}

getFunctions <- function(f, args, firstArg){
    if(is.vector(firstArg))
        firstArg <- matrix(data=firstArg)
    lapply(1:ncol(firstArg), function(x){
        args[[names(formals(f))[1]]] <- firstArg[,x]
        integratArgs(f=f, args=args)
    })
}

#' @importFrom parallel parLapply
evalParallel <- function(cl, f, args, firstArg){
    funlist <- getFunctions(f=f, args=args, firstArg=firstArg)
    parallel::parLapply(cl=cl, X=funlist, fun=function(x) x())
}
