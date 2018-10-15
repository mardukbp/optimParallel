integrateArgs <- function(f, args) {
    if(is.null(formals(f))) ## like sin()
        args <- args[1]
    else if (all(names(formals(f)) != "..."))
        args <- args[names(args) %in% names(formals(f))]
    do.call(function (f, ...){
                                        # inspired from purrr::partial()
        eval(call("function", NULL, substitute(f(...))),
             envir=environment(f))
    }, c(f=list(f), args))
    ##do.call(purrr::partial, c(list(f), args))
}

getFunctions <- function(f,
                         args,     ## potential other arguments
                         firstArg, ## first argument
                         parnames){
    if(is.vector(firstArg))
        firstArg <- matrix(data=firstArg)
    lapply(seq_len(ncol(firstArg)), function(x){
        fa <- firstArg[,x]
        names(fa) <- parnames
        args <- args[names(args) != names(formals(args(f)))[1]]
        allargs <- c(list(fa), args)
        names(allargs)[1] <- names(formals(args(f)))[1]
        integrateArgs(f=f, args=allargs)
    })
}

#' @importFrom parallel parLapply
evalParallel <- function(cl, f, args, firstArg, parnames){
    funlist <- getFunctions(f=f, args=args, firstArg=firstArg, parnames=parnames)
    parallel::parLapply(cl=cl, X=funlist, fun=function(x) x())
}
