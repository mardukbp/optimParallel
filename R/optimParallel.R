#' @name optimParallel
#' @aliases optimparallel optimParallel-package optimParallel-Package OptimParallel-package OptimParallel-Package optimparallel-package optimparallel-Package 
#' @author Florian Gerber, \email{florian.gerber@@math.uzh.ch}, \url{https://user.math.uzh.ch/gerber}.
#' @title parallel version of \code{\link[stats]{optim}}
#' @keywords package
#' @docType package
#' @description
#' The function provides parallel versions of the gradient-based \code{\link[stats]{optim}} methods
#' \code{"L-BFGS-B"}, \code{"BFGS"}, and \code{"CG"}.
#' If the evaluation of the function \code{fn} takes more than 0.05 seconds, \code{optimParallel} can significantly reduce the optimization time. 
#' For a \eqn{p}-parameter optimization based on \code{"L-BFGS-B"}, the speed increase is about factor \eqn{1+2p} when no analytic gradient is specified and \eqn{1+2p} processor cores are available.
#' @param par see the documentation of \code{\link[stats]{optim}}.
#' @param fn see the documentation of \code{\link[stats]{optim}}.
#' @param gr see the documentation of \code{\link[stats]{optim}}.
#' @param ... see the documentation of \code{\link[stats]{optim}}.
#' Note that depending on the chosen cluster type for parallel execution, the \code{.GlobalEnv} of the R processes in the cluster contain different R objects compared to the main R process.
#' In that case, it may be necessary to add all R object required by \code{fn} and \code{gr} here in order to pass them to the R processes in the cluster.
#' @param method parallel versions of the gradient-based methods \code{"L-BFGS-B"} (default), \code{"BFGS"}, and \code{"CG"} of \code{\link[stats]{optim}} are available.
#' The recommended method is \code{"L-BFGS-B"} because it triggers one (approximate) gradient evaluation per iteration, which best fits the implemented parallel processing scheme.   
#' See the documentation of \code{\link[stats]{optim}} for information on the methods.
#' If another method is specified, all arguments are directly passed to \code{\link[stats]{optim}}. 
#' @param lower see the documentation of \code{\link[stats]{optim}}.
#' @param upper see the documentation of \code{\link[stats]{optim}}.
#' @param control see the documentation of \code{\link[stats]{optim}}.
#' @param hessian see the documentation of \code{\link[stats]{optim}}.
#' @param parallel is a list of additional control parameters and can supply any of the following components:
#' \describe{
#' \item{\code{cl}}{ an object of class \code{"cluster"} specifying the cluster to be used for parallel execution.
#' See \code{\link[parallel]{makeCluster}} for more information.
#' If the argument is not specified or \code{NULL}, the default cluster is used.
#' See \code{\link[parallel]{setDefaultCluster}} for information on how to set up a default cluster.} 
#'  \item{\code{forward}}{ logical vector of length 1. If \code{FALSE} (default when loading the package), a numeric central difference approximation of the gradient defined as
#' \eqn{(fn(x+\epsilon)-fn(x-\epsilon))/(2\epsilon)} is used, which corresponds to the approximation used in \code{\link[stats]{optim}}.
#' If \code{TRUE}, a numeric forward difference approximation of the gradient essentially defined as
#' \eqn{(fn(x+\epsilon)-fn(x))/\epsilon} is used. This reduces the number of function calls from \eqn{1+2p} to \eqn{1+p} and can be useful if the number of available cores is smaller than \eqn{1+2p} and if the memory limit is reached.}
#' \item{\code{loginfo}}{ logical vector of length 1 with default value \code{FALSE} when loading the package. If \code{TRUE},
#' additional log information containing the evaluated parameters as well as return values of \code{fn} and \code{gr} is returned.}
#' }
#' 
#' @return Same as the return value of \code{\link[stats]{optim}}. See the documentation thereof for more information.\cr
#' If a gradient-based method is specified and \code{parallel=list(loginfo=TRUE)}, additional log information containing the evaluated parameters as well as
#' the return values of \code{fn} and \code{gr} is returned.
#'
#' @details \code{optimParallel} is a wrapper to \code{\link[stats]{optim}} and relies on the lexical scoping mechanism of R
#' and the R package \pkg{parallel} to evaluate \code{fn}
#' and its (approximate) gradient in parallel.\cr\cr
#' Some default values of the argument \code{parallel} can be set via\cr\code{options("optimParallel.forward", "optimParallel.loginfo")}.
#'
#' @references F. Gerber, R. Furrer (2018)
#' optimParallel: an R Package Providing Parallel Versions of the Gradient-Based Optimization Methods of optim(). 
#' ArXiv e-prints. URL http://arxiv.org/abs/1804.11058.
#' Also available as vignette of this package \code{vignette("optimParallel")}. 
#'
#' @section Notes:
#' \describe{
#' \item{1.}{If \code{fn} or \code{gr} depend on functions or methods from loaded packages,
#' it may be necessary to explicitly load those packages in all processes of the cluster.
#' For \code{cl} of class \code{"cluster"} one can use \code{clusterEvalQ(cl, search())} to check
#' whether all required packages are on the search paths of all processes.
#' If, for example, the R package \pkg{spam} is required and missing on those search paths,
#' it can be added via \code{clusterEvalQ(cl, library("spam"))}.} 
#' \item{2.}{If \code{fn} or \code{gr} depend on functions or objects defined in the current R session,
#' it may be necessary to pass them to \code{optimParallel} via the \code{...} argument.
#' Alternatively, they can be made available to the R processes in the cluster via \code{\link[parallel]{clusterEvalQ}}.}
#' \item{3.}{Using parallel R code inside \code{fn} and \code{gr} may not work, because this results in nested parallel processing.}
#' \item{4.}{Using \code{optimParellel} with \eqn{n} parallel processes increases the memory usage by about factor \eqn{n} compared to a call to \code{\link[stats]{optim}}.
#' If the memory limit is reached this may severely slowdown the optimization.
#' Strategies to reduce memory usage are
#' (1) kill all unused processes on the computer,
#' (2) revise the code of \code{fn} and/or \code{gr} to reduce its memory usage, and
#' (3) reduce the number of parallel processes by specifying the argument \code{parallel=list(forward=TRUE)} and/or
#' setting up a cluster with less parallel processes.}
#' }
#'
#' @section Issues and bug report:
#' A list of known issues of \code{optimParallel} can be found at \url{https://git.math.uzh.ch/florian.gerber/optimParallel/issues}.
#' Please report issues not listed there to\eqn{\,} \email{florian.gerber@@math.uzh.ch}. Do not forget to include
#' an R script reproducing the issue and the output of \code{sessionInfo()}. 
#' 
#' @seealso
#' \code{\link[stats]{optim}},
#' \code{\link[parallel]{makeCluster}},
#' \code{\link[parallel]{setDefaultCluster}},
#' \code{\link[parallel]{stopCluster}},
#' \code{\link[parallel]{detectCores}}.
#' @examples
#' negll <- function(par, x, sleep=0, verbose=TRUE){
#'     if(verbose)
#'         cat(par, "\n")
#'    Sys.sleep(sleep)
#'    -sum(dnorm(x=x, mean=par[1], sd=par[2], log=TRUE))
#' }
#' set.seed(13); x <- rnorm(1000, 5, 2)
#'
#' cl <- makeCluster(2)     # set the number of processor cores
#' setDefaultCluster(cl=cl) # set 'cl' as default cluster
#'
#' optimParallel(par=c(1,1), fn=negll, x=x,
#'               method = "L-BFGS-B", lower=c(-Inf, .0001))
#'
#' optimParallel(par=c(1,1), fn=negll, x=x,
#'               method = "L-BFGS-B", lower=c(-Inf, .0001),
#'               parallel=list(loginfo=TRUE))
#' 
#' setDefaultCluster(cl=NULL); stopCluster(cl)
#'
#' ## default values of the argument 'parallel':
#' options("optimParallel.forward", "optimParallel.loginfo")
#' 
#' \dontrun{
#' ## - use all avilable processor cores
#' ## - return cat() output to R prompt
#' ##   (may have issues on Windows)
#' if(tolower(.Platform$OS.type) != "windows"){
#'     cl <- makeCluster(spec=detectCores(), type="FORK", outfile="")  
#' } else
#'     cl <- makeCluster(spec=detectCores(), outfile="")
#' setDefaultCluster(cl=cl)
#'
#' ## return log information
#' options(optimParallel.loginfo=TRUE)              
#'
#' ## stop if change of f(x) is smaller than 0.01
#' control <- list(factr=.01/.Machine$double.eps)
#'
#' optimParallel(par=c(1,1), fn=negll, x=x, sleep=.5,
#'               verbose=TRUE, method="L-BFGS-B",
#'               lower=c(-Inf, .0001), control=control)
#' ## each step invokes 5 parallel calls to negll()
#'
#' optimParallel(par=c(1,1), fn=negll, x=x, sleep=.5,
#'               method ="L-BFGS-B", lower=c(-Inf, .0001),
#'               control=control,
#'               parallel=list(forward=TRUE))
#' ## each step invokes 3 parallel calls to negll()
#' 
#' setDefaultCluster(cl=NULL); stopCluster(cl) }
#' @export
#' @importFrom stats optim
optimParallel <- function(par, fn, gr = NULL, ..., method = c("L-BFGS-B", "BFGS", "CG"),
                          lower = -Inf, upper = Inf, control = list(), hessian = FALSE,
                          parallel=list()){
    dots <- list(...)
    if(!identical(dots, list()) && (any(names(formals(args(fn)))[1]==names(dots)) || any(names(formals(args(fn)))[1]==names(dots))))
        warning("The first argument of \"fn\" and/or \"gr\" has the same name as one argument passed through \"...\". The value passed through \"...\" for that argument is ignored.")
    
    method <- method[1]
    if(!(method %in% c("L-BFGS-B", "BFGS", "CG"))){
        warning("Only the gradient methods \"L-BFGS-B\", \"BFGS\", and \"CG\" are available in a parallel version.")
        return(optim(par=par, fn=fn, gr=gr, ..., method=method,
                     lower=lower, upper=upper, control=list(), hessian=FALSE))
    }
    if(is.null(parallel$forward))
        parallel$forward <- getOption("optimParallel.forward")
    stopifnot(length(parallel$forward)==1, is.logical(parallel$forward))
    if(is.null(parallel$loginfo))
        parallel$loginfo <- getOption("optimParallel.loginfo")
    stopifnot(length(parallel$loginfo)==1, is.logical(parallel$loginfo))
    
    if(is.null(control$ndeps))
        control$ndeps <- 1e-3
    stopifnot(is.numeric(control$ndeps))

    if(is.null(control$fnscale)){
        fnscale <- 1
    } else {
        fnscale <- control$fnscale
        control$fnscale <- NULL
        fnscale <- fnscale[1]
        stopifnot(is.numeric(fnscale))
    }
    parscale <- control$parscale

    fg <- parallel_fg_generator(fn=fn, gr=gr, args_list=dots,
                                forward=parallel$forward,
                                lower=lower, upper=upper, 
                                cl=parallel$cl, ndeps=control$ndeps,
                                fnscale=fnscale, parscale=parscale,
                                parnames=names(par))
    out <- stats::optim(par=par, fn=fg$f, gr=fg$g, method = method, lower=lower,
                        upper=upper, control=control, hessian=hessian)
    out$value <- out$value*fnscale
    if(parallel$loginfo){
        out[[length(out)+1]] <- fg$getLog()
        names(out)[length(out)] <- "loginfo"
    }
    out
}

#' @importFrom parallel parLapply
parallel_fg_generator <- function(fn, gr=NULL, args_list=list(),
                                  forward=FALSE, lower=-Inf, upper=Inf,
                                  cl=NULL, ndeps=1e-3, fnscale=1, parscale=1,
                                  parnames=NULL){
    stopifnot(is.function(fn),
              is.null(gr) || is.function(gr), 
              is.list(args_list),
              is.logical(forward), length(forward)==1,
              is.numeric(lower), is.numeric(upper),
              is.null(cl) || inherits(cl, "cluster"),
              is.null(ndeps) || is.numeric(ndeps),
              is.numeric(fnscale) || is.null(fnscale),
              is.numeric(parscale) || is.null(parscale),
              is.character(parnames) || is.null(parnames))
    if(any(is.na(lower))) lower[is.na(lower)] <- -Inf
    if(any(is.na(upper))) lower[is.na(upper)] <- Inf

    if(is.null(fnscale)) fnscale <- 1
    if(is.null(parscale)) parscale <- 1

    eval <- function(par){
        ## the first argument of fn has to be a vector of length length(par)
        if(identical(par, par_last))
            return(list(value=value, grad=grad))
        if(is.null(gr)){
            n <- length(par)
            ndeps <- ndeps*parscale
            ndeps_mat <- array(0, c(n,n))
            ndeps_vec <- rep(ndeps, length.out=n)
            diag(ndeps_mat) <- ndeps_vec
            if(forward){
                ndepsused <- ndeps_vec
                PAR <- data.frame(cbind(array(par, c(n,n))+ndeps_mat))
                if(!(is.null(upper) || all(is.na(upper)) || all(upper==Inf))){
                    hitu <- unlist(lapply(PAR, function(par){any(par>upper)}))
                    if(any(hitu)){
                        PARl <- data.frame(cbind(array(par, c(n,n))-ndeps_mat))
                        PAR[hitu] <- PARl[hitu]
                        ndepsused[hitu] <- -ndeps_vec[hitu]
                    }
                }
                PAR <- cbind(PAR, par)
                e <- unname(unlist(evalParallel(cl=cl, f=fn, args=args_list, firstArg=PAR, parnames=parnames)))
                e <- e/fnscale
                value <- e[length(e)]
                length(e) <- length(e)-1
                grad <- (e-value)/ndepsused
            } else { # two sided
                ndepsused <- 2*ndeps_vec
                PAR <- data.frame(cbind(array(par, c(n,n))+ndeps_mat, array(par, c(n,n))-ndeps_mat))
                if(!(is.null(upper) || all(is.na(upper)) || all(upper==Inf))){
                    hitu <- unlist(lapply(PAR, function(par){any(par>upper)}))
                    if(any(hitu)){
                        PAR[hitu] <- par
                        hitui <- apply(matrix(hitu, ncol=2), 1, any)
                        ndepsused[hitui] <- ndeps_vec[hitui] 
                    }
                }
                if(!(is.null(lower) || all(is.na(lower)) || all(lower==Inf))){
                    hitl <- unlist(lapply(PAR, function(par){any(par<lower)}))
                    if(any(hitl)){
                        PAR[hitl] <- par
                        hitli <- apply(matrix(hitl, ncol=2), 1, any)
                        ndepsused[hitli] <- ndeps_vec[hitli] 
                    }
                }
                PAR <- cbind(PAR, par)
                e <- unname(unlist(evalParallel(cl=cl, f=fn, args=args_list, firstArg=PAR, parnames=parnames)))
                e <- e/fnscale
                value <- e[length(e)]
                length(e) <- length(e)-1
                e_mat <- matrix(e, ncol=2)
                grad <- c(e_mat[,1]-e_mat[,2])/ndepsused
            }
        }else{ # gr is not null
            funlist <- list(getFunctions(f=fn, args=args_list, firstArg=par, parnames=parnames)[[1]],
                            getFunctions(f=gr, args=args_list, firstArg=par, parnames=parnames)[[1]])
            res <- parallel::parLapply(cl=cl, X=funlist, fun=function(x) x())
            value <- res[[1]]/fnscale 
            grad <- res[[2]]/fnscale 
        }
        if(is.null(optimlog)){
            optimlog <- c(i_e+1, par, value, grad, use.names=FALSE)
            names(optimlog) <- c("iter", paste0("par", seq_along(par)), "fn", paste0("gr", seq_along(par)))
        }
        else{
            optimlog <- rbind(optimlog, c(i_e+1, par, value=value, grad=grad))
            rownames(optimlog) <- NULL
        }
        par_last <<- par
        value <<- value
        grad <<- grad
        optimlog <<- optimlog
        i_e <<- i_e+1
        return(list(value=value, grad=grad))
    }
    f <- function(par){
        eval(par) 
        i_f <<- i_f+1
        return(value)
    }
    g <- function(par){
        eval(par) 
        i_g <<- i_g+1
        return(grad)
    }
    init <- function(){
        i_f <<- i_g <<- i_e <<- 0
        par_last <<- value <<- grad <<- NA
    }
    getCount <- function(){
        c(i_e, i_f, i_g)
    }
    getLog <- function(){
        optimlog
    }
    i_f <- i_g <- i_e <- 0
    par_last <- value <- grad <- NA
    optimlog <- NULL
    list(f=f, g=g, init=init, eval=eval, getCount=getCount, getLog=getLog)
}
