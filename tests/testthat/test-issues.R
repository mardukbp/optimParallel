## rm(list=ls())
## library("testthat")
## library("optimParallel", lib.loc = "../../../lib/")
source("testsetup.R")

context("test-issues")


FN1 <- function(par, sleep){
    Sys.sleep(sleep)
    sum(par^2)
}

GR1 <- function(par, sleep){
    Sys.sleep(sleep)
    2*par
}

test_that("optimParallel",{
    compareOptim(list(par=c(1,2,3), fn=FN1, gr=GR1, sleep=0,
                      method = "L-BFGS-B",
                      control=structure(list(maxit = 10,
                                             factr = 2.22044604925031e-16),
                                        .Names = c("maxit","factr"))),
                 verbose=verbose)
})

FN2 <- function(par, sleep){
    Sys.sleep(sleep)
    par["a"]^2+par["b"]^2
}
GR2 <- function(par, sleep){
    Sys.sleep(sleep)
    2*c(par["a"],par["b"])
}

FN3 <- function(par, sleep){
    Sys.sleep(sleep)
    par["a"]^2
}
GR3 <- function(par, sleep){
    Sys.sleep(sleep)
    2*c(par["a"])
}

test_that("optimParallel - named arguments",{
    compareOptim(list(par=c(a=1,b=2), fn=FN2, sleep=0,
                      method = "L-BFGS-B",
                      control=structure(list(maxit = 10,
                                             factr = 2.22044604925031e-16),
                                        .Names = c("maxit","factr"))),
                 verbose=verbose)
    compareOptim(list(par=c(a=1,b=2), fn=FN2, gr= GR2, sleep=0,
                      method = "L-BFGS-B",
                      control=structure(list(maxit = 10,
                                             factr = 2.22044604925031e-16),
                                        .Names = c("maxit","factr"))),
                 verbose=verbose)
    compareOptim(list(par=c(a=1), fn=FN3, sleep=0,
                      method = "L-BFGS-B",
                      control=structure(list(maxit = 10,
                                             factr = 2.22044604925031e-16),
                                        .Names = c("maxit","factr"))),
                 verbose=verbose)
    compareOptim(list(par=c(a=1), fn=FN3, gr= GR3, sleep=0,
                      method = "L-BFGS-B",
                      control=structure(list(maxit = 10,
                                             factr = 2.22044604925031e-16),
                                        .Names = c("maxit","factr"))),
                 verbose=verbose)
})


FN4 <- function(par){
    print(search())
    diag(spam(1:4, 2))
    sum(par^2)
}

test_that("optimParallel - dispatch to other packages",{
    skip_if_not(require("spam"), message="spam not available for testing dispatching to loaded packages")
    clusterEvalQ(cl, require("spam"))
    expect_true({optimParallel(par=c(a=1,b=2), fn=FN4, method = "L-BFGS-B"); TRUE})
})
