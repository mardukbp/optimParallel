## rm(list=ls())
## library("testthat")
## library("optimParallel", lib.loc = "../../../lib/")
source("testsetup.R")

context("test-issues")

control <- structure(list(maxit = 10,
                          factr = 2.22044604925031e-16),
                     .Names = c("maxit","factr"))

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
                      control=control),
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
                      control=control),
                 verbose=verbose)
    compareOptim(list(par=c(a=1,b=2), fn=FN2, gr= GR2, sleep=0,
                      method = "L-BFGS-B",
                      control=control),
                 verbose=verbose)
    compareOptim(list(par=c(a=1), fn=FN3, sleep=0,
                      method = "L-BFGS-B",
                      control=control),
                 verbose=verbose)
    compareOptim(list(par=c(a=1), fn=FN3, gr= GR3, sleep=0,
                      method = "L-BFGS-B",
                      control=control),
                 verbose=verbose)
})


test_that("optimParallel - use compiled code from other packages",{
    compareOptim(list(par=1, fn=abs, method = "L-BFGS-B",
                      control=control),
                 verbose=verbose)
    expect_equal(optimParallel(par=1, fn=abs,  aflaf=1, method = "L-BFGS-B"),
                 optim(par=1, fn=abs, method = "L-BFGS-B"))
    compareOptim(list(par=1, fn=abs, gr=abs, method = "L-BFGS-B",
                      control=control),
                 verbose=verbose)
    expect_equal(optimParallel(par=1, fn=abs,  gr=abs, aflaf=1, method = "L-BFGS-B"),
                 optim(par=1, fn=abs, gr=abs,  method = "L-BFGS-B"))

    compareOptim(list(par=1, fn=dnorm, method = "L-BFGS-B",
                      control=control),
                 verbose=verbose)
    compareOptim(list(par=1, fn=dnorm, mean=3, method = "L-BFGS-B",
                      control=control),
                 verbose=verbose)
    expect_equal(optimParallel(par=1, fn=dnorm, mean=2, aflaf=1, method = "L-BFGS-B"),
                 optim(par=1, fn=dnorm, mean=2,  method = "L-BFGS-B"))
})
