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
                                             factr = 2.22044604925031e-16,
                                             ndeps = 0.001),
                                        .Names = c("maxit","factr", "ndeps"))),
                 verbose=verbose)
})
    
