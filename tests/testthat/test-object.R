## rm(list=ls())
## library("testthat")
## library("numDeriv")
## library("optimParallel", lib.loc = "../../../lib/")
context("test-object")
source("testsetup.R")
verbose <- FALSE

f1 <- function(x){
    if(verbose) cat(x, "\n")
    sum(x)
}
f2 <- function(x){
    if(verbose) cat(x, "\n")
    if(any(x<0)) stop()
    sum(x)
} 
f3 <- function(x){
    if(verbose) cat(x, "\n")
    if(any(x>0)) stop()
    sum(x)
} 
f4 <- function(x){
    x[1]^2 + (1-x[2])^2+x[3]^3
}
f5 <- function(x){
    x[1]^2 + (1-x[2])^2+log(x[3])
}

test_that("basic",{
    o1 <- optimParallel:::parallel_fg_generator(f1, cl=cl, args_list=list(verbose=verbose))
    expect_equal(o1$f(1), 1)
    expect_equal(o1$g(1), 1)
    o1 <- optimParallel:::parallel_fg_generator(f1, args_list=list(verbose=verbose))
    expect_equal(o1$f(c(1,1)), 2)
    expect_equal(o1$g(c(1,1)), c(1,1))
    o1 <- optimParallel:::parallel_fg_generator(f1, args_list=list(verbose=verbose))
    expect_equal(o1$f(c(1,1,2)), 4)
    expect_equal(o1$g(c(1,1,2)), c(1,1,1))
})

test_that("bounds",{
    o2 <- optimParallel:::parallel_fg_generator(f2, args_list=list(verbose=verbose), lower=0)
    expect_equal(o2$f(1), 1)
    expect_equal(o2$g(1), 1)
    expect_equal(o2$f(0), 0)
    expect_equal(o2$g(0), 1)
    expect_error(o2$g(-1))

    o3 <- optimParallel:::parallel_fg_generator(f3, upper=0, args_list=list(verbose=verbose))
    expect_equal(o3$f(-1), -1)
    expect_equal(o3$g(-1), 1)
    expect_equal(o3$f(0), 0)
    expect_equal(o3$g(0), 1)
    expect_error(o3$g(1))
})



test_that("derivative",{
    o4 <- optimParallel:::parallel_fg_generator(f4)
    expect_equal(o4$g(c(1,2,3)), numDeriv::grad(f4, c(1,2,3)),
                 tolerance=1e-3)
    expect_equal(o4$g(c(-1,2,-3.3)), numDeriv::grad(f4, c(-1,2,-3.3)),
                 tolerance=1e-3)
    o4_2 <- optimParallel:::parallel_fg_generator(f4, forward=TRUE)
    expect_equal(o4_2$g(c(1,2,3)), numDeriv::grad(f4, c(1,2,3)),
                 tolerance=1e-3)
    expect_equal(o4_2$g(c(-1,2,-3.3)), numDeriv::grad(f4, c(-1,2,-3.3)),
                 tolerance=1e-3)
})

test_that("eps",{
    o5 <- optimParallel:::parallel_fg_generator(f5, ndeps=1e-3)
    expect_equal(o5$g(c(5,6,7)), numDeriv::grad(f5, c(5,6,7)),
                 tolerance=1e-3)
    o5_2 <- optimParallel:::parallel_fg_generator(f5, ndeps=c(.01,.05,.001))
    expect_equal(o5_2$g(c(5,6,71)),
                 numDeriv::grad(f5, c(5,6,71),
                                method="simple"),
                 tolerance=1e-3)
    o5_3 <- optimParallel:::parallel_fg_generator(f5, ndeps=c(.01,.05))
    expect_equal(o5_3$g(c(5,6,71)),
                 numDeriv::grad(f5, c(5,6,71),
                                method="simple"),
                 tolerance=1e-3)
})
