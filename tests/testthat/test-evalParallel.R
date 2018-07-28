## rm(list=ls())
## library("testthat")
## library("optimParallel", lib.loc = "../../../lib/")
context("test-evalParallel")
source("testsetup.R")

f1 <- function(x){
    x
}
f2 <- function(x,y){
    sum(10*x, y)
}
f3 <- function(x,y=1){
    x+y
}
fnames <- function(x){
    x["a"]+x["b"]+x["c"]
}


test_that("evalParallel: f1", {
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=NULL,
                              firstArg=c(1), parnames=NULL),
                 list(1))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=NULL,
                              firstArg=c(1,2), parnames=NULL),
                 list(c(1,2)))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=NULL,
                              firstArg=matrix(c(1,2), ncol=2), parnames=NULL),
                 list(1,2))

    
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=NULL,
                              firstArg=c(1,2,3,4,5), parnames=NULL),
                 list(c(1,2,3,4,5)))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=list(a=1),
                              firstArg=c(1,2,3,4,5), parnames=NULL),
                 list(c(1,2,3,4,5)))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=list(x=100),
                              firstArg=c(1,2,3,4,5), parnames=NULL),
                 list(c(1,2,3,4,5)))

    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=NULL,
                              firstArg=matrix(c(1,2,3,4,5), ncol=5), parnames=NULL),
                 list(1,2,3,4,5))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=list(a=1),
                              firstArg=matrix(c(1,2,3,4,5), ncol=5), parnames=NULL),
                 list(1,2,3,4,5))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=list(x=100),
                              firstArg=matrix(c(1,2,3,4,5), ncol=5), parnames=NULL),
                 list(1,2,3,4,5))
})

test_that("evalParallel: f2", {
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f2, args=list(x=1,y=1),
                              firstArg=matrix(c(1:4), ncol=4), parnames=NULL),
                 list(f2(x=1, y=1),
                      f2(x=2, y=1),
                      f2(x=3, y=1),
                      f2(x=4, y=1)))

    expect_equal(optimParallel:::evalParallel(cl=cl, f=f2, args=list(x=1,y=1),
                              firstArg=t(array(1:4, c(4,3))), parnames=NULL),
                 list(f2(x=c(1,1,1), y=1),
                      f2(x=c(2,2,2), y=1),
                      f2(x=c(3,3,3), y=1),
                      f2(x=c(4,4,4), y=1)))

    expect_equal(optimParallel:::evalParallel(cl=cl, f=f2, args=list(y=1,x=1),
                              firstArg=t(array(1:4, c(4,3))), parnames=NULL),
                 list(f2(x=c(1,1,1), y=1),
                      f2(x=c(2,2,2), y=1),
                      f2(x=c(3,3,3), y=1),
                      f2(x=c(4,4,4), y=1)))

    expect_error(optimParallel:::evalParallel(cl=cl, f=f2, args=list(x=1),
                              firstArg=t(array(1:4, c(4,3))), parnames=NULL))
})


test_that("evalParallel: f3 defaults", {
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f3, args=list(x=1),
                              firstArg=1, parnames=NULL),
                 list(f3(x=1)))
})

test_that("evalParallel: fnames", {
    expect_equal(optimParallel:::evalParallel(cl=cl, f=fnames, args=list(x=1),
                              firstArg=array(1:3, c(3,1)), parnames=c("a","b","c")),
                 list(fnames(x=c(a=1,b=2,c=3))))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=fnames, args=list(x=1),
                                              firstArg=array(1:6, c(3,2)),
                                              parnames=c("a","b","c")),
                 list(fnames(x=c(a=1,b=2,c=3)),
                      fnames(x=c(a=4,b=5,c=6))))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=fnames, args=list(x=1),
                                              firstArg=array(1:9, c(3,3)),
                                              parnames=c("a","b","c")),
                 list(fnames(x=c(a=1,b=2,c=3)),
                      fnames(x=c(a=4,b=5,c=6)),
                      fnames(x=c(a=7,b=8,c=9))))
})
