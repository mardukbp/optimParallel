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


test_that("evalParallel: f1", {
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=NULL,
                              firstArg=c(1)),
                 list(1))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=NULL,
                              firstArg=c(1,2)),
                 list(c(1,2)))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=NULL,
                              firstArg=matrix(c(1,2), ncol=2)),
                 list(1,2))

    
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=NULL,
                              firstArg=c(1,2,3,4,5)),
                 list(c(1,2,3,4,5)))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=list(a=1),
                              firstArg=c(1,2,3,4,5)),
                 list(c(1,2,3,4,5)))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=list(x=100),
                              firstArg=c(1,2,3,4,5)),
                 list(c(1,2,3,4,5)))

    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=NULL,
                              firstArg=matrix(c(1,2,3,4,5), ncol=5)),
                 list(1,2,3,4,5))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=list(a=1),
                              firstArg=matrix(c(1,2,3,4,5), ncol=5)),
                 list(1,2,3,4,5))
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f1, args=list(x=100),
                              firstArg=matrix(c(1,2,3,4,5), ncol=5)),
                 list(1,2,3,4,5))
})

test_that("evalParallel: f2", {
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f2, args=list(x=1,y=1),
                              firstArg=matrix(c(1:4), ncol=4)),
                 list(f2(x=1, y=1),
                      f2(x=2, y=1),
                      f2(x=3, y=1),
                      f2(x=4, y=1)))

    expect_equal(optimParallel:::evalParallel(cl=cl, f=f2, args=list(x=1,y=1),
                              firstArg=t(array(1:4, c(4,3)))),
                 list(f2(x=c(1,1,1), y=1),
                      f2(x=c(2,2,2), y=1),
                      f2(x=c(3,3,3), y=1),
                      f2(x=c(4,4,4), y=1)))

    expect_equal(optimParallel:::evalParallel(cl=cl, f=f2, args=list(y=1,x=1),
                              firstArg=t(array(1:4, c(4,3)))),
                 list(f2(x=c(1,1,1), y=1),
                      f2(x=c(2,2,2), y=1),
                      f2(x=c(3,3,3), y=1),
                      f2(x=c(4,4,4), y=1)))

    expect_error(optimParallel:::evalParallel(cl=cl, f=f2, args=list(x=1),
                              firstArg=t(array(1:4, c(4,3)))))
})


test_that("evalParallel: f3 defaults", {
    expect_equal(optimParallel:::evalParallel(cl=cl, f=f3, args=list(x=1),
                              firstArg=1),
                 list(f3(x=1)))
})
