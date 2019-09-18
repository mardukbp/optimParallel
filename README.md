This branch has a modified `DESCRIPTION`file that allows installing this package in pqR.

The R package optimParallel 
===========================

The package provides a parallel versions of the L-BFGS-B optim method.
If the evaluation of the function fn takes more than 0.1 seconds,
optimParallel can significantly reduce the optimization time. For a p-parameter optimization,
the speed increase is about factor 1+2p when no analytic gradient is specified and
1+2p processor cores are available.

See the ArXiv e-prints URL http://arxiv.org/abs/1804.11058
also available as package vignette for more information.

R> vignette("optimParallel")
