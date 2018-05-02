The R package optimParallel 
===========================

The package provides parallel versions of the gradient-based optim methods
"L-BFGS-B", "BFGS", and "CG". If the evaluation of the function fn takes more than 0.05 seconds,
optimParallel can significantly reduce the optimization time. For a p-parameter optimization based
on "L-BFGS-B", the speed increase is about factor 1+2p when no analytic gradient is specified and
1+2p processor cores are available.

See the ArXiv e-prints URL http://arxiv.org/abs/1804.11058
also available as package vignette.

R> vignette("optimParallel")