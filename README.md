
<!-- README.md is generated from README.Rmd. Please edit that file -->
jpaper
======

Installation
------------

<!-- badges: start -->
<!-- badges: end -->
You can install the released version of jpaper from [CRAN](https://CRAN.R-project.org) with:

``` r
devtools::install_git("https://gitee.com/adv-r/jpaper")
```

Example
-------

``` r
library(jpaper)

set.seed(1)
x <- rnorm(100)
y <- rnorm(100)
t_test(x, y)
#>        first     second       diff        lwr       upr     p adj     tval
#> 1: 0.11±0.90 -0.04±0.96 -0.1466954 -0.4056458 0.1122549 0.2652846 1.117149
#>       pvalue
#> 1: 0.2652902
```
