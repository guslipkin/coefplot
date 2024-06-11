<!-- README.md is generated from README.Rmd. Please edit that file -->

# coefplot <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/jaredlander/coefplot/workflows/R-CMD-check/badge.svg)](https://github.com/jaredlander/coefplot/actions)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/coefplot)](https://CRAN.R-project.org/package=coefplot)
[![Downloads from the RStudio CRAN
mirror](https://cranlogs.r-pkg.org/badges/coefplot)](https://CRAN.R-project.org/package=coefplot)
[![Coverage
Status](https://img.shields.io/codecov/c/github/jaredlander/coefplot/master.svg)](https://codecov.io/github/jaredlander/coefplot?branch=master)
<!-- badges: end -->

Coefplot is a package for plotting the coefficients and standard errors
from a variety of models. Currently `stats::lm()`, `stats::glm()`,
[`parsnip`](https://parsnip.tidymodels.org/) and
[`workflows`](https://workflows.tidymodels.org/) fitted models,
`glmnet::glmnet()`, `maxLik::maxLik()`, `RevoScaleR::rxLinMod()`,
`RevoScaleR::rxGLM()` and `revoScaleR::rxLogit()` are supported.

The package is designed for S3 dispatch from the functions coefplot and
getModelInfo to make for easy additions of new models.

If interested in helping please contact the package author.

## Example

![](man/figures/the_plot.png)
