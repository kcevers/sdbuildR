
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sdbuildR: Easily Build, Simulate, and Explore Stock-and-Flow Models in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/KCEvers/sdbuildR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KCEvers/sdbuildR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/KCEvers/sdbuildR/graph/badge.svg)](https://app.codecov.io/gh/KCEvers/sdbuildR)
[![CRAN
status](https://www.r-pkg.org/badges/version/sdbuildR)](https://CRAN.R-project.org/package=sdbuildR)
<!-- badges: end -->

Stock-and-flow models are a powerful tool for understanding complex
systems. Originating in the field of system dynamics, they represent
processes as quantities (stocks) that accumulate or deplete over time
and the processes (inflows and outflows) that change them. sdbuildR is
an R package for building, simulating, and exploring stock-and-flow
models. Get started at <https://kcevers.github.io/sdbuildR/>\!

## Features

  - **Accessibility**: Get started with stock-and-flow modelling with
    limited knowledge.
  - **Flexibility**: Easily modify models and use either R or Julia as a
    simulation backend.
  - **Rigour**: Use unit tests to verify that models behave as intended,
    and run ensemble simulations to explore model behaviour across
    parameter ranges and initial conditions.

All package capabilities are described in the vignettes:

  - [Build](https://kcevers.github.io/sdbuildR/articles/build.html):
    Build, modify, and simulate stock-and-flow
    models.
  - [Ensemble](https://kcevers.github.io/sdbuildR/articles/ensemble.html):
    Explore a model’s behaviour across parameter ranges and initial
    conditions.
  - [Unit
    tests](https://kcevers.github.io/sdbuildR/articles/unit-tests.html):
    Verify models behave as intended with unit tests.
  - [Job Demands-Resources
    Theory](https://kcevers.github.io/sdbuildR/articles/jdr.html): An
    example of formalizing psychological theory with sdbuildR.
  - [Julia
    setup](https://kcevers.github.io/sdbuildR/articles/julia-setup.html):
    Speed up simulations with
    Julia.
  - [Import/Export](https://kcevers.github.io/sdbuildR/articles/import-export.html):
    Import models from deSolve or Insight Maker, and export to other
    formats.

## Installation

The release version can be installed from CRAN:

``` r
install.packages("sdbuildR")
```

The development version can be installed from GitHub:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("KCEvers/sdbuildR")
```

## Citation

To cite sdbuildR, please use:

``` r
citation("sdbuildR")
#> To cite package 'sdbuildR' in publications use:
#> 
#>   Evers K (2025). _sdbuildR: Easily Build, Simulate, and Visualise
#>   Stock-and-Flow Models_. doi:10.32614/CRAN.package.sdbuildR
#>   <https://doi.org/10.32614/CRAN.package.sdbuildR>, R package version
#>   1.0.8.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {sdbuildR: Easily Build, Simulate, and Visualise Stock-and-Flow Models},
#>     author = {Kyra Caitlin Evers},
#>     year = {2025},
#>     note = {R package version 1.0.8},
#>     doi = {10.32614/CRAN.package.sdbuildR},
#>   }
```

## Limitations

  - Unlike in other system dynamics software, sdbuildR provides only
    minimal support for non-negative stocks and flows. Specifically,
    setting stocks to non-negative will constrain the stocks to remain
    non-negative, but will not adjust the corresponding flows. In any
    case, enforcing either stocks or flows to be non-negative is not
    recommended, as it may mask model misspecification. Stocks and flows
    that logically cannot be negative (e.g., animals or deaths) should
    ideally remain non-negative as a result of the model’s equations and
    parameters, rather than by forcing them to be non-negative.

  - sdbuildR does not support vectorized operations, destructuring
    assignment, or minimum and maximum constraints for variables.

  - sdbuildR does not support the Insight Maker functions Stop(),
    Prompt(), Confirm(), Pause(), Fix(), Map(), Filter(), and Repeat(),
    nor the delay and past functions. A message is issued if any of
    these are detected. Units (e.g., kilograms) are also not supported.

## Other system dynamics software

sdbuildR is heavily based on common system dynamics software such as
[Vensim](https://en.wikipedia.org/wiki/Vensim),
[Powersim](https://powersim.com/),
[Stella](https://www.iseesystems.com/), and [Insight
Maker](https://insightmaker.com/). To translate xmile models to R, see
the R package [readsdr](https://CRAN.R-project.org/package=readsdr). To
build stock-and-flow models with the R package
[deSolve](https://CRAN.R-project.org/package=deSolve), the book [System
Dynamics Modeling with
R](https://link.springer.com/book/10.1007/978-3-319-34043-2) by Jim
Duggan will prove useful. In Python, stock-and-flow models are supported
by [PySD](https://doi.org/10.21105/joss.04329).

## Troubleshooting

sdbuildR is under active development. While thoroughly tested, the
package may have bugs, particularly in complex model translations. We
encourage users to report [issues on
GitHub](https://github.com/KCEvers/sdbuildR/issues) - your input helps
the package improve\! Use `summary()` to run model diagnostics, and use
the vignettes for guidance.
