# Changelog

## sdbuildR (development version)

- Much faster R-to-Julia equation translation via a new AST-based
  translator that walks R’s parse tree instead of regex string-rewriting
  (~14x faster on equation-heavy models). It falls back to the previous
  translator for any construct it does not handle, so behaviour is
  unchanged; namespaced calls such as
  [`base::sum()`](https://rdrr.io/r/base/sum.html) now translate
  correctly.
- Faster, content-hashed assembly cache. Repeated
  [`simulate()`](https://rdrr.io/r/stats/simulate.html)/`compile()`/[`summary()`](https://rdrr.io/r/base/summary.html)
  with no intervening edits now return essentially instantly (the
  model’s inputs are hashed; an unchanged hash short-circuits
  reassembly). The R-\>Julia translation of each equation is memoised
  per variable, so editing one equation only retranslates that equation.
  The first built-in-function pass also skips the ~180 function patterns
  an equation does not use.
- Fixed a bug where converting a variable to a stock (or otherwise
  adding, removing, or renaming stocks) could misalign each stock’s
  derivative slot with the state vector in Julia simulations, silently
  swapping stock dynamics. Stock derivative indices are now kept
  consistent with the state-vector order.
- Added an internal structural validator that checks model layout (stock
  derivative alignment, unique variable names) before code generation,
  turning this class of inconsistency into an explicit error rather than
  a wrong result.
- Fixed a temporary CSV file leak in single Julia simulations.
- textutils is now a required, not suggested dependency.

## sdbuildR 1.0.8

CRAN release: 2025-11-19

### Minor improvements and bug fixes

- Fixed bug in replacing scientifically formatted numbers
- Fixed bug in comparing Julia versions
- Added alias “sigmoid” for logistic function
- Created constructor and validator for class sdbuildR_sim to improve
  consistency in output

## sdbuildR 1.0.7

CRAN release: 2025-11-03

## sdbuildR 1.0.6

- Fixed error in finding Julia installation

- Simulations in Julia are now ensured to stop at exact simulation
  times, which removes unexpected results created by numerical errors in
  the solver (using tstops argument in solve())

## sdbuildR 1.0.5

CRAN release: 2025-10-29

## sdbuildR 1.0.4

## sdbuildR 1.0.3

- Removed automatic installation of Julia packages and instead wrote a
  separate function for this: install_julia_env(). install_julia_env()
  is called in .onLoad() ONLY if the custom environmental variable
  AUTO_INSTALL_JULIA_ENV is “true” and NOT_CRAN is “true”. This is to
  ensure GitHub workflows have the Julia environment instantiated, but
  this will not affect users, as they do not have
  AUTO_INSTALL_JULIA_ENV.

- Added vignette for formalizing Job Demands-Resources (JDR) Theory

- Improved documentation

## sdbuildR 1.0.2

## sdbuildR 1.0.1

## sdbuildR 1.0.0

- Added extensive solver options
- Created Julia package (SystemDynamicsBuildR.jl) to contain sdbuildR
  Julia code
- Added vignette for support in installing and setting up Julia
- Added obligatory argument `times` to step, pulse, ramp, and seasonal
