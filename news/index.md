# Changelog

## sdbuildR (development version)

### Breaking changes

- [`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)
  is now the constructor for stock-and-flow models. Code that used
  `xmile()` or the development-only `sdbuildR()` constructor should call
  [`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)
  instead.

- [`update()`](https://rdrr.io/r/stats/update.html) now replaces the old
  `build()` workflow for general model edits. For clearer model-building
  code, use
  [`stock()`](https://kcevers.github.io/sdbuildR/reference/stock.md),
  [`flow()`](https://kcevers.github.io/sdbuildR/reference/flow.md),
  [`constant()`](https://kcevers.github.io/sdbuildR/reference/constant.md),
  [`aux()`](https://kcevers.github.io/sdbuildR/reference/auxiliary.md)/[`auxiliary()`](https://kcevers.github.io/sdbuildR/reference/auxiliary.md),
  [`lookup()`](https://kcevers.github.io/sdbuildR/reference/lookup.md),
  and
  [`custom_func()`](https://kcevers.github.io/sdbuildR/reference/custom_func.md).

- `stockflow` is now the primary model class. Code that checked for
  `sdbuildR_xmile` or `sdbuildR` should check for `stockflow` instead.

- [`simulate()`](https://rdrr.io/r/stats/simulate.html) now returns
  `simulate_stockflow` objects,
  [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
  returns `ensemble_stockflow` objects,
  [`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md)
  returns `verify_stockflow` objects,
  [`summary()`](https://rdrr.io/r/base/summary.html) returns
  `summary_stockflow` objects, and
  [`compare_models()`](https://kcevers.github.io/sdbuildR/reference/compare_models.md)
  returns `compare_stockflow` objects.

- [`sim_settings()`](https://kcevers.github.io/sdbuildR/reference/sim_settings.md)
  now replaces `sim_specs()`, and
  [`meta()`](https://kcevers.github.io/sdbuildR/reference/meta.md) now
  replaces `header()`.

- [`summary()`](https://rdrr.io/r/base/summary.html) now runs model
  diagnostics. Code that used
  [`debugger()`](https://rdrr.io/r/utils/debugger.html) should call
  [`summary()`](https://rdrr.io/r/base/summary.html) instead.

- [`dependencies()`](https://kcevers.github.io/sdbuildR/reference/dependencies.md)
  now replaces `find_dependencies()`.

- `export_model(format = "sdbuildR")` now replaces `get_build_code()`.

- [`sim_methods()`](https://kcevers.github.io/sdbuildR/reference/sim_methods.md)
  now replaces `solvers()`.

- [`custom_func()`](https://kcevers.github.io/sdbuildR/reference/custom_func.md)
  now replaces `macro()` for user-defined model functions.

- [`import_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/import_insightmaker.md)
  now replaces `insightmaker_to_sfm()`, and
  [`url_to_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/url_to_insightmaker.md)
  now replaces `url_to_IM()`.

- [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
  now uses `conditions` instead of `range`. To retain individual
  simulations, use `save_sims = TRUE` instead of `return_sims = TRUE`.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods now
  use `show_constants` instead of `add_constants`. For ensemble plots,
  use `which`, `sim`, `condition`, and `label_subplots` instead of
  `type`, `i`, `j`, and `j_labels`.

- `use_julia(nthreads = )` now controls Julia threading. Code that used
  `use_threads()` should set the thread count with
  [`use_julia()`](https://kcevers.github.io/sdbuildR/reference/use_julia.md)
  instead.

- Unit-specific helpers from 1.x, including `u()`, `convert_u()`,
  `drop_u()`, `model_units()`, `unit_prefixes()`, `get_units()`,
  `get_regex_units()`, and `get_regex_time_units()`, are no longer part
  of the public API.

### New features

- [`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)
  creates an empty model or loads a built-in template using a
  case-insensitive `template` name.

- [`stock()`](https://kcevers.github.io/sdbuildR/reference/stock.md),
  [`flow()`](https://kcevers.github.io/sdbuildR/reference/flow.md),
  [`constant()`](https://kcevers.github.io/sdbuildR/reference/constant.md),
  [`aux()`](https://kcevers.github.io/sdbuildR/reference/auxiliary.md)/[`auxiliary()`](https://kcevers.github.io/sdbuildR/reference/auxiliary.md),
  [`lookup()`](https://kcevers.github.io/sdbuildR/reference/lookup.md),
  and
  [`custom_func()`](https://kcevers.github.io/sdbuildR/reference/custom_func.md)
  provide typed model-building helpers around
  [`update()`](https://rdrr.io/r/stats/update.html).

- [`update()`](https://rdrr.io/r/stats/update.html) now supports bare
  variable names and R expressions in `name`, `type`, `eqn`, `to`,
  `from`, and `source`. Strings and programmatic injection with `!!`
  remain supported.

- [`change_name()`](https://kcevers.github.io/sdbuildR/reference/change_name.md),
  [`change_type()`](https://kcevers.github.io/sdbuildR/reference/change_type.md),
  and
  [`discard()`](https://kcevers.github.io/sdbuildR/reference/discard.md)
  provide focused helpers for renaming, changing, and removing model
  variables.

- [`sim_settings()`](https://kcevers.github.io/sdbuildR/reference/sim_settings.md)
  gains flexible output controls with `save_at`, `save_n`, `vars`, and
  `save_sims`.

- [`simulate()`](https://rdrr.io/r/stats/simulate.html) accepts
  temporary simulation-setting overrides through `...`, using the same
  arguments as
  [`sim_settings()`](https://kcevers.github.io/sdbuildR/reference/sim_settings.md).

- [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
  can run ensembles in R and Julia, vary stocks and constants with
  `conditions`, and retain individual simulations with
  `save_sims = TRUE`.

- [`unit_test()`](https://kcevers.github.io/sdbuildR/reference/unit_test.md),
  [`unit_tests()`](https://kcevers.github.io/sdbuildR/reference/unit_tests.md),
  and
  [`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md)
  add model-level unit tests for expected simulation behavior, including
  tests under alternative `conditions`.

- [`compare_models()`](https://kcevers.github.io/sdbuildR/reference/compare_models.md)
  compares model structure, equations, simulation settings, and
  nonlinearity scores across two `stockflow` models.

- [`import_desolve()`](https://kcevers.github.io/sdbuildR/reference/import_desolve.md)
  converts deSolve-style ODE models into `stockflow` models.

- [`import_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/import_insightmaker.md)
  imports Insight Maker models from public URLs, `.InsightMaker` files,
  and ModelJSON `.json` files.

- [`export_model()`](https://kcevers.github.io/sdbuildR/reference/export_model.md)
  exports models as sdbuildR reconstruction code, standalone deSolve
  scripts, or Psychomodels JSON records.

### Improvements and fixes

- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
  now support the `stockflow`, `simulate_stockflow`,
  `ensemble_stockflow`, and `verify_stockflow` classes.

- [`dependencies()`](https://kcevers.github.io/sdbuildR/reference/dependencies.md)
  can now filter dependencies by variable name and type.

- [`export_plot()`](https://kcevers.github.io/sdbuildR/reference/export_plot.md)
  now supports the current plot classes and additional graph export
  options.

- [`simulate()`](https://rdrr.io/r/stats/simulate.html) and Julia code
  generation now use a faster AST-based R-to-Julia equation translator.
  Namespaced calls such as
  [`base::sum()`](https://rdrr.io/r/base/sum.html) translate correctly,
  and unsupported constructs fall back to the previous translator.

- [`simulate()`](https://rdrr.io/r/stats/simulate.html), compilation,
  and [`summary()`](https://rdrr.io/r/base/summary.html) now use a
  content-hashed assembly cache. Repeated calls on unchanged models can
  skip reassembly, and editing one equation only retranslates that
  equation.

- [`simulate()`](https://rdrr.io/r/stats/simulate.html) now keeps Julia
  stock derivative positions aligned with the state vector when stocks
  are added, removed, renamed, or created by changing a variable’s type.

- [`summary()`](https://rdrr.io/r/base/summary.html) and code generation
  now use an internal structural validator to catch inconsistent model
  layouts before simulation.

- [`simulate()`](https://rdrr.io/r/stats/simulate.html) now removes
  temporary CSV files created by single Julia simulations.

- `textutils` is now a required dependency.

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
