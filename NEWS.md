# sdbuildR (development version)

## Breaking changes

* `stockflow()` is now the constructor for stock-and-flow models. Code that
  used `xmile()` or the development-only `sdbuildR()` constructor should call
  `stockflow()` instead.

* `update()` now replaces the old `build()` workflow for general model edits.
  For clearer model-building code, use `stock()`, `flow()`, `constant()`,
  `aux()`/`auxiliary()`, `lookup()`, and `custom_func()`.

* `stockflow` is now the primary model class. Code that checked for
  `sdbuildR_xmile` or `sdbuildR` should check for `stockflow` instead.

* `simulate()` now returns `simulate_stockflow` objects, `ensemble()` returns
  `ensemble_stockflow` objects, `verify()` returns `verify_stockflow` objects,
  `summary()` returns `summary_stockflow` objects, and `compare_models()`
  returns `compare_stockflow` objects.

* `sim_settings()` now replaces `sim_specs()`, and `meta()` now replaces
  `header()`.

* `summary()` now runs model diagnostics. Code that used `debugger()` should
  call `summary()` instead.

* `dependencies()` now replaces `find_dependencies()`.

* `export_model(format = "sdbuildR")` now replaces `get_build_code()`.

* `sim_methods()` now replaces `solvers()`.

* `custom_func()` now replaces `macro()` for user-defined model functions.

* `import_insightmaker()` now replaces `insightmaker_to_sfm()`, and
  `url_to_insightmaker()` now replaces `url_to_IM()`.

* `ensemble()` now uses `conditions` instead of `range`. To retain individual
  simulations, use `save_sims = TRUE` instead of `return_sims = TRUE`.

* `plot()` methods now use `show_constants` instead of `add_constants`. For
  ensemble plots, use `which`, `sim`, `condition`, and `label_subplots` instead
  of `type`, `i`, `j`, and `j_labels`.

* `use_julia(nthreads = )` now controls Julia threading. Code that used
  `use_threads()` should set the thread count with `use_julia()` instead.

* Unit-specific helpers from 1.x, including `u()`, `convert_u()`, `drop_u()`,
  `model_units()`, `unit_prefixes()`, `get_units()`, `get_regex_units()`, and
  `get_regex_time_units()`, are no longer part of the public API.

## New features

* `stockflow()` creates an empty model or loads a built-in template using a
  case-insensitive `template` name.

* `stock()`, `flow()`, `constant()`, `aux()`/`auxiliary()`, `lookup()`, and
  `custom_func()` provide typed model-building helpers around `update()`.

* `update()` now supports bare variable names and R expressions in `name`,
  `type`, `eqn`, `to`, `from`, and `source`. Strings and programmatic injection
  with `!!` remain supported.

* `change_name()`, `change_type()`, and `discard()` provide focused helpers for
  renaming, changing, and removing model variables.

* `sim_settings()` gains flexible output controls with `save_at`, `save_n`,
  `vars`, and `save_sims`.

* `simulate()` accepts temporary simulation-setting overrides through `...`,
  using the same arguments as `sim_settings()`.

* `ensemble()` can run ensembles in R and Julia, vary stocks and constants with
  `conditions`, and retain individual simulations with `save_sims = TRUE`.

* `unit_test()`, `unit_tests()`, and `verify()` add model-level unit tests for
  expected simulation behavior, including tests under alternative `conditions`.

* `compare_models()` compares model structure, equations, simulation settings,
  and nonlinearity scores across two `stockflow` models.

* `import_desolve()` converts deSolve-style ODE models into `stockflow` models.

* `import_insightmaker()` imports Insight Maker models from public URLs,
  `.InsightMaker` files, and ModelJSON `.json` files.

* `export_model()` exports models as sdbuildR reconstruction code, standalone
  deSolve scripts, or Psychomodels JSON records.

## Improvements and fixes

* `as.data.frame()` methods now support the `stockflow`, `simulate_stockflow`,
  `ensemble_stockflow`, and `verify_stockflow` classes.

* `dependencies()` can now filter dependencies by variable name and type.

* `export_plot()` now supports the current plot classes and additional graph
  export options.

* `simulate()` and Julia code generation now use a faster AST-based R-to-Julia
  equation translator. Namespaced calls such as `base::sum()` translate
  correctly, and unsupported constructs fall back to the previous translator.

* `simulate()`, compilation, and `summary()` now use a content-hashed assembly
  cache. Repeated calls on unchanged models can skip reassembly, and editing one
  equation only retranslates that equation.

* `simulate()` now keeps Julia stock derivative positions aligned with the state
  vector when stocks are added, removed, renamed, or created by changing a
  variable's type.

* `summary()` and code generation now use an internal structural validator to
  catch inconsistent model layouts before simulation.

* `simulate()` now removes temporary CSV files created by single Julia
  simulations.

* `textutils` is now a required dependency.

# sdbuildR 1.0.8

## Minor improvements and bug fixes

* Fixed bug in replacing scientifically formatted numbers
* Fixed bug in comparing Julia versions
* Added alias "sigmoid" for logistic function
* Created constructor and validator for class sdbuildR_sim to improve consistency in output

# sdbuildR 1.0.7

# sdbuildR 1.0.6

* Fixed error in finding Julia installation 

* Simulations in Julia are now ensured to stop at exact simulation times, which removes unexpected results created by numerical errors in the solver (using tstops argument in solve()) 

# sdbuildR 1.0.5

# sdbuildR 1.0.4

# sdbuildR 1.0.3

* Removed automatic installation of Julia packages and instead wrote a separate function for this: install_julia_env(). install_julia_env() is called in .onLoad() ONLY if the custom environmental variable AUTO_INSTALL_JULIA_ENV is "true" and NOT_CRAN is "true". This is to ensure GitHub workflows have the Julia environment instantiated, but this will not affect users, as they do not have AUTO_INSTALL_JULIA_ENV.

* Added vignette for formalizing Job Demands-Resources (JDR) Theory 

* Improved documentation

# sdbuildR 1.0.2

# sdbuildR 1.0.1

# sdbuildR 1.0.0

* Added extensive solver options
* Created Julia package (SystemDynamicsBuildR.jl) to contain sdbuildR Julia code
* Added vignette for support in installing and setting up Julia
* Added obligatory argument `times` to step, pulse, ramp, and seasonal
