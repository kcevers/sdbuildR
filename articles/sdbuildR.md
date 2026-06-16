# Get started with sdbuildR

``` r

library(sdbuildR)
```

sdbuildR is an R package for building, simulating, and testing
stock-and-flow models. This page gives a quick overview of its main
features.

### Load models from the model library

Dozens of example models can be loaded using
[`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).
Here we load the SIR (Susceptible-Infected-Recovered) model, a classic
model in epidemiology:

``` r

sfm <- stockflow("SIR")
print(sfm)
#> 
#> ── Stock-and-Flow Model: Susceptible-Infected-Recovered (SIR) ──────────────────
#> 3 stocks • 2 flows • 4 constants
#> 
#> ── Stock-Flow Structure ──
#> 
#> infected: + new_infections - new_recoveries
#> recovered: + new_recoveries
#> susceptible: - new_infections
#> 
#> ── Other Variables ──
#> 
#> Constants: `contact_rate`, `infection_rate`, `recovery_rate`, and
#> `total_population`
#> 
#> ── Simulation Settings ──
#> 
#> Time: 0.0 to 20.0 weeks (dt = 0.01) • euler • R
```

Plot the stock-and-flow diagram for a structural overview:

``` r

plot(sfm)
```

Simulate and plot the model’s behaviour over time:

``` r

sim <- simulate(sfm)
plot(sim)
```

### Build a model from scratch

The equivalent stock-and-flow model can also be built from scratch. We
initialise an empty model with
[`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)
and add three stocks.

``` r

sfm <- stockflow() |>
  stock(susceptible, eqn = 99999, label = "Susceptible") |>
  stock(infected, eqn = 1, label = "Infected") |>
  stock(recovered, eqn = 0, label = "Recovered")
```

Above, we use the pipe operator `|>` for better legibility. It simply
passes the result of an expression to the next expression as its first
argument. Next, we add four constants to the model:

``` r

sfm <- sfm |>
  constant(total_population,
    eqn = "susceptible + infected + recovered",
    label = "Total population"
  ) |>
  constant(contact_rate, eqn = 2, label = "Contact rate") |>
  constant(recovery_rate, eqn = 0.1, label = "Recovery rate") |>
  constant(infection_rate,
    eqn = "contact_rate / total_population",
    label = "Infection rate"
  )
```

Finally, we add two flows that move population from one stock to
another:

``` r

sfm <- sfm |>
  flow(new_infections,
    eqn = "infection_rate * susceptible * infected",
    from = "susceptible", to = "infected", label = "New infections"
  ) |>
  flow(new_recoveries,
    eqn = "recovery_rate * infected",
    from = "infected", to = "recovered", label = "New recoveries"
  )

print(sfm)
#> 
#> ── Stock-and-Flow Model ────────────────────────────────────────────────────────
#> 3 stocks • 2 flows • 4 constants
#> 
#> ── Stock-Flow Structure ──
#> 
#> infected: + new_infections - new_recoveries
#> recovered: + new_recoveries
#> susceptible: - new_infections
#> 
#> ── Other Variables ──
#> 
#> Constants: `contact_rate`, `infection_rate`, `recovery_rate`, and
#> `total_population`
#> 
#> ── Simulation Settings ──
#> 
#> Time: 0 to 100 seconds (dt = 0.01) • euler • R
```

Simulation settings such as the time range and time step (`dt`) are
configured with
[`sim_settings()`](https://kcevers.github.io/sdbuildR/reference/sim_settings.md).

``` r

sfm <- sfm |>
  sim_settings(start = 0, stop = 20, time_units = "weeks") |>
  # Add model name
  meta(name = "Susceptible-Infected-Recovered (SIR)")
```

``` r

sim <- simulate(sfm)
plot(sim)
```

See the [Build](https://kcevers.github.io/sdbuildR/articles/build.html)
vignette for a full guide on constructing and modifying models.

### Ensemble simulations

Running multiple simulations (i.e., an ensemble) provides insight into a
model’s behavioural variability. Here, we initialize all stocks to a
random value:

``` r

sfm_ens <- stockflow("SIR") |>
  update(c(susceptible, infected, recovered), eqn = runif(1, 1, 1000)) |>
  # Save fewer values for computational efficiency
  sim_settings(stop = 50, save_at = 1)
```

``` r

sims <- ensemble(sfm_ens, n = 100)
#> Starting ensemble simulation in "R" with 100 simulations.
#> ✔ Ensemble simulation completed in 6.7647 seconds.
plot(sims)
```

Ensemble simulations can be run in parallel in R for speed, or in Julia
for even faster performance.

See the
[Ensemble](https://kcevers.github.io/sdbuildR/articles/ensemble.html)
vignette for more, including varying multiple parameters, parallel
execution, and accessing simulation data.

### Unit tests

Stock-and-flow models can easily become complex, producing unexpected
behaviours as the model is developed. Unit tests assert that a model
behaves as expected. An expression that evaluates to `TRUE` means the
test passes. Here we check that the susceptible population never becomes
negative, and that the total population is conserved:

``` r

sfm <- stockflow("SIR") |>
  unit_test(expr = all(susceptible >= 0)) |>
  unit_test(
    expr = all(abs(susceptible + infected + recovered - total_population) < 1e-8),
    label = "Population is conserved"
  )

verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 2/2 tests passed.
#> ✔ 1. susceptible is at least 0 (for all values)
#> ✔ 2. Population is conserved
```

See the [Unit
tests](https://kcevers.github.io/sdbuildR/articles/unit-tests.html)
vignette for more, including conditional tests, visualizing and
extracting test results, and debugging failed tests.

## Learn more

- [Build](https://kcevers.github.io/sdbuildR/articles/build.html):
  Build, modify, and simulate stock-and-flow models.
- [Ensemble
  simulations](https://kcevers.github.io/sdbuildR/articles/ensemble.html):
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
  Speed up simulations with Julia.
- [Import/Export](https://kcevers.github.io/sdbuildR/articles/import-export.html):
  Import models from deSolve or Insight Maker, and export to other
  formats.

## Citation

To cite sdbuildR, please use:

``` r

citation("sdbuildR")
#> To cite package 'sdbuildR' in publications use:
#> 
#>   Evers K (2025). _sdbuildR: Easily Build, Simulate, and Visualise
#>   Stock-and-Flow Models_. doi:10.32614/CRAN.package.sdbuildR
#>   <https://doi.org/10.32614/CRAN.package.sdbuildR>. R package version
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
