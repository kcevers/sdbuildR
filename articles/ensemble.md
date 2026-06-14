# Ensemble simulations

``` r

library(sdbuildR)
```

After having built a stock-and-flow model, you may want to explore how
different parameter values affect the model’s behaviour. Running
multiple simulations with varying parameters is also called an ensemble,
which provides insight into the range of possible outcomes and
uncertainty associated with your model. In this vignette, we will
explore how to set up and run ensemble simulations using the sdbuildR
package.

## Setting up the model

For this example, we will use Crielaard et al.’s (2022) model of eating
behaviour, including the stocks hunger, eating, and compensatory
behaviour (i.e., disordered eating behaviour such as purging and
overexercising). For more details, see Crielaard et al. (2022). We can
load this example from the model library and look what is inside:

``` r

sfm <- sdbuildR("Crielaard2022")
print(sfm)
#> 
#> ── Stock-and-Flow Model: Eating Behaviour (Crielaard et al., 2022) ─────────────
#> 3 stocks • 8 flows • 3 constants
#> 
#> ── Stock-Flow Structure ──
#> 
#> Compensatory_behaviour: + Compensating_for_having_eaten -
#> Satisfaction_with_hungry_feeling
#> Food_intake: + Effect_of_eating_triggers + Feeling_hunger -
#> Effect_of_compensatory_behavior - Satiety
#> Hunger: + Losing_energy_by_compensatory_behavior - Food_intake_reduces_hunger
#> 
#> ── Other Variables ──
#> 
#> Constants: `a0`, `a1`, and `a2`
#> 
#> ── Simulation Settings ──
#> 
#> Time: 0.0 to 100.0 days (dt = 0.01) • euler • R
```

Simulations run in R by default. If you want to use Julia for faster
execution or additional parallelization options, you can activate the
Julia environment for sdbuildR and change the simulation language to
Julia (see below). For guidance on installing and setting up the Julia
environment, see [this
vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html).

Without changing the parameters, we can run a single simulation to see
how the model behaves:

``` r

sim <- simulate(sfm)
plot(sim)
```

As the model has random initial conditions, another run will be
different:

``` r

sim <- simulate(sfm)
plot(sim)
```

To explore this more systematically, we can run an ensemble simulation
using the
[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
function.

## Running ensemble simulations

Ensemble simulations create multiple runs of the model, which only makes
sense if the model either has some random elements or if parameters are
being varied. Our model already has random initial conditions, but if it
did not, we could create these:

``` r

sfm <- update(sfm, c(Food_intake, Hunger, Compensatory_behaviour),
  eqn = runif(1)
)
```

With random initial conditions, multiple runs of the same model will be
different. As running ensemble simulations can be quite memory
intensive, it is highly recommended to reduce the size of the returned
timeseries. This will save memory and speed up the simulation. For
example, we can only save the timeseries every 1 time units:

``` r

sfm <- sim_settings(sfm, save_at = 1)
```

The model is now ready for running ensemble simulations. We complete 100
runs using the
[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
function:

``` r

sims <- ensemble(sfm, n = 100)
#> Starting ensemble simulation in "R" with 100 simulations.
#> ✔ Ensemble simulation completed in 21.9946 seconds.
```

``` r

plot(sims)
```

The plot shows the mean and confidence interval of the stocks (mean with
95% confidence interval). We can also plot the individual runs, for
which we first have to rerun the simulation with `save_sims = TRUE`:

``` r

sims <- ensemble(sfm, n = 30, save_sims = TRUE)
#> Starting ensemble simulation in "R" with 30 simulations.
#> ✔ Ensemble simulation completed in 6.5798 seconds.
```

``` r

plot(sims, which = "sims")
```

This automatically only plots the first ten simulations, as plotting a
large number of simulations can be quite slow. We can change which
simulations we plot by specifying the `i` argument:

``` r

plot(sims, which = "sims", sim = 15:30)
```

By default, only the stocks are saved. To save all variables, set
`only_stocks = FALSE`:

``` r

sims <- ensemble(sfm, n = 100, only_stocks = FALSE)
#> Starting ensemble simulation in "R" with 100 simulations.
#> ✔ Ensemble simulation completed in 72.7342 seconds.
```

``` r

plot(sims)
```

### Parallel simulations (R)

By default, R ensemble simulations run sequentially. To run simulations
in parallel, use the `future` package to control parallel execution.
First, check that the `future` and `future.apply` packages are
available:

``` r

if (requireNamespace("future", quietly = TRUE) &&
  requireNamespace("future.apply", quietly = TRUE)) {
  # Set up parallel execution with 4 workers
  future::plan(future::multisession, workers = 4)

  # Run ensemble simulations (now in parallel)
  sims <- ensemble(sfm, n = 100)

  # Restore sequential execution
  future::plan(future::sequential)
}
```

On Windows, `multisession` is the recommended backend. On POSIX systems
(Linux, macOS), you can also use `multicore` for potentially better
performance, though `multisession` will work on those systems as well.
The `workers` argument specifies how many parallel processes to use;
adjust this based on your system’s capabilities.

#### Ensemble simulations (Julia)

Ensemble simulations can also be run in Julia, which is typically faster
than R for large simulations. To use the Julia backend, first activate
the Julia environment for sdbuildR:

``` r

use_julia()
#> ℹ Activating Julia environment for sdbuildR at
#>   /home/runner/work/_temp/Library/sdbuildR...
#> ✔ Julia environment ready.
```

Then, set the simulation language to Julia:

``` r

sfm <- sim_settings(sfm, language = "julia")
```

##### Parallel simulations (Julia)

We can also enable parallel execution in Julia by setting the number of
threads:

``` r

use_julia(nthreads = 4)
```

To stop using threaded simulations, run:

``` r

use_julia(restart = TRUE)
```

## Specifying ranges

Instead of generating an ensemble with random initial conditions, we can
also specify ensembles with exact parameter values. For example, we
could vary the a_2 parameter, which determines how strongly having eaten
increases compensatory behaviour.

``` r

sims <- ensemble(sfm,
  n = 100,
  conditions = list(a2 = c(0.2, 0.4, 0.6, 0.8))
)
#> Starting ensemble simulation in "Julia" with 400 simulations in total.
#> ℹ 4 conditions x 100 simulations per condition.
#> ✔ Ensemble simulation completed in 10.2176 seconds.
```

``` r

plot(sims)
```

We can also vary multiple parameters at once. For example, we can vary
both a_2 and a_1, where the latter influences how strongly food intake
leads to more food intake. `n` now specifies the number of simulations
per condition. By default, `cross = TRUE`, which means that all possible
combinations of parameters are simulated.

``` r

sims <- ensemble(sfm,
  conditions = list(
    a2 = c(0.2, 0.8),
    a1 = c(1.3, 1.5)
  ),
  n = 100
)
#> Starting ensemble simulation in "Julia" with 400 simulations in total.
#> ℹ 4 conditions x 100 simulations per condition.
#> ✔ Ensemble simulation completed in 2.601 seconds.
```

``` r

plot(sims)
```

The plot shows similarity within columns but differences between
columns. As a_1 differs between columns, it appears that a_1 has a
larger effect than a_2. To view the parameter combination corresponding
to each condition, view `conditions` in `sims`:

``` r

sims$conditions
#>      condition  a1  a2
#> [1,]         1 1.3 0.2
#> [2,]         2 1.5 0.2
#> [3,]         3 1.3 0.8
#> [4,]         4 1.5 0.8
```

To generate a non-crossed designed, set `cross = FALSE`. In this case,
the length of each conditions vector needs to be the same.

``` r

sims <- ensemble(sfm,
  conditions = list(
    a2 = c(0.4, 0.5, 0.6),
    a1 = c(1.3, 1.4, 1.5)
  ),
  n = 100, cross = FALSE, save_sims = TRUE
)
#> Starting ensemble simulation in "Julia" with 300 simulations in total.
#> ℹ 3 conditions x 100 simulations per condition.
#> ✔ Ensemble simulation completed in 2.8885 seconds.
```

``` r

plot(sims, nrows = 1)
```

We can select specific conditions to compare, where here we plot the
first fifteen simulations of the first two conditions:

``` r

plot(sims, sim = 1:15, condition = 1:2, which = "sims", nrows = 1)
```

### Accessing simulation results

The results of the ensemble simulation are stored in the `sims` object,
which is a list containing, among others: - `summary`: summary
statistics across all simulations per condition - `df`: individual
simulation data (if `save_sims = TRUE`) - `init`: initial values of
stocks - `constants`: parameter values used - `conditions`: matrix
showing parameter combinations for each condition

You can access the summary statistics per condition and per time point,
such as the mean and confidence intervals, using:

``` r

head(sims)
#>   condition time               variable      mean    median   variance
#> 1         1    0 Compensatory_behaviour 0.4783079 0.4449562 0.08892955
#> 2         1    0            Food_intake 0.4766333 0.4552623 0.07613286
#> 3         1    0                 Hunger 0.4784890 0.4548582 0.10593726
#> 4         1    1 Compensatory_behaviour 0.6064398 0.5782980 0.03768898
#> 5         1    1            Food_intake 0.3499367 0.2950685 0.06815139
#> 6         1    1                 Hunger 0.4860790 0.4168530 0.07462279
#>   missing_count        q025      q975
#> 1             0 0.023064509 0.9746829
#> 2             0 0.016268546 0.9624419
#> 3             0 0.014191360 0.9650377
#> 4             0 0.323508122 0.9706544
#> 5             0 0.008076535 0.9006591
#> 6             0 0.122635157 0.9467490
# or
head(as.data.frame(sims))
#>   condition time               variable      mean    median   variance
#> 1         1    0 Compensatory_behaviour 0.4783079 0.4449562 0.08892955
#> 2         1    0            Food_intake 0.4766333 0.4552623 0.07613286
#> 3         1    0                 Hunger 0.4784890 0.4548582 0.10593726
#> 4         1    1 Compensatory_behaviour 0.6064398 0.5782980 0.03768898
#> 5         1    1            Food_intake 0.3499367 0.2950685 0.06815139
#> 6         1    1                 Hunger 0.4860790 0.4168530 0.07462279
#>   missing_count        q025      q975
#> 1             0 0.023064509 0.9746829
#> 2             0 0.016268546 0.9624419
#> 3             0 0.014191360 0.9650377
#> 4             0 0.323508122 0.9706544
#> 5             0 0.008076535 0.9006591
#> 6             0 0.122635157 0.9467490
```

By default, simulations are returned in long format, but can also be
shaped in wide format as well:

``` r

head(sims, direction = "wide")
#>   condition time mean.Compensatory_behaviour median.Compensatory_behaviour
#> 1         1    0                   0.4783079                     0.4449562
#> 2         1    1                   0.6064398                     0.5782980
#> 3         1    2                   0.6654096                     0.6654575
#> 4         1    3                   0.6878700                     0.7044476
#> 5         1    4                   0.6870671                     0.7128944
#> 6         1    5                   0.6699829                     0.6943745
#>   variance.Compensatory_behaviour missing_count.Compensatory_behaviour
#> 1                      0.08892955                                    0
#> 2                      0.03768898                                    0
#> 3                      0.02426997                                    0
#> 4                      0.02030700                                    0
#> 5                      0.01884024                                    0
#> 6                      0.01758314                                    0
#>   q025.Compensatory_behaviour q975.Compensatory_behaviour mean.Food_intake
#> 1                  0.02306451                   0.9746829       0.47663330
#> 2                  0.32350812                   0.9706544       0.34993669
#> 3                  0.41107518                   0.9594017       0.22519740
#> 4                  0.44038959                   0.9406263       0.13572832
#> 5                  0.45185705                   0.9242090       0.08344722
#> 6                  0.44313084                   0.9038378       0.05127490
#>   median.Food_intake variance.Food_intake missing_count.Food_intake
#> 1        0.455262347           0.07613286                         0
#> 2        0.295068492           0.06815139                         0
#> 3        0.165096699           0.05124440                         0
#> 4        0.064093203           0.03508451                         0
#> 5        0.024972732           0.02343164                         0
#> 6        0.009849555           0.01085386                         0
#>   q025.Food_intake q975.Food_intake mean.Hunger median.Hunger variance.Hunger
#> 1     1.626855e-02        0.9624419   0.4784890     0.4548582      0.10593726
#> 2     8.076535e-03        0.9006591   0.4860790     0.4168530      0.07462279
#> 3     3.105759e-03        0.8200163   0.5270163     0.4672373      0.05153272
#> 4     7.936879e-04        0.7273670   0.5865395     0.5536180      0.03599362
#> 5     2.082555e-04        0.6395364   0.6498461     0.6280648      0.02551272
#> 6     6.398903e-05        0.4426647   0.7079553     0.6899168      0.01823153
#>   missing_count.Hunger q025.Hunger q975.Hunger
#> 1                    0  0.01419136   0.9650377
#> 2                    0  0.12263516   0.9467490
#> 3                    0  0.21685901   0.9447705
#> 4                    0  0.28533754   0.9440976
#> 5                    0  0.39115944   0.9538292
#> 6                    0  0.44863920   0.9608182
```

If you have set `save_sims = TRUE`, you can access the individual
simulation runs as well. The dataframe contains the value of each
variable, for each time point, for each simulation, for each condition.

``` r

head(sims, which = "sims", direction = "long")
#>   condition sim time               variable      value
#> 1         1   1    0 Compensatory_behaviour 0.09211427
#> 2         1   1    0            Food_intake 0.69842600
#> 3         1   1    0                 Hunger 0.49628470
#> 4         1   1    1 Compensatory_behaviour 0.40993234
#> 5         1   1    1            Food_intake 0.63479705
#> 6         1   1    1                 Hunger 0.34165639
```

Finally, to access the parameters (i.e., constants) of each simulation
per condition, run:

``` r

head(sims, which = "sims", type = "constant")
#>   condition sim time               variable      value
#> 1         1   1    0 Compensatory_behaviour 0.09211427
#> 2         1   1    0            Food_intake 0.69842600
#> 3         1   1    0                 Hunger 0.49628470
#> 4         1   1    1 Compensatory_behaviour 0.40993234
#> 5         1   1    1            Food_intake 0.63479705
#> 6         1   1    1                 Hunger 0.34165639
```

To view their summary statistics, run:

``` r

head(sims, which = "summary", type = "constant")
#>   condition time               variable      mean    median   variance
#> 1         1    0 Compensatory_behaviour 0.4783079 0.4449562 0.08892955
#> 2         1    0            Food_intake 0.4766333 0.4552623 0.07613286
#> 3         1    0                 Hunger 0.4784890 0.4548582 0.10593726
#> 4         1    1 Compensatory_behaviour 0.6064398 0.5782980 0.03768898
#> 5         1    1            Food_intake 0.3499367 0.2950685 0.06815139
#> 6         1    1                 Hunger 0.4860790 0.4168530 0.07462279
#>   missing_count        q025      q975
#> 1             0 0.023064509 0.9746829
#> 2             0 0.016268546 0.9624419
#> 3             0 0.014191360 0.9650377
#> 4             0 0.323508122 0.9706544
#> 5             0 0.008076535 0.9006591
#> 6             0 0.122635157 0.9467490
```

### Close Julia session

``` r

use_julia(stop = TRUE)
#> ✔ Closed Julia session.
```
