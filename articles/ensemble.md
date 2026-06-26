# Ensemble simulations

``` r

library(sdbuildR)

# Disable WebGL: many plotly widgets per HTML page can exceed the browser WebGL
# context limit and render blank. SVG always renders.
options(sdbuildR.webgl = FALSE)
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

sfm <- stockflow("Crielaard2022")
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

sfm |> simulate() |> plot()
```

As the model has random initial conditions, another run will be
different:

``` r

sfm |> simulate() |> plot()
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
#> ✔ Ensemble simulation completed in 22.9845 seconds.
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
#> ✔ Ensemble simulation completed in 6.936 seconds.
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
#> ✔ Ensemble simulation completed in 79.9799 seconds.
```

``` r

plot(sims)
```

### Choosing which summaries to keep

Storing every simulation is expensive, so by default
[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
keeps only summaries statistics across runs at each time point: the mean
and a 95% interval (the 2.5% and 97.5% quantiles). The quantile levels
can be changed as follows:

``` r

sims <- ensemble(sfm,
  n = 100,   
  quantiles = c(0.1, 0.9)
)
#> Starting ensemble simulation in "R" with 100 simulations.
#> ✔ Ensemble simulation completed in 22.9887 seconds.
head(sims)
#>   condition               variable time      mean    median missing_count
#> 1         1 Compensatory_behaviour    0 0.4750875 0.4036320             0
#> 2         1 Compensatory_behaviour    1 0.5978226 0.5469748             0
#> 3         1 Compensatory_behaviour    2 0.6594824 0.6397814             0
#> 4         1 Compensatory_behaviour    3 0.6868372 0.6901910             0
#> 5         1 Compensatory_behaviour    4 0.6906507 0.7017107             0
#> 6         1 Compensatory_behaviour    5 0.6775006 0.6806661             0
#>       quant1    quant2
#> 1 0.09538729 0.8620636
#> 2 0.38936237 0.8665326
#> 3 0.49574861 0.8745699
#> 4 0.52067662 0.8726997
#> 5 0.52001627 0.8472388
#> 6 0.51999692 0.8454621
```

Here, quantiles appear as columns `quant1`, `quant2` which correspond to
`sims$quantiles`.

To change which summary statistics are computed, use `central` (defining
the central tendency) and `spread` (defining the measure of dispersion).
For example, to summarise each run by its median together with a
standard deviation:

``` r

sims <- ensemble(sfm,
  n = 100,
  central = "median",
  spread = "sd"
)
#> Starting ensemble simulation in "R" with 100 simulations.
#> ✔ Ensemble simulation completed in 23.0713 seconds.
head(sims)
#>   condition               variable time    median        sd missing_count
#> 1         1 Compensatory_behaviour    0 0.4036320 0.2949805             0
#> 2         1 Compensatory_behaviour    1 0.5469748 0.1882976             0
#> 3         1 Compensatory_behaviour    2 0.6397814 0.1430877             0
#> 4         1 Compensatory_behaviour    3 0.6901910 0.1245730             0
#> 5         1 Compensatory_behaviour    4 0.7017107 0.1174524             0
#> 6         1 Compensatory_behaviour    5 0.6806661 0.1143088             0
```

``` r

plot(sims)
```

All available summary statistics can be computed with:

``` r

sims <- ensemble(sfm,
  n = 100,
  central = c("mean", "median"),
  spread = c("quantile", "sd", "range")
)
#> Starting ensemble simulation in "R" with 100 simulations.
#> ✔ Ensemble simulation completed in 22.9867 seconds.
head(sims)
#>   condition               variable time      mean    median        sd
#> 1         1 Compensatory_behaviour    0 0.4750875 0.4036320 0.2949805
#> 2         1 Compensatory_behaviour    1 0.5978226 0.5469748 0.1882976
#> 3         1 Compensatory_behaviour    2 0.6594824 0.6397814 0.1430877
#> 4         1 Compensatory_behaviour    3 0.6868372 0.6901910 0.1245730
#> 5         1 Compensatory_behaviour    4 0.6906507 0.7017107 0.1174524
#> 6         1 Compensatory_behaviour    5 0.6775006 0.6806661 0.1143088
#>           min       max missing_count     quant1    quant2
#> 1 0.008115848 0.9553296             0 0.01931528 0.9259952
#> 2 0.309331064 0.9657745             0 0.34270610 0.9190606
#> 3 0.364860898 0.9694763             0 0.41868692 0.9192805
#> 4 0.383161078 0.9670575             0 0.46006368 0.9187255
#> 5 0.391525379 0.9581517             0 0.47481319 0.9183256
#> 6 0.395668622 0.9403187             0 0.45766539 0.9043367
```

Plots can then switch between summary statistics:

``` r

plot(sims, central = "median", spread = "quantile")
```

``` r

plot(sims, central = "median", spread = "range")
```

``` r

plot(sims, central = "none", spread = "range")
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

The `workers` argument specifies how many parallel processes to use;
adjust this based on your system’s capabilities. On Windows,
`multisession` is the recommended backend. On POSIX systems (Linux,
macOS), you can also use `multicore` for potentially better performance,
though `multisession` will work on those systems as well.

#### Ensemble simulations (Julia)

Ensemble simulations can also be run in Julia, which is typically faster
than R for large simulations. To use the Julia backend, first follow the
instructions in the [Julia setup
vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html)
to install and set up the Julia environment for sdbuildR.

Activate the Julia environment for sdbuildR:

``` r

use_julia()
#> ℹ Activating Julia environment for sdbuildR at
#>   /home/runner/.local/share/R/sdbuildR/julia...
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
#> ✔ Ensemble simulation completed in 11.4236 seconds.
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
#> ✔ Ensemble simulation completed in 2.6059 seconds.
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
#> ✔ Ensemble simulation completed in 2.9685 seconds.
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
#>   condition time               variable      mean    median missing_count
#> 1         1    0 Compensatory_behaviour 0.4783079 0.4449562             0
#> 2         1    0            Food_intake 0.4766333 0.4552623             0
#> 3         1    0                 Hunger 0.4784890 0.4548582             0
#> 4         1    1 Compensatory_behaviour 0.6064398 0.5782980             0
#> 5         1    1            Food_intake 0.3499367 0.2950685             0
#> 6         1    1                 Hunger 0.4860790 0.4168530             0
#>        quant1    quant2
#> 1 0.023064509 0.9746829
#> 2 0.016268546 0.9624419
#> 3 0.014191360 0.9650377
#> 4 0.323508122 0.9706544
#> 5 0.008076535 0.9006591
#> 6 0.122635157 0.9467490
# or
sims |> as.data.frame() |> head()
#>   condition time               variable      mean    median missing_count
#> 1         1    0 Compensatory_behaviour 0.4783079 0.4449562             0
#> 2         1    0            Food_intake 0.4766333 0.4552623             0
#> 3         1    0                 Hunger 0.4784890 0.4548582             0
#> 4         1    1 Compensatory_behaviour 0.6064398 0.5782980             0
#> 5         1    1            Food_intake 0.3499367 0.2950685             0
#> 6         1    1                 Hunger 0.4860790 0.4168530             0
#>        quant1    quant2
#> 1 0.023064509 0.9746829
#> 2 0.016268546 0.9624419
#> 3 0.014191360 0.9650377
#> 4 0.323508122 0.9706544
#> 5 0.008076535 0.9006591
#> 6 0.122635157 0.9467490
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
#>   missing_count.Compensatory_behaviour quant1.Compensatory_behaviour
#> 1                                    0                    0.02306451
#> 2                                    0                    0.32350812
#> 3                                    0                    0.41107518
#> 4                                    0                    0.44038959
#> 5                                    0                    0.45185705
#> 6                                    0                    0.44313084
#>   quant2.Compensatory_behaviour mean.Food_intake median.Food_intake
#> 1                     0.9746829       0.47663330        0.455262347
#> 2                     0.9706544       0.34993669        0.295068492
#> 3                     0.9594017       0.22519740        0.165096699
#> 4                     0.9406263       0.13572832        0.064093203
#> 5                     0.9242090       0.08344722        0.024972732
#> 6                     0.9038378       0.05127490        0.009849555
#>   missing_count.Food_intake quant1.Food_intake quant2.Food_intake mean.Hunger
#> 1                         0       1.626855e-02          0.9624419   0.4784890
#> 2                         0       8.076535e-03          0.9006591   0.4860790
#> 3                         0       3.105759e-03          0.8200163   0.5270163
#> 4                         0       7.936879e-04          0.7273670   0.5865395
#> 5                         0       2.082555e-04          0.6395364   0.6498461
#> 6                         0       6.398903e-05          0.4426647   0.7079553
#>   median.Hunger missing_count.Hunger quant1.Hunger quant2.Hunger
#> 1     0.4548582                    0    0.01419136     0.9650377
#> 2     0.4168530                    0    0.12263516     0.9467490
#> 3     0.4672373                    0    0.21685901     0.9447705
#> 4     0.5536180                    0    0.28533754     0.9440976
#> 5     0.6280648                    0    0.39115944     0.9538292
#> 6     0.6899168                    0    0.44863920     0.9608182
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
#> [1] condition sim       time      variable  value    
#> <0 rows> (or 0-length row.names)
```

To view their summary statistics, run:

``` r

head(sims, which = "summary", type = "constant")
#> [1] condition     time          variable      mean          median       
#> [6] missing_count quant1        quant2       
#> <0 rows> (or 0-length row.names)
```

## Close Julia session

``` r

use_julia(stop = TRUE)
#> ✔ Closed Julia session.
```
