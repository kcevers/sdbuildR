# Building stock-and-flow models

``` r

library(sdbuildR)
```

Stock-and-flow models represent systems as states (stocks) that
accumulate over time with processes (flows) that change these variables.
In this vignette, we will demonstrate how to create stock-and-flow
models from scratch using sdbuildR. It covers the basics of
stock-and-flow modelling in the context of psychology with an example of
burnout. Note that this vignette serves as online supplemental material
A accompanying the paper *Formalizing Psychological Theory with
sdbuildR: A Stock-and-Flow Modelling Tutorial in R* by Evers et
al. (under review). To reproduce the figures in the paper, please see
the corresponding .Rmd file.

## Stock-and-flow models

Stock-and-flow models conceptualize systems in terms of quantities that
accumulate (i.e., stocks) and the processes (i.e., flows) that change
them over time. Stocks are like the amount of water in a bathtub: they
store the effects of past and present flows. Stocks must be able to
increase and decrease, and should be measurable at a single moment in
time. Inflows – water from the tap – raise the stock, while outflows –
water through the drain – lower it. As such, flows represent the rates
at which stocks change, measured in units per time (e.g., litre per
minute). The net rate of change in the water level is determined by the
difference between the inflows and outflows. In this way, a stock
functions as a memory of past activity: it increases when inflows exceed
outflows and decreases when outflows outpace inflows. Without an
outflow, the water remains in the bathtub; without an inflow, the
bathtub stays empty. This structure is the foundation of stock-and-flow
models, where stocks represent the state of a system, and flows
represent the processes that alter that state over time.

Stock-and-flow models provide an intuitive way to formalize
psychological theories as many are fundamentally concerned with change
over time. Despite the physical connotation of the term, stocks need not
be tangible: emotions, knowledge, beliefs, perceptions, stress,
motivation, and trust are all examples of psychological constructs that
accumulate over time, often in response to experience or behaviour. The
processes that drive these changes – such as emotion regulation, coping,
and learning – are the flows, specifying what causes psychological
states to increase or decrease.

Stock-and-flow models are easiest to understand through a worked
example. We will create a simplified model of burnout. Before building
it from scratch, we load it from the model library:

``` r

sfm <- stockflow("burnout", version = 1)
print(sfm)
#> 
#> ── Stock-and-Flow Model: Burnout ───────────────────────────────────────────────
#> 1 stock • 2 flows • 2 constants • 1 auxiliary
#> 
#> ── Stock-Flow Structure ──
#> 
#> energy: + recovery - depletion
#> 
#> ── Other Variables ──
#> 
#> Constants: `depletion_rate` and `recovery_rate`
#> Auxiliaries: `net_flow`
#> 
#> ── Simulation Settings ──
#> 
#> Time: 0.0 to 183.0 days (dt = 0.01) • euler • R
#> Simulation output: all variables
```

`sfm` is a stock-and-flow model object, containing a single stock
representing energy, an inflow for recovery, and an outflow for energy
depletion. In addition, the model contains two other variable types:
constants and auxiliaries. Throughout the tutorial, we use the term
“variable” for any part of the system, be that a stock, flow, constant,
or auxiliary. Though this usage may differ from other scientific fields,
we here choose to adhere to system dynamics terminology (Ford 2019;
Sterman 2000). Constants are static parameters that do not change over
the time course of the simulation. In contrast, auxiliaries are dynamic,
meaning they are computed anew at each step. They are intermediate
variables used in flow equations or to monitor other dynamic quantities.
To illustrate the difference, a constant defined as `runif(1)` will be
fixed to a random number at the beginning of the simulation, whereas an
equivalently defined auxiliary will draw a new number each time step.
Lastly, the object contains simulation settings such as the total
duration, the timestep (`dt`) specifying the temporal resolution of the
simulation, and a solver (`euler`) indicating the numerical technique
used to generate output from the model (for more details, see Karline
Soetaert et al. 2010). All variables are saved in the simulation output,
which can be reduced to saving only stocks or specific variables for
computational efficiency.

Plotting the model shows its stock-and-flow diagram:

``` r

plot(sfm, show_constants = TRUE)
```

To assess the model’s dynamics, we simulate it over time and visualise
the resulting timeseries:

``` r

sfm |> simulate() |> plot()
```

Above, we use the pipe operator `|>` to pass the result of an expression
to the next expression as its first argument. As shown above, energy
increases over time but then stabilizes at a fixed level when the
outflow of energy depletion meets the constant inflow of recovery.

We now build this same model from scratch in iterative steps. The table
below provides an overview of each model revision and the behaviour it
produces.

| Panel | Stocks | Constants | Recovery eqn (inflow) | Depletion eqn (outflow) | Interpretation | Behaviour |
|:---|:---|:---|:---|:---|:---|:---|
| A | energy |  |  |  | No process of change | Static |
| B | energy | `depletion_rate` |  | `depletion_rate` | Energy decreases at a constant rate | Linear decrease |
| C | energy | `depletion_rate` |  | `depletion_rate * energy` | Energy decreases at a rate proportional to its current value | Exponential decrease towards zero |
| D | energy | `depletion_rate`, `recovery_rate` | `recovery_rate` | `depletion_rate * energy` | Energy changes at a rate equal to a constant minus a rate proportional to its current value | Stability when recovery and depletion are equal |
| E | energy, recovery_rate | `depletion_rate` | `recovery_rate` | `depletion_rate * energy` | Energy recovers at a rate which itself changes over time | Rise and collapse |

Connecting Equations to Model Behaviour {.table .table
style="margin-left: auto; margin-right: auto;"}

``` r

sfm <- stockflow()
print(sfm)
#> 
#> ── Stock-and-Flow Model ────────────────────────────────────────────────────────
#> ℹ Empty model without any variables.
#> 
#> ── Simulation Settings ──
#> 
#> Time: 0 to 100 seconds (dt = 0.01) • euler • R
#> Simulation output: stocks only
```

We update the simulation settings to model energy over the course of
half a year (i.e., specified in days; note that the time unit merely
changes the labels on the axes of the resulting plots, and does not
affect the model’s behaviour). Additionally, we set
`only_stocks = FALSE` to return all variables in the simulation output,
not just the stocks:

``` r

sfm <- sim_settings(sfm, stop = round(365/2), time_units = "days",
 only_stocks = FALSE
 )
```

A model name can be supplied with
[`meta()`](https://kcevers.github.io/sdbuildR/reference/meta.md).

``` r

sfm <- meta(sfm, name = "Burnout")
```

Next, we introduce a stock to the model to represent energy. Each
variable requires a `name` such as `energy`, which serves as its
identifier in equations. Each name should be unique and adhere to the
same naming rules as R variables (e.g., no spaces or special
characters). An optional `label` can be supplied for use in plots and
diagrams (e.g., `label = "Energy Level"`); when omitted, the name is
used.

Every stock also needs an *initial condition*: the value of the stock at
the start of the simulation. This is set via the `eqn` argument, where
here, we initialize energy at .3:

``` r

sfm <- stock(sfm, name = energy, eqn = .3, label = "Energy")
```

Plotting the stock-and-flow model yields its stock-and-flow diagram,
which now consists of only one stock:

``` r

plot(sfm)
```

To assess its dynamics, we simulate the model over time and visualise
the resulting timeseries:

``` r

sfm |>
  simulate() |>
  plot()
```

Across the entirety of the simulation, energy remains at its initial
state. Stocks without flows are static, as there is no process
specifying how they change. To deplete energy, we introduce an outflow
representing energy depletion. For simplicity, we specify that depletion
occurs at a constant rate over time, such as `.05`. Rather than defining
the flow’s `eqn` to be `.05` directly, we add a constant to the model,
so that it can easily be changed later. This also helps to keep track of
how parametrized the model is.

``` r

sfm <- constant(sfm, depletion_rate, eqn = .05, label = "Depletion Rate")
```

`eqn` is a generic argument used for all variable types, denoting the
initial condition for a stock, a static value for a constant, and an
equation that is recomputed at each time step in the simulation for
flows and auxiliaries. `eqn` accepts any valid R expression that
evaluates to a scalar, including functions (e.g.,
[`sqrt()`](https://rdrr.io/r/base/MathFun.html),
[`min()`](https://rdrr.io/r/base/Extremes.html), `runif(1)`) and
arithmetic operators (e.g., `*`, `+`). `eqn` can reference other
variables defined in the model.

`depletion_rate` can now be used as a variable in the equation for the
outflow from energy:

``` r

sfm <- flow(sfm, depletion,
  eqn = depletion_rate, from = energy,
  label = "Depletion"
)
```

Aside from requiring a `name` and `eqn`, flows further need to be
connected to a stock, at least as either an inflow (`to`) or an outflow
(`from`). Variable properties can be modified with
[`update()`](https://rdrr.io/r/stats/update.html). Note that by
definition, outflows are subtracted from the stock, and as such do not
need a minus sign in `eqn` to indicate that they decrease the stock. We
simulate the model to check whether energy indeed depletes:

``` r

sfm |>
  simulate() |>
  plot()
```

As a stock with a constant outflow decreases linearly, energy becomes
negative. To rectify this implausible behaviour, a naive solution may be
to include a logical statement such as `ifelse(energy < 0, 0, energy)`.
However, this computational trick would mask model misspecification.
Ideally, stocks should remain within bounds due to plausible equations
and parameters. For instance, we can prevent negative energy by making
`depletion` proportional to the amount of available energy:
`depletion_rate * energy`. In this way, when `energy` is zero, the
outflow is also zero.

To assess whether this produces more plausible model behaviour, we
modify the outflow using
[`update()`](https://rdrr.io/r/stats/update.html):

``` r

sfm <- update(sfm, depletion, eqn = depletion_rate * energy)

sfm |>
  simulate() |>
  plot()
```

Energy now follows an exponential decay pattern, where depletion now
depletes energy until it is zero, but not beyond this point. In other
words, we have introduced a *feedback loop* to the system (Meadows
2008). Positive feedback loops amplify change, whereas negative feedback
loops bring the system back to a target state (Sterman 2000). In our
model, energy and depletion form a negative feedback loop that pulls
energy to zero: the higher energy is, the more its outflow decreases it,
until it reaches the implicit target state of zero.

To allow energy to recover, we introduce an inflow, again specified as a
simple constant rate:

``` r

sfm <- constant(sfm, recovery_rate, eqn = .3, label = "Recovery Rate") |>
  flow(recovery, eqn = recovery_rate, to = energy, label = "Recovery")

sfm |>
  simulate() |>
  plot()
```

We finally add an auxiliary to keep track of net flow to energy
(inflow - outflow):

``` r

sfm <- aux(sfm, net_flow, eqn = recovery - depletion, label = "Net flow to energy")

sfm |>
  simulate() |>
  plot()
```

``` r


sfm |>
  simulate() |>
  plot(vars = c("depletion", "recovery", "net_flow"))
```

As a result of the new inflow, energy now stabilizes at a fixed level,
as energy recovery and depletion balance out.

Though the model no longer produces a negative energy state, our goal
was to produce a burnout pattern. We thus need to revise the model. What
if the ability to recover is not static, but erodes over time? Put
differently, what if the recovery rate is not a constant, but a stock?
To implement this idea, we change the type of `recovery_rate`:

``` r

sfm <- change_type(sfm, recovery_rate, new_type = stock)
```

We then add a new outflow that depletes the recovery rate in proportion
to the amount worked:

``` r

sfm <- flow(sfm, erosion,
  eqn = recovery_rate * depletion,
  from = recovery_rate, label = "Recovery Erosion"
)
sfm |>
  simulate() |>
  plot()
```

The plot shows how the erosion of the ability to recover produces a
characteristic burnout pattern: a steep initial rise followed by a
collapse of energy.

The net flow to energy is initially positive, as recovery exceeds energy
depletion from depletion. As erosion progressively reduces the recovery
rate, this inflow weakens, eventually falling below the outflow from
depletion. The net flow to energy is negative, and energy begins to
decline, leading to the observed collapse. This complex behaviour is
produced by a simple stock-and-flow model consisting of two stocks and
three flows:

``` r

plot(sfm)
```

Note that this is identical to the version stored in the model library,
which can be loaded using
[`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md):

``` r

sfm <- stockflow("burnout", version = 2)
```

In summary, stock-and-flow models consist of one or more stocks, each
requiring an inflow and/or outflow to change over time. Without limiting
processes, stocks may continue to increase indefinitely; without
restorative processes, stocks may deplete past the point of recovery. In
principle, inflows and outflows connected to the same stock could be
combined into a single net flow (as often done in differential equation
models). However, separating inflows and outflows encourages more
precise thinking about what processes increase and decrease stocks, and
what distinct information and rates of change govern each flow (Sterman
2000, 547). Disaggregation further reframes interventions by for example
identifying whether to limit inflows or promote outflows (Levine 1993;
Meadows 2008).

## Variable types

| Characteristic | Stock | Flow | Constant | Auxiliary |
|:---|:---|:---|:---|:---|
| Role in system | Defines the state of the system; accumulates the effects of flow(s) over time | Increases or decreases a stock | Specifies static quantity | Provides intermediate computations for convenience; keeps track of changing quantities |
| Varies within time horizon | ✓ | ✓ | ✗ | ✓ |
| A process taking place over time | ✗ | ✓ | ✗ | Possibly |
| Can be captured at any given moment in time | ✓ | ✗ | ✓ | Possibly |
| `eqn` denotes | Initial condition | Flow rate computed at every time step | Fixed value | Value computed at every time step |
| Allowed dependencies in `eqn` | Constants and (initial values of) other stocks | Any other variable | Other constants and (initial values of) stocks | Any other variable |
| Examples | Emotions, beliefs, stress, trust, resources | Coping, learning, emotion regulation | Rates, capacities, thresholds | Performance indices, ratios, sums of stocks |

Characteristics of Variable Types in Stock-and-Flow Models {.table
.table style="margin-left: auto; margin-right: auto;"}

The following flowchart can be used to determine a variable’s type:

## Overview of package functionality

| Function | Purpose |
|:---|:---|
| [`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md) | Create empty model or load template |
| [`stock()`](https://kcevers.github.io/sdbuildR/reference/stock.md) | Add or modify a stock |
| [`flow()`](https://kcevers.github.io/sdbuildR/reference/flow.md) | Add or modify a flow |
| [`constant()`](https://kcevers.github.io/sdbuildR/reference/constant.md) | Add or modify a constant |
| [`aux()`](https://kcevers.github.io/sdbuildR/reference/auxiliary.md) | Add or modify an auxiliary |
| [`lookup()`](https://kcevers.github.io/sdbuildR/reference/lookup.md) | Add or modify a lookup function |
| [`update()`](https://rdrr.io/r/stats/update.html) | Add or modify any variable (generic) |
| [`simulate()`](https://rdrr.io/r/stats/simulate.html) | Simulate model |
| [`plot()`](https://rdrr.io/r/graphics/plot.default.html) | Plot model diagram or simulation |
| [`summary()`](https://rdrr.io/r/base/summary.html) | Run model diagnostics |
| [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) | Get model properties in a dataframe |
| [`sim_settings()`](https://kcevers.github.io/sdbuildR/reference/sim_settings.md) | Modify simulation specifications |
| [`meta()`](https://kcevers.github.io/sdbuildR/reference/meta.md) | Modify model metadata |
| [`export_model()`](https://kcevers.github.io/sdbuildR/reference/export_model.md) | Export models to other formats |

Main functions in *sdbuildR* {.table .table
style="margin-left: auto; margin-right: auto;"}

In some cases, it may be useful to refer to global simulation variables
in the model’s equations:

| Variable | Description | Use case |
|:---|:---|:---|
| `times` | Vector with simulation times | Use in equations, e.g., `pulse(times, 5, width = dt)` |
| `t` | Current time in the ODE | Use in equations, e.g., `input(t)` |
| `dt` | Time step of the simulation | Use in equations, e.g., `pulse(times, 5, width = dt)` |

Global simulation variables in *sdbuildR* {.table .table
style="margin-left: auto; margin-right: auto;"}

### Simulation specifications

We may want to observe the system over a longer time period, or with a
different time step.

``` r

sfm <- sfm |>
  sim_settings(
    start = 0,
    stop = 250,
    dt = 0.001
  )
```

Simulation settings can be set directly on the model object as above, or
passed to [`simulate()`](https://rdrr.io/r/stats/simulate.html):

``` r

sim <- simulate(sfm, start = 0, stop = 250)
```

`dt` refers to the time step of the simulation, which determines how
often the model’s equations are evaluated. A smaller `dt` can increase
the accuracy of the simulation, but also increases computational time
and the size of the resulting dataframe. To reduce the saved output, we
may save fewer timepoints, for instance, every 0.1 days:

``` r

sfm <- sim_settings(sfm, save_at = 0.1)
```

Or specific time points:

``` r

sfm <- sim_settings(sfm, save_at = c(1, 50, 100))
```

Alternatively, we can specify the number of time points to save with
`save_n`:

``` r

sfm <- sim_settings(sfm, save_n = 100)
```

Similarly, we may change the numerical method used to solve the model.
The default method is `"euler"`, which is the simplest numerical
integration method. For more complex models or when higher accuracy is
needed, consider other methods like `"rk4"`:

``` r

sfm <- sim_settings(sfm, method = "rk4")
```

All available simulation methods can found with:

``` r

sim_methods()
#> $R
#>  [1] "euler"      "rk2"        "rk4"        "rk23bs"     "ode23"     
#>  [6] "rk45dp6"    "rk45dp7"    "rk45e"      "rk45f"      "rk45ck"    
#> [11] "rk78dp"     "rk78f"      "ode45"      "irk3r"      "irk5r"     
#> [16] "irk4hh"     "irk4l"      "irk6kb"     "irk6l"      "lsoda"     
#> [21] "lsodar"     "lsode"      "lsodes"     "bdf"        "bdf_d"     
#> [26] "vode"       "daspk"      "adams"      "impAdams"   "impAdams_d"
#> [31] "radau"     
#> 
#> $Julia
#>  [1] "Euler()"        "ForwardEuler()" "Midpoint()"     "Heun()"        
#>  [5] "RK4()"          "BS3()"          "Tsit5()"        "Vern6()"       
#>  [9] "Vern7()"        "Vern8()"        "Vern9()"        "Rosenbrock23()"
```

Note that some methods may not be available in Julia and vice versa.

In case the simulation contains stochastic elements, we may want to set
a seed to ensure that the simulation is reproducible. For example, the
initial value of energy could be a random number:

``` r

sfm <- stock(sfm, energy, eqn = runif(1, 0, 1))
```

The seed needs to be an integer:

``` r

sfm <- sim_settings(sfm, seed = 123)
```

The seed can also be removed to ensure variation in the simulation. This
can be useful to for example test the sensitivity of the model to
initial condition variation.

``` r

sfm <- sim_settings(sfm, seed = NULL)
```

### Renaming variables

Variable names can easily be changed:

``` r

sfm <- change_name(sfm, recovery_rate, new_name = recovery_store)
```

This will ensure that all references to `recovery_rate` are changed to
`recovery_store`.

### Allowed variable names

When creating variables or changing variable names, a warning may be
issued that the name was modified to be syntactically valid and unique.
For example:

``` r

sfm <- change_name(sfm, recovery_store, new_name = t)
#> Warning: A name was changed for syntactic validity or uniqueness.
#> ℹ "t" → `t_1`
```

The name `t` is not usable, as this already refers to the current time
step. Similarly, names cannot contain spaces or special characters:

``` r

sfm <- change_name(sfm, t_1, new_name = recovery - rate)
#> Warning: A name was changed for syntactic validity or uniqueness.
#> ℹ "recovery - rate" → `recovery___rate`
```

Names also cannot be duplicated:

``` r

sfm <- change_name(sfm, energy, new_name = recovery)
#> Warning: A name was changed for syntactic validity or uniqueness.
#> ℹ "recovery" → `recovery_1`
```

### Removing variables

To remove a variable from the model, use
[`discard()`](https://kcevers.github.io/sdbuildR/reference/discard.md):

``` r

sfm <- discard(sfm, net_flow)
```

Note that this cannot be undone!

### Lookup functions

Lookup functions (`lookup`), also known as table or graphical functions,
are interpolation functions used to create custom input-output
functions, where we define the desired output (y) for a specified input
(x). They are defined by a set of x- and y-domain points. The
interpolation method defines the behaviour of the lookup function
between x-points, and the extrapolation method defines the behaviour
outside of the x-points. For example, a simple lookup function called
`"graph"` may look like this:

``` r

sfm <- stockflow() |>
  lookup(graph,
    xpts = c(0, 1, 2), ypts = c(0.5, 1, 1),
    interpolation = "linear", extrapolation = "nearest"
  )
```

The function can now be used in any equation in the model like so:

``` r

sfm <- constant(sfm, x, eqn = graph(1))
```

### Custom functions

New functions can be defined such that they can be used anywhere in the
model. For example, if the
[`logistic()`](https://kcevers.github.io/sdbuildR/reference/logistic.md)
function did not exist, you could create it yourself:

``` r

sfm <- stockflow() |>
  custom_func(f, eqn = function(x, slope = 1, midpoint = .5) 1 / (1 + exp(-slope * (x - midpoint))))
```

This will create a function `f()` that can be used in any equation in
the model like so:

``` r

sfm <- constant(sfm, x, eqn = f(0))
```

### Documenting

To document meta-properties of the model, use
[`meta()`](https://kcevers.github.io/sdbuildR/reference/meta.md). For
example, the model’s `name`, subtitle (`caption`), or `author`.
[`meta()`](https://kcevers.github.io/sdbuildR/reference/meta.md) accepts
any key-value pair, so custom metadata can also be added.

``` r

sfm <- meta(sfm, author = "Kyra Evers", affiliation = "University of Amsterdam")
```

Ford, David N. 2019. “A System Dynamics Glossary.” *System Dynamics
Review* 35 (4): 369–79. <https://doi.org/10.1002/sdr.1641>.

Karline Soetaert, Thomas Petzoldt, and R. Woodrow Setzer. 2010. “Solving
Differential Equations in R: Package deSolve.” *Journal of Statistical
Software* 33 (9): 1–25. <https://doi.org/10.18637/jss.v033.i09>.

Levine, Ralph L. 1993. “System Dynamics Applied To Psychological and
Social Problems.” *Proceedings of the 18th International Conference of
the System Dynamics Society* (Bergen, Norway).

Meadows, Donella H. 2008. *Thinking in Systems: A Primer*. Chelsea Green
Publishing.

Sterman, John D. 2000. *Business dynamics: systems thinking and modeling
for a complex world*. Irwin/McGraw-Hill.
