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
stockflow: A Stock-and-Flow Modelling Tutorial in R* by Evers et
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

To demonstrate stock-and-flow modelling in a psychological context, we
will create a simplified model of burnout. The model represents a
stylized pattern of how burnout develops over time on a within-person
level. It contains a single stock representing the current level of
energy, an inflow for recovery, and an outflow for depletion from work.
We first initialize an empty stock-and-flow model:

``` r

sfm <- sdbuildR()
print(sfm)
#> 
#> ── Stock-and-Flow Model ────────────────────────────────────────────────────────
#> ℹ Empty model without any variables.
#> 
#> ── Simulation Settings ──
#> 
#> Time: 0 to 100 seconds (dt = 0.01) • euler • R
```

Though the model contains no elements, it includes default simulation
settings, such as the duration of the simulation and a solver (`euler`)
specifying the numerical technique used to generate output from the
model. We change the simulation settings to model energy over the course
of 16 weeks (note that the time unit merely changes the labels on the
axes of the resulting plots, and does not affect the model’s behaviour).
In addition, we set `only_stocks = FALSE` to return all model variables
in the simulation output, not just the stocks:

``` r

sfm <- sim_settings(sfm, stop = 16, time_units = "weeks", only_stocks = FALSE)
```

A model name can be supplied with
[`meta()`](https://kcevers.github.io/sdbuildR/reference/meta.md).

``` r

sfm <- meta(sfm, name = "Burnout")
```

Next, we introduce a stock to the model to represent energy. Each model
element requires a unique `name`, which serves as its identifier in
equations and follows the same naming rules as R variables (e.g., no
spaces or special characters). We here simply choose `energy`. An
optional `label` can be supplied for use in plots and diagrams (e.g.,
`label = "Energy Level"`); when omitted, the name is used.

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

simulate(sfm) |> plot()
```

Above, we use the pipe operator `|>` for better legibility. It simply
passes the result of an expression to the next expression as its first
argument. Across the entirety of the simulation, energy remains at its
initial state. Stocks without flows are static, as there is no process
specifying how they change. To deplete energy, we introduce an outflow
representing work. For simplicity, we specify that work occurs at a
constant rate over time, say `.5`. Rather than defining the flow’s `eqn`
to be `.5` directly, we add a constant to the model, so that it can
easily be changed later. This also helps to keep track of how
parametrized the model is.

``` r

sfm <- constant(sfm, work_rate, eqn = .5, label = "Work Rate")
```

As shown above, `eqn` is a generic argument used for all variable types.
`work_rate` can now be used as a variable in the equation for the
outflow from energy:

``` r

sfm <- flow(sfm, work,
  eqn = work_rate, from = energy,
  label = "Depletion from work"
)
```

Note that `eqn` accepts any valid R expression, including functions
(e.g., [`sqrt()`](https://rdrr.io/r/base/MathFun.html),
[`min()`](https://rdrr.io/r/base/Extremes.html)) and arithmetic
operators (e.g., `*`, `+`). Expressions must evaluate to a scalar,
whereas vectors and matrices are not supported. Equations can reference
only other variables defined in the model, as shown above; they cannot
access objects from the user’s R environment.

Flows require more than an `eqn`, and also need to be connected to a
stock, at least as either an inflow (`to`) or an outflow (`from`). Note
that by definition, outflows are subtracted from the stock, and as such
do not need a minus sign in `eqn` to indicate that they decrease the
stock. We simulate the model to check whether energy indeed depletes
from work:

``` r

simulate(sfm) |> plot()
```

As shown in the plot, energy decreases with a constant rate at each time
step, creating linear decay over time. As the outflow is specified as
`.5`, energy decreases by `.5` per unit of time (1 week), producing a
negative energy state. To rectify this implausible behaviour, a naive
solution may be to include a logical statement such as
`ifelse(energy < 0, 0, energy)`. However, this computational trick would
mask model misspecification. Ideally, stocks should remain within bounds
as a result of the model’s equations and parameters. For instance, we
can prevent negative energy by making `work` proportional to the amount
of available energy: `work_rate * energy`.

To assess whether this produces more plausible model behaviour, we
modify the outflow using
[`update()`](https://rdrr.io/r/stats/update.html):

``` r

sfm <- update(sfm, work, eqn = work_rate * energy)

simulate(sfm) |> plot()
```

Energy now follows an exponential decay pattern, where work now depletes
energy until it is zero, but not beyond this point. By letting the flow
rate depend on the level of the stock itself, we have introduced a
*feedback loop* to the system. Positive feedback loops amplify change,
whereas negative feedback loops bring the system back to a target state.
Here, `work` consists of a negative feedback loop that pulls energy to
zero: the higher energy is, the more its outflow decreases it.

To allow energy to recover, we introduce an inflow, again specified as a
simple constant rate:

``` r

sfm <- constant(sfm, recovery_rate, eqn = .3, label = "Recovery Rate") |>
  flow(recovery, eqn = recovery_rate, to = energy, label = "Recovery")

simulate(sfm) |> plot()
```

Flows can differ in their rates, meaning some processes are slower or
faster than others. Here, we have specified recovery to evolve more
slowly than work. As a result of the new inflow, energy now stabilizes
at a fixed level, as energy recovery and depletion balance out. But what
if the ability to recover is not static, but erodes over time? Put
differently, what if the recovery rate is not a constant, but a stock?
To implement this idea, we change the type of `recovery_rate`, and
update its initial value:

``` r

sfm <- change_type(sfm, recovery_rate, new_type = stock) |>
  update(recovery_rate, eqn = 1)
```

We specify with a new outflow that the recovery rate erodes over time as
a function of the available recovery rate and the amount worked.

``` r

sfm <- flow(sfm, erosion,
  eqn = recovery_rate * work,
  from = recovery_rate, label = "Recovery Erosion"
)
simulate(sfm) |> plot()
```

The plot shows how the erosion of the ability to recover produces a
characteristic burnout pattern: a steep initial rise followed by a
collapse of energy. Counter-intuitively, though the recovery rate only
decreases over time, energy first rises. However, this becomes clear
once we inspect the balance between the inflow and outflow. To track
this balance, we introduce an auxiliary: an intermediate variable
computed at each time step that does not itself feed into any stock, but
can be used in flow equations or to monitor other dynamic quantities:

``` r

# Add auxiliary to keep track of net change in energy (inflow - outflow)
sfm <- aux(sfm, net_change, eqn = recovery - work, label = "Net change in energy")
simulate(sfm) |> plot()
```

The net change in energy is initially positive, as recovery exceeds
energy depletion from work. As erosion progressively reduces the
recovery rate, this inflow weakens, eventually falling below the outflow
from work. The net change in energy is negative, and energy begins to
decline, leading to the observed collapse. This complex behaviour is
produced by a simple stock-and-flow model consisting of two stocks and
three flows:

``` r

plot(sfm)
```

Throughout the modelling process, variables can be renamed with
[`change_name()`](https://kcevers.github.io/sdbuildR/reference/change_name.md)
and removed with
[`discard()`](https://kcevers.github.io/sdbuildR/reference/discard.md).
Moreover, we can easily check for common mistakes in the model
specification, such as references to undefined variables or flows which
are not connected to any stock:

``` r

summary(sfm)
#> 
#> ── Stock-and-Flow Model Diagnostics ────────────────────────────────────────────
#> ✔ No problems detected!
```

In summary, stock-and-flow models consist of one or more stocks, each
requiring an inflow and/or outflow to change over time. Without limiting
processes, stocks may continue to increase indefinitely; without
restorative processes, stocks may deplete past the point of recovery.
Although inflows and outflows connected to the same stock could be
combined into a single net flow (as often done in differential equation
models), stock-and-flow modelling generally keeps flows disaggregated.
Separating inflows and outflows encourages more precise thinking about
what processes increase and decrease stocks, and what distinct
information and rates of change govern each flow. Disaggregation also
supports more targeted interventions, for example by identifying whether
to limit inflows or promote outflows.

For convenience, stock-and-flow models may further be supplemented with
constants and auxiliaries. Constants are static parameters that do not
change over the time course of the simulation, whereas auxiliaries are
dynamic, meaning they are computed anew at each step. To illustrate the
difference, a constant with `eqn = runif(1)` will be fixed to a random
number at the beginning of the simulation, whereas an auxiliary with the
same `eqn` will draw a new number each time step. Throughout the
tutorial, we use the term \`\`variable’’ for any part of the system, be
that a stock, flow, constant, or auxiliary. Though this usage may differ
from other scientific fields, we here choose to adhere to system
dynamics terminology.

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
| [`sdbuildR()`](https://kcevers.github.io/sdbuildR/reference/sdbuildR.md) | Create empty model or load template |
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

sfm <- change_name(sfm, recovery_rate, new_name = t)
#> Error in `check_var_existence()`:
#> ! Variable not found in model.
#> ✖ `recovery_rate` does not exist.
```

The name `t` is not usable, as this already refers to the current time
step. Similarly, names cannot contain spaces or special characters:

``` r

sfm <- change_name(sfm, recovery_rate, new_name = recovery - rate)
#> Error in `check_var_existence()`:
#> ! Variable not found in model.
#> ✖ `recovery_rate` does not exist.
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

sfm <- discard(sfm, net_change)
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

sfm <- sdbuildR() |>
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

sfm <- sdbuildR() |>
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
