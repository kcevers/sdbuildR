# Formalizing Job Demands-Resources Theory

Stock-and-flow models present an intuitive way to formalize
psychological systems as dynamic processes that unfold over time. In
this vignette, we will formalize Job Demands-Resources (JD-R) theory as
a stock-and-flow model. JD-R theory is a prominent framework for
understanding burnout and work engagement. Note that this vignette
serves as online supplemental material B accompanying the paper
*Formalizing Psychological Theory with stockflow: A Stock-and-Flow
Modelling Tutorial in R* by Evers et al. (under review). To reproduce
the figures in the paper, please see the corresponding .Rmd file.

``` r

library(sdbuildR)
library(kableExtra)
```

## Overview of System Dynamics Modelling

To develop an understanding of the system, system dynamics modelling
follows a structured process (see below). The first two steps are
covered in detail in the paper, and we will only cover their application
to JD-R theory here.

|  | Step | Description |
|:---|:---|:---|
| **1** | **Problem articulation** |  |
| 1(a) | Target phenomenon | Express the phenomenon to be explained as a pattern over time, which forms the reference mode throughout the modelling process. |
| 1(b) | Key variables | Select the most important variables needed to define and explain the target phenomenon. |
| 1(c) | Time horizon and time unit | Specify the time frame across which the simulation takes place and the time resolution with which variables change. |
| **2** | **Dynamic hypothesis** | Formulate a provisional account explaining how the target phenomenon arises endogenously from the system structure. |
| **3** | **Formalization** |  |
| 3(a) | Formalizing variables | Formulate variables such that they can be represented as continuous quantities. |
| 3(b) | Stock-and-flow diagram | Categorize variables as constants, stocks, flows, or auxiliaries, and draw connections between variables. |
| 3(c) | Stock-and-flow model | Iteratively build and refine the stock-and-flow model to reproduce the target phenomenon. |
| **4** | **Testing** | Perform verification and validity tests to expose errors, misspecifications, and implausibilities in the model. |
| **5** | **Application** | Design and evaluate interventions to identify effective leverage points. |

Overview of the Modelling Process in System Dynamics {.table .table
style="margin-left: auto; margin-right: auto;"}

## Step 1. Problem Articulation

We select as our **target phenomenon** the development of burnout, which
we characterize by the co-occurrence of gradually decreasing work
engagement, increasing exhaustion, and decreasing job performance.

The **key variables** in JD-R theory are job demands, job resources,
work engagement, exhaustion, proactive behaviour, self-undermining
behaviour, and job performance. As working definitions, we follow those
provided in Bakker et al. (2023), as included below.

JD-R theory primarily explains how burnout develops rather than its
maintenance or recovery, suggesting a time horizon of months rather than
years. We adopt six months as the **time horizon** for reproducing our
core phenomenon. The shortest timescale JD-R theory seems to address is
a day, as job demands and resources are thought to fluctuate daily
(Bakker and Demerouti 2024; Bakker et al. 2023).

| Variable | Definition |
|:---|:---|
| Job Demands | The physical, psychological, social, or organizational aspects of the job that require sustained physical, cognitive, and/or emotional effort and are therefore associated with certain physiological and/or psychological costs |
| Job Resources | The physical, psychological, social, or organizational aspects of the job that have motivating potential, that are functional in achieving work goals, that regulate the impact of job demands, and that stimulate learning and personal growth |
| Work Engagement | a positive, fulfilling, work-related state of mind that is characterized by vigor, dedication, and absorption. Vigor refers to high levels of energy and mental resilience while working, the willingness to invest effort in one’s work, and persistence even in the face of difficulties. Dedication implies being strongly involved in one’s work and experiencing a sense of significance, enthusiasm, and challenge. Absorption refers to being fully concentrated and happily engrossed in one’s work, whereby time passes quickly. Thus, work engagement is characterized by a high level of energy and strong identification with one’s work, whereas burnout is characterized by the opposite: a low level of energy and poor identification with one’s work |
| Exhaustion | Depletion of energy resources; also used interchangeable or as part of job strain |
| Proactive Behaviour | Also called job crafting; employees’ personal initiative to change their job demands and job resources in order to better align the design of the job with their own abilities and preferences |
| Self-Undermining Behaviour | Employees’ dysfunctional behaviors (e.g., poor communication, conflict behaviors) that create obstacles and may undermine performance. |
| Job Performance | Undefined in the literature; refers to the extent to which an individual performs well at their job (e.g., fulfilling responsibilities) |

Definition of key variables in Job-Demands Resources Theory from Bakker
et al. (2025) {.table .table
style="margin-left: auto; margin-right: auto;"}

## Step 2. Dynamic Hypothesis

JD-R theory embodies the dynamic hypothesis that burnout occurs as a
result of two competing feedback loops: a health impairment loop, in
which excessive demands produce exhaustion, which in turn triggers
self-undermining behaviour that further increases demands and depletes
resources; and a motivational loop, in which resources foster work
engagement, which promotes proactive behaviour that generates additional
resources and lowers demands. Burnout emerges when the demands and
exhaustion amplified by the health impairment loop overwhelm the
resources and engagement sustained by the motivational loop. As the
target phenomenon is articulated on a within-person level, the dynamic
hypothesis correspondingly describes within-person dynamics.

## Step 3. Formalization

Please see the paper for Step 3a and 3b; here, we only note that
exhaustion was reformulated to energy.

## Step 3c. Building Stock-and-Flow Models in R

As a first step, we initialize a new stock-and-flow model, and set the
simulation to take place over the time course of six months, with the
time unit of a day:

``` r

sfm <- stockflow() |>
  sim_settings(
    stop = round(182.5), time_units = "day",
    # Simulation timestep
    dt = 0.01,
    # Reduce the output size
    save_at = 1,
    # Set only_stocks = FALSE to return all variables (not just stocks) in the simulation output
    only_stocks = FALSE
  )
```

We specify a model name, which will be used as a figure title:

``` r

sfm <- meta(sfm, name = "Job Demands and Resources (JD-R) Theory")
```

In the positive feedback loop between work engagement and job resources,
resources have a motivating impact on engagement and engagement drives
proactive behaviour which increases resources. To implement this diagram
as a stock-and-flow model, we first add two stocks – work engagement and
job resources – and choose arbitrary values for their initial states:

``` r

sfm <- sfm |>
  stock(engagement, eqn = 0.7, label = "Work Engagement") |>
  stock(resources, eqn = 0.5, label = "Job Resources")
```

Both stocks should remain static.

``` r

simulate(sfm) |> plot()
```

With both stocks in place, we next define their inflows. Because
resources and engagement mutually affect each other’s inflows, each
inflow requires a **functional form** specifying how one stock
influences the other’s rate of change. Verbal theories rarely constrain
this choice. JD-R theory, for instance, postulates that resources
motivate engagement but not the magnitude or shape of that effect.

A natural starting point is the simplest functional form: engagement
increases resources, and resources increase engagement, each at a rate
proportional to the other’s current level. This linear functional form
is illustrated above. We begin by adding an inflow to engagement:

``` r

sfm <- sfm |>
  constant(motivation_rate, eqn = .3, label = "Motivation Rate") |>
  flow(motivation, motivation_rate * resources, to = engagement, label = "Motivation")
```

Followed by an inflow to resources:

``` r

sfm <- sfm |>
  constant(proactive_rate, eqn = 0.2, label = "Proactive Behaviour Rate") |>
  flow(proactive,
    eqn = proactive_rate * engagement, to = resources,
    label = "Proactive behaviour"
  )

simulate(sfm) |> plot()
```

As shown above, this produces exponential growth: as resources and
engagement accumulate, their reciprocal effect grows without bound.
Though JD-R theory does postulate that individuals may enter a
`gain cycle'' of increasingly higher engagement and resources [@bakker_job_2023], this is presumably not intended as infinite growth. Though positive correlations are ubiquitous in psychology (i.e., crud factor; @Meehl1990WhyUninterpretable), when formalized, this quickly produces`an
orgy of mutual benefaction’’ that escalates to infinity (May and McLean
2007). Some balancing mechanism must constrain the system. Balancing
mechanisms, such as homeostatic processes, are ubiquitous in real
systems, enhancing resilience by countering perturbations (Meadows
2008). For example, the mutual benefit of resources and engagement could
diminish at higher levels, or both could naturally deteriorate over
time. JD-R theory, however, only states that exhaustion depletes
engagement and resources, leaving any bounding process implied or
omitted. The theory is thus *underspecified* for modelling change over
time, and must be supplemented with a reasonable assumption (Poile and
Safayeni 2016).

Resources and engagement plausibly decay over time, requiring active
upkeep to sustain high levels. A simple formalization is an outflow
proportional to each stock’s current level. We first add an outflow from
engagement:

``` r

sfm <- sfm |>
  constant(engagement_decay_rate, eqn = 0.2, label = "Engagement Decay Rate") |>
  flow(engagement_decay,
    eqn = engagement_decay_rate * engagement,
    from = engagement, label = "Decay"
  )
```

We similarly add an outflow from resources, inspecting whether our
changes curb the system’s unbounded growth:

``` r

sfm <- sfm |>
  constant(resource_decay_rate, eqn = 0.1, label = "Resource Decay Rate") |>
  flow(resource_decay,
    eqn = resource_decay_rate * resources,
    from = resources, label = "Decay"
  )

simulate(sfm) |> plot()
```

Resources and engagement still grow exponentially (see above). The decay
processes are not strong enough to counter the inflows. Though we could
increase the decay rates, this would only stabilize the system if growth
and decay rates (`proactive_rate` and `resource_decay_rate` for
resources; `motivation_rate` and `engagement_decay_rate` for engagement)
are exactly equal. When the growth rate exceeds the decay rate, the
stock explodes to infinity; when the decay rate exceeds the growth rate,
the stock collapses to zero. A feedback loop that appears simple at
first glance thus raises questions that are far from self-evident.
Formalization forces one to specify assumptions that were unstated or
omitted in the verbal theory, such that we may better evaluate the
theory’s plausibility (Rooij and Baggio 2021).

When a model displays implausible behaviour, it helps to revisit the
theoretical propositions embedded in the equations. The inflows to
resources and engagement imply that their mutually beneficial effects
grow without limit as both stocks increase. However, these benefits may
not increase indefinitely, but plateau at high levels. To capture this
saturation, we replace the linear functional form with a Hill function
(see functional forms figure above; Hill (1910)), a sigmoidal (S-shaped)
curve defined by two parameters: the *midpoint*, at which the function
reaches half its maximum value, and the *slope*, which controls how
abruptly the function transitions from low to high values. A slope above
1 produces a sigmoidal shape.

We use a medium slope:

``` r

sfm <- sfm |>
  constant(m_slope, eqn = 3, label = "Medium Slope")
```

And update both inflows to make use of **stockflow**’s
[`hill()`](https://kcevers.github.io/sdbuildR/reference/hill.md)
function, and evaluate the implications of our revised formalization:

``` r

sfm <- sfm |>
  # Update inflow to engagement
  update(motivation, eqn = motivation_rate * hill(resources, m_slope)) |>
  # Update inflow to resources
  update(proactive, eqn = proactive_rate * hill(engagement, m_slope))

simulate(sfm) |> plot()
```

As shown above, resources and engagement now stabilize. Morever, unlike
the linear case, stability is robust to variations in growth and decay
rates rather than requiring exact equality between them. Note, however,
that the Hill function alone does not ensure stability: it places a
ceiling on the *effect* of resources on engagement and vice versa, but
without outflows, both stocks still grow without bound:

``` r

sfm2 <- discard(sfm, c(engagement_decay, resource_decay))

simulate(sfm) |> plot()
```

Stability emerges from the combination of saturating inflows and
proportional decay. Though the system has the same structure in both
cases — the same stocks, inflows, outflows, and dependencies — the
linear and Hill functional forms produce qualitatively different
dynamical behaviour (Robinaugh et al. 2021).

We now present the remaining implementation of JD-R theory that is
omitted from the main text for brevity. To implement the health
impairment pathway, we add two stocks – job demands and energy – as well
as an auxiliary for job performance.

``` r

sfm <- sfm |>
  stock(demands, eqn = .2, label = "Job Demands") |>
  stock(energy, eqn = .9, label = "Energy") |>
  aux(performance, eqn = engagement + energy, label = "Job Performance")

simulate(sfm) |> plot()
```

We next implement the buffer hypothesis, which states that decreases in
energy are induced by job demands, but buffered by resources.
Additionally, engaged employees optimize job demands, so the effect of
demands on energy should be smaller when engagement is higher. In our
implementation, these effects are multiplied by energy to avoid
depleting energy past zero.

``` r

sfm <- sfm |>
  constant(effort_rate, eqn = 0.5, label = "Effort Rate") |>
  flow(effort,
    eqn = effort_rate / (1 + engagement) * energy * demands / (1 + resources),
    from = energy, label = "Effort"
  )
```

Similarly, the boost hypothesis states that job demands amplify the
motivating impact of resources on engagement. This effect is
multiplicative according to Bakker et al. (2023). In our implementation,
we further multiply the effect by energy, because engagement is
partially defined as vigour. Moreover, we suppose energy is required to
sustain motivation.

``` r

sfm <- sfm |>
  flow(motivation,
    eqn = motivation_rate * energy * hill(resources, m_slope) * demands
  )

sim <- simulate(sfm)

# Specify variables to plot
vars <- c("engagement", "demands", "resources", "energy", "performance")
plot(sim, vars = vars)
```

Now that energy has been added to the model, we can implement the
draining effect of exhaustion on resources and engagement. To do so, we
modify both the outflow from resources and engagement to get larger when
energy is low:

``` r

sfm <- sfm |>
  flow(resource_decay, eqn = resource_decay_rate * resources / (1 + energy)) |>
  flow(engagement_decay, eqn = engagement_decay_rate * engagement / (1 + energy))

simulate(sfm) |> plot(vars = vars)
```

JD-R theory only specifies how energy can be depleted, not how it can
recover. This omission leaves a substantial gap in our understanding of
burnout, as sufficient recovery could counter the energy-depleting
effects of job demands, thereby preventing burnout. To ameliorate this
gap, we next allow energy to recover with a new inflow. We implement
this as a Ricker function, meaning that energy recovery is zero when
energy is zero (some energy is needed to recover), recovery is highest
at mid-levels of energy, and recovery drops off at higher levels of
energy (where recovery is not needed).

``` r

sfm <- sfm |>
  constant(recovery_rate, eqn = 0.3, label = "Recovery Rate") |>
  constant(s_slope, eqn = 5, label = "Steep Slope") |>
  flow(recovery,
    eqn = recovery_rate * energy * exp(-s_slope * energy),
    to = energy, label = "Recovery"
  )

simulate(sfm) |> plot(vars = vars)
```

Job demands are reduced by expending energy. Furthermore, the outflow
from job demands is influenced by engagement: engaged employees
“optimize” job demands, which we interpret to mean that they are more
effective at reducing demands. We specify this by multiplying energy by
`(1 + engagement)`, which captures the idea that engagement is not
*necessary* for reducing demands (whereas `* engagement` would turn the
outflow from demands to zero when engagement is zero). Lastly, the
effect is multiplied by demands to prevent reducing demands past zero.

``` r

sfm <- sfm |>
  constant(work_rate, eqn = 0.5, label = "Demand Reduction Rate") |>
  flow(work,
    eqn = work_rate * energy * demands * (1 + engagement),
    from = demands, label = "Work"
  )

simulate(sfm) |> plot(vars = vars)
```

Job demands are increased by self-undermining behaviour. We similarly
implement self-undermining as a steep Ricker function: it is zero when
energy is zero (some energy is needed to self-undermine), increases
steeply at low levels of energy where self-undermining is highest, and
drops off quickly to represent the idea that only exhausted individuals
self-undermine. The effect is independent of demands because
self-undermining behaviour creates new demands even when there are no
existing demands.

``` r

sfm <- sfm |>
  constant(e_slope, eqn = 10, label = "Extreme Slope") |>
  constant(undermining_rate, eqn = 5, label = "Self-undermining Rate") |>
  flow(undermining,
    eqn = undermining_rate * energy * exp(-e_slope * energy),
    to = demands, label = "Self-undermining"
  )

simulate(sfm) |> plot(vars = vars)
```

As of now, job resources are only cultivated through proactive
behaviour, and job demands are only created by self-undermining
behaviour. However, JD-R theory implies that both job resources and
demands are partially exogenously driven. Both are defined as “physical,
psychological, social, or organizational aspects of the job” (Bakker et
al. 2023). We capture these exogenous drivers with a new inflow to job
resources and demands. Here, exogenously provided resources drop off
exponentially at higher levels of resources, reflecting the idea that
employers are less likely to provide resources when employees are
already well-resourced:

``` r

sfm <- sfm |>
  constant(exo_resource_rate, eqn = .1, label = "New resource rate") |>
  flow(exo_resources,
    eqn = exo_resource_rate * exp(-s_slope * resources),
    to = resources, label = "Exogenous support"
  )

simulate(sfm) |> plot(vars = vars)
```

In the absence of more information about the functional form of
exogenous demands, we use the same functional form as for exogenous
resources. Exogenously provided demands drop off exponentially at higher
levels of demands, reflecting the idea that employers are less likely to
assign new tasks when employees are already at high workloads:

``` r

sfm <- sfm |>
  constant(exo_demand_rate, eqn = .1, label = "New task rate") |>
  flow(exo_demands,
    eqn = exo_demand_rate * exp(-s_slope * demands), to = demands,
    label = "Exogenous tasks"
  )
```

Simulating the model reproduces our target phenomenon:

``` r

simulate(sfm) |> plot(vars = vars)
```

As shown above, job demands rise to an excessive degree, rapidly
depleting energy and work engagement. Energy and engagement collapse to
zero and do not recover, forming a permanent burnout.

The target phenomenon should not depend on a single, carefully chosen
initial condition. Individuals may have very different starting levels
of demands, resources, engagement, and energy, yet still experience
burnout. Initial conditions are typically chosen with some degree of
arbitrariness and should not be of substantial consequence to the theory
when varied within reasonable ranges. This is not to say they are
inconsequential. Initializing all stocks at zero, for instance, can
prevent any dynamics from occurring, and different initial conditions
can reveal that the system is capable of qualitatively different
behaviour. As a first assessment of our model’s dependence on initial
conditions, we set the initial state of all stocks to be drawn from a
uniform distribution bounded between .01 and 2:

``` r

sfm <- sfm |>
  update(c(demands, resources, energy, engagement),
    eqn = runif(1, 0.01, 2)
  )

sim1 <- simulate(sfm, seed = 1)
pl1 <- plot(sim1, vars = vars)

sim2 <- simulate(sfm, seed = 2)
pl2 <- plot(sim2, vars = vars, showlegend = FALSE)

sim3 <- simulate(sfm, seed = 6)
pl3 <- plot(sim3, vars = vars, showlegend = FALSE)

pl <- plotly::subplot(pl1, pl2, pl3, nrows = 1)
pl
```

As shown above, multiple variables can be updated at the same time.
Simulating the model several times, we observe that although with
differing speeds and peak magnitudes, the system reliably ends in a
burnout state. A more systematic approach of varying initial conditions
and parameters will be demonstrated in a later section.

To make our simulations reproducible, a seed can be specified:

``` r

sfm <- sim_settings(sfm, seed = 123)
```

We save this version of the stock-and-flow model as `sfm0` for later
use:

``` r

sfm0 <- sfm
```

Note that this is identical to the version stored in the model library,
which can be loaded using
[`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md):

``` r

sfm0 <- sfm <- stockflow("jdr")
```

## Step 4. Testing

### Verification Tests

Verification involves assessing whether our model behaves as we intended
it to. Although it has been designed to do so, an increasingly complex
model can generate unexpected behaviours. A model should conform to
known real-world behaviours, physical limitations and logical
constraints (Sterman 2000). For example, in a population model, setting
birth rates to zero should result in no new people being born.
Similarly, daily work hours should never exceed 24 hours, the severity
of a headache cannot become negative, and income cannot grow to
infinity. Any violation indicates the model needs to be reformulated.
\rev{To implement such verification tests in a stock-and-flow model, we
make use of *unit tests*, a concept from software engineering (Duggan
2016; Fowler and Beck 2019; Martin 2012). A unit test compares the
behaviour of a small aspect of the model (i.e., a unit) to an explicitly
formulated expectation. In JD-R theory, we may for example check that
self-undermining never becomes negative, as behaviours should be
strictly zero or positive:

``` r

sfm <- unit_test(sfm, expr = all(undermining >= 0))
```

As `undermining` refers to the entire timeseries of self-undermining
behaviour, we use [`all()`](https://rdrr.io/r/base/all.html) to check
that all time points are equal to or above zero. To assess whether our
expectation holds, we run
[`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md),
which simulates the model and checks all unit tests on its output:

``` r

verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 1/1 test passed.
#> ✔ 1. undermining is at least 0 (for all values)
```

The model passes the test, increasing our confidence in its
plausibility. A test label
(`"undermining is at least 0 (for all values)"`) has been automatically
generated based on the test’s expectation, but may also be customized by
passing a `label`. To expose more unrealistic behaviours, the model can
be subjected to **extreme conditions**. Extreme values, such as zero,
negative, or infinite values, tend to reveal equation errors more
readily than variations within plausible ranges \[Peterson and Eberlein
(1994); Barlas1996FormalDynamics\]. For example, when job demands start
and remain at zero, no tasks ever enter the system, such that there is
no work to perform well on. In this scenario, job performance should be
low. In
[`unit_test()`](https://kcevers.github.io/sdbuildR/reference/unit_test.md),
we can set the conditions under which an expectation should hold with
`condition`, which should be specified as a named list with only
constants or initial values of stocks. Here, we expect the last value of
job performance to be low when demands start at zero and its inflow
rates at zero:

``` r

conditions <- list(demands = 0, exo_demand_rate = 0, undermining_rate = 0)
sfm <- unit_test(sfm,
  expr = tail(performance, 1) < 0.1,
  conditions = conditions
)

verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 1/2 tests passed.
#> ✔ 1. undermining is at least 0 (for all values)
#> ✖ 2. the last 1 value of performance is less than 0.1 (demands = 0,
#>   exo_demand_rate = 0, undermining_rate = 0)
#>   Expected: TRUE Actual: FALSE
```

Our test failed. To understand why, we may selectively plot the failed
test:

``` r

verify(sfm) |> plot(status = "fail")
```

Despite demands being zero throughout the simulation, performance
reaches high levels. Our formal model enables us to exactly pinpoint the
reason for this implausible behaviour, namely in performance’s `eqn`:

``` r

as.data.frame(sfm, name = performance, properties = "eqn")
#>   type        name                 eqn
#> 1  aux performance engagement + energy
```

As JD-R theory merely states that performance is increased by engagement
and decreased by exhaustion, we have defined performance simply as the
sum of engagement and energy. Demands are thus not directly necessary to
perform well. This may be rectified by simply revising job performance
to also depend on demands, which causes all tests to pass:

``` r

sfm <- update(sfm, performance, eqn = demands * (engagement + energy))

verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 2/2 tests passed.
#> ✔ 1. undermining is at least 0 (for all values)
#> ✔ 2. the last 1 value of performance is less than 0.1 (demands = 0,
#>   exo_demand_rate = 0, undermining_rate = 0)
```

We add this modification to our saved model as well:

``` r

sfm0 <- update(sfm0, performance, eqn = demands * (engagement + energy))
```

We include some additional verification tests checks that are omitted
from the paper for brevity.

``` r

# Job demands and energy should negatively correlate
sfm <- unit_test(sfm, expr = cor(demands, energy) < -.2)
verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 3/3 tests passed.
#> ✔ 1. undermining is at least 0 (for all values)
#> ✔ 2. the last 1 value of performance is less than 0.1 (demands = 0,
#>   exo_demand_rate = 0, undermining_rate = 0)
#> ✔ 3. the correlation between demands and energy is less than -0.2

# Job resources and energy should positive correlate
sfm <- unit_test(sfm, expr = cor(resources, energy) > .2)
verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 4/4 tests passed.
#> ✔ 1. undermining is at least 0 (for all values)✔ 2. the last 1 value of performance is less than 0.1 (demands = 0,
#>   exo_demand_rate = 0, undermining_rate = 0)✔ 3. the correlation between demands and energy is less than -0.2✔ 4. the correlation between resources and energy is greater than 0.2

# Job performance and work engagement should positively correlate
sfm <- unit_test(sfm, expr = cor(engagement, performance) > .2)
verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 5/5 tests passed.
#> ✔ 1. undermining is at least 0 (for all values)✔ 2. the last 1 value of performance is less than 0.1 (demands = 0,
#>   exo_demand_rate = 0, undermining_rate = 0)✔ 3. the correlation between demands and energy is less than -0.2✔ 4. the correlation between resources and energy is greater than 0.2✔ 5. the correlation between engagement and performance is greater than 0.2

# Job performance and energy should positively correlate
sfm <- unit_test(sfm, expr = cor(performance, energy) > .2)
verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 6/6 tests passed.
#> ✔ 1. undermining is at least 0 (for all values)✔ 2. the last 1 value of performance is less than 0.1 (demands = 0,
#>   exo_demand_rate = 0, undermining_rate = 0)✔ 3. the correlation between demands and energy is less than -0.2✔ 4. the correlation between resources and energy is greater than 0.2✔ 5. the correlation between engagement and performance is greater than 0.2✔ 6. the correlation between performance and energy is greater than 0.2
```

In general, behaviours should always be positive:

``` r

sfm <- unit_test(sfm, expr = all(proactive >= 0)) |>
  unit_test(expr = all(work >= 0))

verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 8/8 tests passed.
#> ✔ 1. undermining is at least 0 (for all values)
#> ✔ 2. the last 1 value of performance is less than 0.1 (demands = 0,
#>   exo_demand_rate = 0, undermining_rate = 0)
#> ✔ 3. the correlation between demands and energy is less than -0.2
#> ✔ 4. the correlation between resources and energy is greater than 0.2
#> ✔ 5. the correlation between engagement and performance is greater than 0.2
#> ✔ 6. the correlation between performance and energy is greater than 0.2
#> ✔ 7. proactive is at least 0 (for all values)
#> ✔ 8. work is at least 0 (for all values)
```

When motivation is zero, motivation should be zero at all time points:

``` r

sfm <- unit_test(sfm,
  expr = all(motivation == 0),
  conditions = list(motivation_rate = 0)
)

verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 9/9 tests passed.
#> ✔ 1. undermining is at least 0 (for all values)
#> ✔ 2. the last 1 value of performance is less than 0.1 (demands = 0,
#>   exo_demand_rate = 0, undermining_rate = 0)
#> ✔ 3. the correlation between demands and energy is less than -0.2
#> ✔ 4. the correlation between resources and energy is greater than 0.2
#> ✔ 5. the correlation between engagement and performance is greater than 0.2
#> ✔ 6. the correlation between performance and energy is greater than 0.2
#> ✔ 7. proactive is at least 0 (for all values)
#> ✔ 8. work is at least 0 (for all values)
#> ✔ 9. motivation is equal to 0 (for all values) (motivation_rate = 0)
```

We would expect that when resources are initialized at zero and its
inflow rate is zero, engagement decays to zero:

``` r

sfm <- unit_test(sfm,
  expr = tail(engagement, 1) < 0.01,
  conditions = list(resources = 0, exo_resource_rate = 0)
)

verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 10/10 tests passed.
#> ✔ 1. undermining is at least 0 (for all values)
#> ✔ 2. the last 1 value of performance is less than 0.1 (demands = 0,
#>   exo_demand_rate = 0, undermining_rate = 0)
#> ✔ 3. the correlation between demands and energy is less than -0.2
#> ✔ 4. the correlation between resources and energy is greater than 0.2
#> ✔ 5. the correlation between engagement and performance is greater than 0.2
#> ✔ 6. the correlation between performance and energy is greater than 0.2
#> ✔ 7. proactive is at least 0 (for all values)
#> ✔ 8. work is at least 0 (for all values)
#> ✔ 9. motivation is equal to 0 (for all values) (motivation_rate = 0)
#> ✔ 10. the last 1 value of engagement is less than 0.01 (resources = 0,
#>   exo_resource_rate = 0)
```

As an extreme condition test, we can initialize all stocks at zero. In
this case, only job demands rises.

``` r

sfm <- unit_test(sfm,
  expr = all(is.finite(c(engagement, resources, energy, demands))),
  conditions = list(engagement = 0, resources = 0, energy = 0, demands = 0)
)

result <- verify(sfm)
print(result)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 11/11 tests passed.
#> ✔ 1. undermining is at least 0 (for all values)
#> ✔ 2. the last 1 value of performance is less than 0.1 (demands = 0,
#>   exo_demand_rate = 0, undermining_rate = 0)
#> ✔ 3. the correlation between demands and energy is less than -0.2
#> ✔ 4. the correlation between resources and energy is greater than 0.2
#> ✔ 5. the correlation between engagement and performance is greater than 0.2
#> ✔ 6. the correlation between performance and energy is greater than 0.2
#> ✔ 7. proactive is at least 0 (for all values)
#> ✔ 8. work is at least 0 (for all values)
#> ✔ 9. motivation is equal to 0 (for all values) (motivation_rate = 0)
#> ✔ 10. the last 1 value of engagement is less than 0.01 (resources = 0,
#>   exo_resource_rate = 0)
#> ✔ 11. is.finite([engagement, resources, energy, demands]) (for all values)
#>   (engagement = 0, resources = 0, energy = 0, demands = 0)
```

``` r

plot(result, test = 11)
```

As another robustness check, we initialize all stocks at high values,
which the model is able to handle.

``` r

sfm <- unit_test(sfm,
  expr = all(is.finite(c(engagement, resources, energy, demands))),
  conditions = list(engagement = 5, resources = 5, energy = 5, demands = 5)
)

result <- verify(sfm)
print(result)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 12/12 tests passed.
#> ✔ 1. undermining is at least 0 (for all values)
#> ✔ 2. the last 1 value of performance is less than 0.1 (demands = 0,
#>   exo_demand_rate = 0, undermining_rate = 0)
#> ✔ 3. the correlation between demands and energy is less than -0.2
#> ✔ 4. the correlation between resources and energy is greater than 0.2
#> ✔ 5. the correlation between engagement and performance is greater than 0.2
#> ✔ 6. the correlation between performance and energy is greater than 0.2
#> ✔ 7. proactive is at least 0 (for all values)
#> ✔ 8. work is at least 0 (for all values)
#> ✔ 9. motivation is equal to 0 (for all values) (motivation_rate = 0)
#> ✔ 10. the last 1 value of engagement is less than 0.01 (resources = 0,
#>   exo_resource_rate = 0)
#> ✔ 11. is.finite([engagement, resources, energy, demands]) (for all values)
#>   (engagement = 0, resources = 0, energy = 0, demands = 0)
#> ✔ 12. is.finite([engagement, resources, energy, demands]) (for all values)
#>   (engagement = 5, resources = 5, energy = 5, demands = 5)
```

``` r

plot(result, test = 12)
```

Show all unit tests:

``` r

unit_tests(sfm)
#> 
#> ── Stock-and-Flow Unit Tests ───────────────────────────────────────────────────
#> 12 tests • 12/12 active • 5/12 include conditions
#> • 1. undermining is at least 0 (for all values)
#>   `all(undermining >= 0)`
#> • 2. the last 1 value of performance is less than 0.1 (demands = 0,
#> exo_demand_rate = 0, undermining_rate = 0)
#>   `tail(performance, 1) < 0.1`
#>   Conditions: demands = 0, exo_demand_rate = 0, undermining_rate = 0
#> • 3. the correlation between demands and energy is less than -0.2
#>   `cor(demands, energy) < -0.2`
#> • 4. the correlation between resources and energy is greater than 0.2
#>   `cor(resources, energy) > 0.2`
#> • 5. the correlation between engagement and performance is greater than 0.2
#>   `cor(engagement, performance) > 0.2`
#> • 6. the correlation between performance and energy is greater than 0.2
#>   `cor(performance, energy) > 0.2`
#> • 7. proactive is at least 0 (for all values)
#>   `all(proactive >= 0)`
#> • 8. work is at least 0 (for all values)
#>   `all(work >= 0)`
#> • 9. motivation is equal to 0 (for all values) (motivation_rate = 0)
#>   `all(motivation == 0)`
#>   Conditions: motivation_rate = 0
#> • 10. the last 1 value of engagement is less than 0.01 (resources = 0,
#> exo_resource_rate = 0)
#>   `tail(engagement, 1) < 0.01`
#>   Conditions: resources = 0, exo_resource_rate = 0
#> • 11. is.finite([engagement, resources, energy, demands]) (for all values)
#> (engagement = 0, resources = 0, energy = 0, demands = 0)
#>   `all(is.finite(c(engagement, resources, energy, demands)))`
#>   Conditions: engagement = 0, resources = 0, energy = 0, demands = 0
#> • 12. is.finite([engagement, resources, energy, demands]) (for all values)
#> (engagement = 5, resources = 5, energy = 5, demands = 5)
#>   `all(is.finite(c(engagement, resources, energy, demands)))`
#>   Conditions: engagement = 5, resources = 5, energy = 5, demands = 5
```

Plot all tests:

``` r

verify(sfm) |> plot(nrows = 3)
```

### Uncertainty

To illustrate the impact of aleatory uncertainty in our JD-R model, we
substitute the deterministic formulation of demand influx for a
stochastic process. This more closely aligns with JD-R theory, which
posits job demands may fluctuate rapidly on a daily basis due to
environmental volatility (Bakker and Demerouti 2024; Downes et al.
2021). To implement this, we adopt the Cox-Ingersoll-Ross model (Cox et
al. 1985), a classical stochastic process. It consists of both a
deterministic and stochastic component. The former ensures `demands`
reverts to its mean `demand_mean` with a rate `exo_demand_rate`. The
latter adds normally distributed noise at each step, and scales this
with the amount of demands. As long as `exo_demand_rate` and
`demand_mean` are positive, this ensures that the inflow to demands
cannot become negative, as noise is reduced to zero when demands are
zero.

``` r

sfm <- sfm0 |>
  constant(demand_mean, eqn = 1) |>
  constant(demand_sigma, eqn = 10) |>
  constant(exo_demand_rate, eqn = .1) |>
  aux(D_deterministic, eqn = exo_demand_rate * (demand_mean - demands)) |>
  aux(D_stochastic, eqn = demand_sigma * sqrt(demands) * rnorm(1) * sqrt(dt)) |>
  update(exo_demands, eqn = D_deterministic + D_stochastic)

sim <- simulate(sfm, seed = 1)
plot(sim, vars = vars)
```

As shown above, job demands now exhibit autocorrelated fluctuations,
which in turn create variability in job performance. Intrinsic
variability in one variable may thus propagate to other variables, even
when the latter are strictly deterministically formulated.

### Removing Variables

Theories tend to inflate over time, acquiring more assumptions,
constructs, and interactions in an attempt to gain explanatory breadth
and depth (Haslam 2016; Meehl 1990a; Smid 2023). Though this may indeed
improve a theory’s explanatory power, it can stand in direct opposition
to the principle of parsimony (Keas 2018). Formal models enable a direct
comparison between the predictions of a more extensive versus a simpler
version of the theory (also known as a perturbation analysis; Weisberg
(2013)). In JD-R theory, proactive and self-undermining behaviours are
newer additions to the original theory proposed in 2001 (Bakker et al.
2023), and we may wonder what their contribution is to the model’s
behaviour. We could remove self-undermining behaviour, and compare these
two models:

``` r

sim1 <- simulate(sfm0)
pl1 <- plot(sim1)

sfm2 <- discard(sfm0, undermining)
sim2 <- simulate(sfm2)
pl2 <- plot(sim2, showlegend = FALSE)

pl <- plotly::subplot(pl1, pl2, nrows = 2, shareY = TRUE)
pl
```

As shown above, in our implementation of JD-R theory, self-undermining
behaviour appears to be essential for the occurrence of burnout. Without
self-undermining behaviour, the system settles in a healthy state with
manageable demands and high energy, albeit with low work engagement.
More generally, systematically removing components reveals which aspects
of the theory are necessary for the target phenomenon and which are
theoretically redundant, directly assessing empirical relevance (Dongen
et al. 2025).

### Challenging the Model Boundary

Endogenous and exogenous variables can be distinguished by looking at
their dependencies:

``` r

dependencies(sfm0, name = performance)
#> $performance
#> [1] "demands"    "engagement" "energy"
```

By reversing the dependencies, we obtain which variables depend on job
performance:

``` r

dependencies(sfm0, name = performance, reverse = TRUE)
#> $performance
#> character(0)
```

Job performance has no effect on any part of the system. In other words,
it is merely an outcome variable, illustrating a case of open-loop
thinking. However, it seems plausible that engagement does not only
increase job performance, but that performance itself contributes to
engagement. To represent this idea, we add a new inflow to engagement
that grows with performance.

``` r

# Make performance feedback to the system
sfm2 <- constant(sfm0, performance_effect,
  eqn = .1,
  label = "Effect Job Performance on Work Engagement"
) |>
  flow(pride,
    eqn = performance_effect * performance,
    to = engagement
  )
```

As shown below, this produces a qualitatively new pattern of behaviour:
rather than a permanent burnout, the system now oscillates between a
healthy and burnout state. Expanding the model boundary thus extends the
possible range of model behaviours, which hardly could have been
inferred from the verbal theory alone.

``` r

sim1 <- simulate(sfm0, stop = 500)
pl1 <- plot(sim1, vars = vars)

sim2 <- simulate(sfm2, stop = 500)
pl2 <- plot(sim2, vars = vars, showlegend = FALSE)

pl <- plotly::subplot(pl1, pl2, nrows = 2, shareY = TRUE) |>
  plotly::layout(title = "Model Boundary")
pl
```

## Step 5. Application

### Parallelization

Large-scale simulations are also called **ensemble** simulations, which
can be computationally intensive. We therefore recommend to reduce the
size of the simulation output, for instance by saving only a hundred
time points, evenly spaced across the simulation interval:

``` r

sfm <- sfm0 <- sim_settings(sfm0, save_n = 100)
```

Alternatively, `save_at` can be passed to `sim_settings` to save at
specific time points (e.g., `save_at = c(1, 100)`) or at a fixed
interval (e.g., `save_at = 10` to save every ten time units). To further
support computationally intensive ensemble simulations, **stockflow**
enables parallelization supported by the **future** package (Bengtsson
2021). The parallization backend needs to be configured prior to running
ensemble simulations:

``` r

if (!requireNamespace("future")) {
  install.packages("future")
}
if (!requireNamespace("future.apply")) {
  install.packages("future.apply")
}
future::plan(future::multisession, workers = parallelly::availableCores() - 1)
```

After the ensemble simulations are completed, parallelization can be
ended with:

``` r

future::plan(future::sequential)
```

For even greater computational efficiency, ensemble simulations can be
conducted in Julia. Julia is a modern, open-source programming language
that reaches performance comparable to C and Fortran while maintaining
readable, high-level syntax similar to R and Python (Bezanson et al.
2017). Julia is increasingly finding applications in psychology, such as
for mixed-effects and structural equation modelling (Ernst et al. 2025;
Bates et al. 2025). In the context of **stockflow**, the Julia package
**DifferentialEquations.jl** (Rackauckas and Nie 2017) offers
state-of-the-art differential equation solvers that can vastly
outperform R’s **deSolve** (Rackauckas 2024; Karline Soetaert et al.
2010). To simulate with Julia, **stockflow** translates R to Julia code
and uses the **JuliaConnectoR** package (Lenz et al. 2022) to call Julia
from R, so that users may benefit from Julia’s computational speed
without interacting with Julia directly.

To enable Julia simulations in **stockflow**, Julia and a specific
environment needs to be configured, as detailed in the vignette [Julia
setup](https://kcevers.github.io/sdbuildR/articles/julia-setup.html).
Once completed, the Julia environment can be activated (note that this
needs to be repeated in each new R session):

``` r

use_julia()
#> ℹ Activating Julia environment for sdbuildR at
#>   /home/runner/work/_temp/Library/sdbuildR...
#> ✔ Julia environment ready.
```

Parallelization is also supported with Julia:

``` r

use_julia(nthreads = parallelly::availableCores() - 1)
```

By default, the simulation engine is R, which should be changed in the
simulation settings to make use of Julia:

``` r

sfm <- sfm0 <- sim_settings(sfm0, language = "Julia")
```

To revert to single-threaded simulation, stop and restart the Julia
session:

``` r

use_julia(restart = TRUE)
```

The Julia session can be ended with:

``` r

use_julia(stop = TRUE)
```

In summary, ensemble simulations are more efficient with reduced output
size and can be conducted with or without parallelization in either R or
Julia.

### Exploring Model-Implied Phenomena

A first assessment of the target phenomenon’s robustness may involve
evaluating its dependence on initial conditions. Though we already
simulated the model with several initial conditions, this can be more
systematically examined with an ensemble simulation, where we run the
model for a thousand iterations:

``` r

sims <- ensemble(sfm, n = 1000)
#> Starting ensemble simulation in "Julia" with 1000 simulations.
#> ✔ Ensemble simulation completed in 14.6905 seconds.
plot(sims, vars = c("engagement", "demands"))
```

The mean and 95% confidence interval across all simulations is shown
above. The burnout phenomenon appears robust to variations in initial
conditions, as all simulations eventually converge to the same stable
state of high job demands and zero work engagement.

Parameters (i.e., constants) are often of greater theoretical interest
than initial conditions, as their variation can for instance represent
individual differences, contextual factors, or uncertainty in the
theory’s assumptions. Parameters in ensemble simulations can be varied
by redefining their `eqn` to draw from a distribution, or by passing a
set of values to vary. For example, we can simulate three values of
`motivation_rate`, the rate at which resources increase engagement.

``` r

# Define values to vary
conditions <- list(motivation_rate = c(0.2, 0.7, 4))

# Retain individual simulations
sfm <- sim_settings(sfm, save_sims = TRUE)

# Generate ensemble
n <- 100
sims <- ensemble(sfm, n = n, conditions = conditions)
#> Starting ensemble simulation in "Julia" with 300 simulations in total.
#> ℹ 3 conditions x 100 simulations per condition.
#> ✔ Ensemble simulation completed in 3.3762 seconds.

# Plot all trajectories
plot(sims,
  which = "sims", sim = 1:n, alpha = .75,
  nrows = 3, central_tendency = FALSE,
  vars = c("engagement", "demands")
)
```

### “What If” Scenarios: Developing Interventions

Our ensemble simulations revealed that boosting the rate at which
resources increase engagement could be an effective intervention to
prevent burnout. For example, we may imagine that this parameter could
be targeted by a training that teaches employees to make better use of
their existing resources (Bakker and Van Wingerden 2021). To develop an
intuition of the system’s response to increasing `motivation_rate`, we
can simulate an idealized intervention which is active for a particular
time period (more realistic implementations can be explored at later
modelling stages; Sterman (2000)). To implement this, `motivation_rate`
first needs to be converted from a constant to a stock, as it should
increase over the time course of the simulation:

``` r

sfm <- change_type(sfm0, motivation_rate, new_type = "stock")
```

Next, we create a pulse function that is 1 for a period of two weeks and
0 otherwise. The use of input and interpolation functions should be
preferred over using if-statements. Floating-point precision errors
introduce small numerical inaccuracies in the solver. As a result, hard
logical conditions like `if (t == 0.5)` can yield unpredictable results,
where the condition may fail to occur at all. In contrast, interpolation
functions make the model more robust to numerical errors. In
[`pulse()`](https://kcevers.github.io/sdbuildR/reference/pulse.md), we
set the starting time of the intervention to 21 days and its duration to
14 days. Additionally, we pass the global variable `times` as its first
argument, which specifies the simulation time vector. Other types of
external inputs can be created with the
[`step()`](https://kcevers.github.io/sdbuildR/reference/step.md),
[`ramp()`](https://kcevers.github.io/sdbuildR/reference/ramp.md), and
[`pulse()`](https://kcevers.github.io/sdbuildR/reference/pulse.md)
functions.

``` r

sfm <- sfm |>
  constant(start, eqn = 14) |>
  constant(duration, eqn = 14) |>
  constant(intervention, eqn = pulse(times, start, width = duration)) |>
  flow(intervention_effect,
    eqn = 0.1 * intervention(t),
    to = motivation_rate, label = "Intervention Effect"
  )
```

Run a single simulation:

``` r

simulate(sfm) |> plot()
```

Run an ensemble simulation:

``` r

sfm <- sim_settings(sfm, save_sims = TRUE)
sims <- ensemble(sfm, n = n)
#> Starting ensemble simulation in "Julia" with 100 simulations.
#> ✔ Ensemble simulation completed in 2.8254 seconds.
plot(sims,
  which = "sims", sim = 1:n, alpha = .75, central_tendency = FALSE,
  vars = c("engagement", "demands", "motivation_rate")
)
```

To compute the effectiveness of the intervention, we increase the
simulation length and only save the last timepoint.

``` r

n <- 1000
sfm <- sim_settings(sfm,
  # Only save engagement at the last time point
  vars = "engagement", stop = 1000, save_at = 1000
)
sims <- ensemble(sfm, n = n)
#> Starting ensemble simulation in "Julia" with 1000 simulations.
#> ✔ Ensemble simulation completed in 26.8954 seconds.
df <- as.data.frame(sims, direction = "wide", which = "sims")
tab <- table(round(df$engagement)) |>
  prop.table() |>
  as.data.frame()
colnames(tab) <- c("Engagement", "Proportion")
print(tab)
#>   Engagement Proportion
#> 1          0     0.1895
#> 2          1     0.6930
#> 3          2     0.1175
```

A shorter intervention is not as effective.

``` r

sfm2 <- update(sfm, duration, eqn = 7)
sims2 <- ensemble(sfm2, n = n)
#> Starting ensemble simulation in "Julia" with 1000 simulations.
#> ✔ Ensemble simulation completed in 27.1334 seconds.
df2 <- as.data.frame(sims2, direction = "wide", which = "sims")
tab2 <- table(round(df2$engagement)) |>
  prop.table() |>
  as.data.frame()
colnames(tab2) <- c("Engagement", "Proportion")
print(tab2)
#>   Engagement Proportion
#> 1          0     0.2050
#> 2          1     0.6775
#> 3          2     0.1175
```

### Informing Experimental and Statistical Design

Finally, formal models are powerful tools for guiding experimental and
statistical design. The derivation chain from theory to empirical test
involves a multitude of decisions that the theory itself does not
constrain. Though meta-analyses may quantify the impact of such
decisions, they do not resolve whether discrepant findings reflect mere
design artefacts or genuine challenges to the theory (Meehl 1990b). By
contrast, a formally specified theory predicts what discrepancies are
**implied by the theory itself**. As an illustration, we use our JD-R
model to predict the results of a cross-lagged panel model, a widely
used analysis in the JD-R literature (Upadyaya et al. 2016; Hakanen et
al. 2008; Sorjonen et al. 2024). We assess the cross-lagged relationship
between job demands and work engagement with **lavaan** (Rosseel 2012)
on an ensemble dataset (*n = 10,000*), sampled at day 20 (wave 1) and
three months later at day 110 (wave 2).

As shown below, our model implies that engagement has a strong positive
effect on itself, whereas demands have little effect on future demands.
Higher demands lead to lower future work engagement, but higher
engagement increases future demands. Furthermore, our JD-R model allows
us to assess how these effects depend on the time between waves. For
instance, as shown below, though the autoregressive effect of job
demands is initially negative, it flips in sign as the time between
waves increases. If provided with only a verbal theory, there would be
no principled basis for anticipating this lag dependence.

Stop Julia session:

``` r

use_julia(stop = TRUE)
#> ✔ Closed Julia session.
```

## Session Information

``` r

sessionInfo()
#> R version 4.6.0 (2026-04-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] lavaan_0.6-21    kableExtra_1.4.0 sdbuildR_2.0.0  
#> 
#> loaded via a namespace (and not attached):
#>  [1] tidyr_1.3.2          plotly_4.12.0        sass_0.4.10         
#>  [4] generics_0.1.4       xml2_1.5.2           stringi_1.8.7       
#>  [7] digest_0.6.39        magrittr_2.0.5       evaluate_1.0.5      
#> [10] grid_4.6.0           RColorBrewer_1.1-3   fastmap_1.2.0       
#> [13] jsonlite_2.0.0       this.path_2.8.0      deSolve_1.42        
#> [16] httr_1.4.8           purrr_1.2.2          crosstalk_1.2.2     
#> [19] viridisLite_0.4.3    scales_1.4.0         pbivnorm_0.6.0      
#> [22] lazyeval_0.2.3       textshaping_1.0.5    jquerylib_0.1.4     
#> [25] mnormt_2.1.2         cli_3.6.6            rlang_1.2.0         
#> [28] withr_3.0.2          cachem_1.1.0         yaml_2.3.12         
#> [31] otel_0.2.0           parallel_4.6.0       tools_4.6.0         
#> [34] JuliaConnectoR_1.1.5 dplyr_1.2.1          ggplot2_4.0.3       
#> [37] vctrs_0.7.3          R6_2.6.1             stats4_4.6.0        
#> [40] lifecycle_1.0.5      stringr_1.6.0        fs_2.1.0            
#> [43] htmlwidgets_1.6.4    MASS_7.3-65          ragg_1.5.2          
#> [46] pkgconfig_2.0.3      desc_1.4.3           pkgdown_2.2.0       
#> [49] bslib_0.11.0         pillar_1.11.1        gtable_0.3.6        
#> [52] data.table_1.18.4    glue_1.8.1           systemfonts_1.3.2   
#> [55] xfun_0.58            tibble_3.3.1         tidyselect_1.2.1    
#> [58] rstudioapi_0.19.0    knitr_1.51           farver_2.1.2        
#> [61] htmltools_0.5.9      igraph_2.3.2         rmarkdown_2.31      
#> [64] svglite_2.2.2        compiler_4.6.0       quadprog_1.5-8      
#> [67] S7_0.2.2
```

## References

Bakker, Arnold B., and Evangelia Demerouti. 2024. “Job Demands–Resources
Theory: Frequently Asked Questions.” *Journal of Occupational Health
Psychology* 29 (3): 188–200. <https://doi.org/10.1037/ocp0000376>.

Bakker, Arnold B, Evangelia Demerouti, and Ana Sanz-Vergel. 2023. “Job
Demands-Resources Theory: Ten Years Later.” *Annual Review of
Organizational Psychology and Organizational Behavior* 10: 25–53.
<https://doi.org/10.1146/annurev-orgpsych-120920-053933>.

Bakker, Arnold B., and Jessica Van Wingerden. 2021. “Do Personal
Resources and Strengths Use Increase Work Engagement? The Effects of a
Training Intervention.” *Journal of Occupational Health Psychology* 26
(1): 20–30. <https://doi.org/10.1037/ocp0000266>.

Bates, Douglas, Phillip Alday, Dave Kleinschmidt, et al. 2025.
*MixedModels.jl: A Julia Package for Fitting (Statistical) Mixed-Effects
Models*. Zenodo. <https://doi.org/10.5281/zenodo.596435>.

Bengtsson, Henrik. 2021. “A Unifying Framework for Parallel and
Distributed Processing in R Using Futures.” *The R Journal* 13 (2): 208.
<https://doi.org/10.32614/RJ-2021-048>.

Bezanson, Jeff, Alan Edelman, Stefan Karpinski, and Viral B Shah. 2017.
“Julia: A fresh approach to numerical computing.” *SIAM Review* 59 (1):
65–98. <https://doi.org/10.1137/141000671>.

Cox, John C., Jonathan E. Ingersoll, and Stephen A. Ross. 1985. “A
Theory of the Term Structure of Interest Rates.” *Econometrica* 53 (2):
385. <https://doi.org/10.2307/1911242>.

Dongen, Noah van, Riet van Bork, Adam Finnemann, et al. 2025.
“Productive explanation: A framework for evaluating explanations in
psychological science.” *Psychological Review* 132 (2): 311–29.
<https://doi.org/10.1037/rev0000479>.

Downes, Patrick E., Cody J. Reeves, Brian W. McCormick, Wendy R.
Boswell, and Marcus M. Butts. 2021. “Incorporating Job Demand
Variability Into Job Demands Theory: A Meta-Analysis.” *Journal of
Management* 47 (6): 1630–56. <https://doi.org/10.1177/0149206320916767>.

Duggan, Jim. 2016. *System Dynamics Modeling with R*.
<http://www.springer.com/series/8768>.

Ernst, Maximilian Stefan, Aaron Peikert, and Andreas Markus Brandmaier.
2025. *StructuralEquationModels.jl: A Julia Package for Extensible and
Efficient Structural Equation Modeling*. PsyArXiv.
<https://doi.org/10.31234/osf.io/zwe8g_v1>.

Fowler, Martin, and Kent Beck. 2019. *Refactoring: Improving the Design
of Existing Code*. Second edition. The Addison-Wesley Signature Series.
Addison-Wesley.

Hakanen, Jari J., Wilmar B. Schaufeli, and Kirsi Ahola. 2008. “The Job
Demands-Resources Model: A Three-Year Cross-Lagged Study of Burnout,
Depression, Commitment, and Work Engagement.” *Work & Stress* 22 (3):
224–41. <https://doi.org/10.1080/02678370802379432>.

Haslam, Nick. 2016. “Concept Creep: Psychology’s Expanding Concepts of
Harm and Pathology.” *Psychological Inquiry* 27 (1): 1–17.
<https://doi.org/10.1080/1047840X.2016.1082418>.

Hill, Archibald V. 1910. “The Possible Effects of the Aggregation of the
Molecules of Haemoglobin on Its Dissociation Curves.” *The Journal of
Physiology* 40 (Proc. Physiol. Soc.): iv–vii.
<https://doi.org/10.1113/jphysiol.1910.sp001386>.

Karline Soetaert, Thomas Petzoldt, and R. Woodrow Setzer. 2010. “Solving
Differential Equations in R: Package deSolve.” *Journal of Statistical
Software* 33 (9): 1–25. <https://doi.org/10.18637/jss.v033.i09>.

Keas, Michael N. 2018. “Systematizing the theoretical virtues.”
*Synthese* 195 (6): 2761–93.
<https://doi.org/10.1007/s11229-017-1355-6>.

Lenz, Stefan, Maren Hackenberg, and Harald Binder. 2022. “The
JuliaConnectoR: A Functionally-Oriented Interface for Integrating Julia
in R.” *Journal of Statistical Software* 101 (6): 1–24.
<https://doi.org/10.18637/jss.v101.i06>.

Martin, Robert C. 2012. *Clean Code: A Handbook of Agile Software
Craftsmanship*. Repr. Robert C. Martin Series. Prentice Hall.

May, Robert, and Angela R. McLean, eds. 2007. *Theoretical Ecology:
Principles and Applications*. 3rd ed. Oxford University Press.
<https://doi.org/10.1093/oso/9780199209989.001.0001>.

Meadows, Donella H. 2008. *Thinking in Systems: A Primer*. Chelsea Green
Publishing.

Meehl, Paul E. 1990a. “Appraising and Amending Theories: The Strategy of
Lakatosian Defense and Two Principles That Warrant It.” *Psychological
Inquiry* 1 (2): 108–41.

Meehl, Paul E. 1990b. “Why Summaries of Research on Psychological
Theories Are Often Uninterpretable.” *Psychological Reports* 66:
195–244.

Peterson, David W., and Robert L. Eberlein. 1994. “Reality check: A
bridge between systems thinking and system dynamics.” *System Dynamics
Review* 10 (2-3): 159–74. <https://doi.org/10.1002/sdr.4260100205>.

Poile, Christopher, and Frank Safayeni. 2016. *Using Computational
Modeling for Building Theory: A Double Edged Sword*.

Rackauckas, Christopher. 2024. *GPU-Accelerated Ordinary Differential
Equations (ODE) in R with Diffeqr*.
<https://CRAN.R-project.org/package=diffeqr>.

Rackauckas, Christopher, and Qing Nie. 2017. “DifferentialEquations.jl–a
Performant and Feature-Rich Ecosystem for Solving Differential Equations
in Julia.” *Journal of Open Research Software* 5 (1).

Robinaugh, Donald J., Jonas M. B. Haslbeck, Oisín Ryan, Eiko I. Fried,
and Lourens J. Waldorp. 2021. “Invisible Hands and Fine Calipers: A Call
to Use Formal Theory as a Toolkit for Theory Construction.”
*Perspectives on Psychological Science* 16 (4): 725–43.
<https://doi.org/10.1177/1745691620974697>.

Rooij, Iris van, and Giosuè Baggio. 2021. “Theory Before the Test: How
to Build High-Verisimilitude Explanatory Theories in Psychological
Science.” *Perspectives on Psychological Science* 16 (4): 682–97.
<https://doi.org/10.1177/1745691620970604>.

Rosseel, Yves. 2012. “Lavaan: An R Package for Structural Equation
Modeling.” *Journal of Statistical Software* 48 (2): 1–36.
<https://doi.org/10.18637/jss.v048.i02>.

Smid, Jeroen. 2023. “The Magic of Ad Hoc Solutions.” *Journal of the
American Philosophical Association* 9 (4): 724–41.
<https://doi.org/10.1017/apa.2022.27>.

Sorjonen, Kimmo, Bo Melin, Filippa Folke, and Marika Melin. 2024.
*Questionable Prospective Effects on Burnout and Exhaustion: Simulated
Reanalyses of Cross-Lagged Panel Models*. PsyArXiv.
<https://doi.org/10.31234/osf.io/nz3xk>.

Sterman, John D. 2000. *Business dynamics: systems thinking and modeling
for a complex world*. Irwin/McGraw-Hill.

Upadyaya, Katja, Matti Vartiainen, and Katariina Salmela-Aro. 2016.
“From Job Demands and Resources to Work Engagement, Burnout, Life
Satisfaction, Depressive Symptoms, and Occupational Health.” *Burnout
Research* 3 (4): 101–8. <https://doi.org/10.1016/j.burn.2016.10.001>.

Weisberg, Michael. 2013. *Simulation and Similarity: Using Models to
Understand the World*.
