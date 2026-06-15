# Plot timeseries of ensemble simulation

Visualize ensemble simulation results of a stock-and-flow model. Either
summary statistics or individual trajectories can be plotted. When
multiple conditions j are specified, a grid of subplots is plotted. See
[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
for examples.

## Usage

``` r
# S3 method for class 'ensemble_stockflow'
plot(
  x,
  which = c("summary", "sims")[1],
  sim = seq(1, min(c(x[["n"]], 10))),
  condition = seq(1, min(c(x[["n_conditions"]], 9))),
  vars = NULL,
  show_constants = FALSE,
  nrows = ceiling(sqrt(max(condition))),
  margin = 0.05,
  shareX = TRUE,
  shareY = TRUE,
  palette = "Dark 2",
  alpha = 0.3,
  colors = NULL,
  font_family = "Times New Roman",
  font_size = 16,
  wrap_width = 25,
  showlegend = TRUE,
  label_subplots = TRUE,
  central_tendency = c("mean", "median", FALSE)[1],
  central_tendency_width = 3,
  ...
)
```

## Arguments

- x:

  Output of
  [`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md).

- which:

  Type of plot. Either `"summary"` for a summary plot with mean or
  median lines and confidence intervals, or `"sims"` for individual
  simulation trajectories with mean or median lines. Defaults to
  `"summary"`.

- sim:

  Indices of the individual trajectories to plot if which = `"sims"`.
  Defaults to 1:10. Including a high number of trajectories will slow
  down plotting considerably.

- condition:

  Indices of the condition to plot. Defaults to 1:9. If only one
  condition is specified, the plot will not be a grid of subplots.

- vars:

  Variables to plot. Defaults to NULL to plot all variables.

- show_constants:

  If TRUE, include constants in plot. Defaults to FALSE.

- nrows:

  Number of rows in the plot grid. Defaults to
  ceiling(sqrt(n_conditions)).

- margin:

  Margin between subplots. Either a single numeric or a vector of length
  four(left, right, top, bottom). See `?plotly::subplot()` for more
  details. Defaults to 0.05.

- shareX:

  If `TRUE`, share the x-axis across subplots. Defaults to `TRUE`.

- shareY:

  If `TRUE`, share the y-axis across subplots. Defaults to `TRUE`.

- palette:

  Colour palette. Must be one of hcl.pals().

- alpha:

  Trajectory opacity. Defaults to `1`.

- colors:

  Vector of colours. If NULL, the color palette will be used. If
  specified, will override palette. The number of colours must be equal
  to the number of variables in the simulation data frame. Defaults to
  NULL.

- font_family:

  Font family. Defaults to "Times New Roman".

- font_size:

  Font size. Defaults to 16.

- wrap_width:

  Width of text wrapping for labels. Must be an integer. Defaults to 25.

- showlegend:

  Whether to show legend. Must be TRUE or FALSE. Defaults to TRUE.

- label_subplots:

  Whether to plot labels indicating the condition of the subplot.

- central_tendency:

  Central tendency to use for the mean line. Either "mean", "median", or
  FALSE to not plot the central tendency. Defaults to "mean".

- central_tendency_width:

  Line width of central tendency. Defaults to 3.

- ...:

  Optional parameters

## Value

Plotly object

## See also

[`ensemble()`](https://kcevers.github.io/sdbuildR/reference/ensemble.md)
