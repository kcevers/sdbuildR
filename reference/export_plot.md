# Save plot to a file

Save a plot of a stock-and-flow diagram or a simulation to a specified
file path. Note that saving plots requires additional packages to be
installed (see below).

## Usage

``` r
export_plot(
  pl,
  file,
  width = 3,
  height = 4,
  units = "cm",
  dpi = 300,
  font_family = ""
)
```

## Arguments

- pl:

  Plot object. Can be a `grViz` object from the DiagrammeR package (for
  stock-and-flow diagrams) or a `plotly` object from the plotly package
  (for (ensemble) simulation results).

- file:

  File path to save plot to, including a file extension. For plotting a
  stock-and-flow model, the file extension can be one of png, pdf, svg,
  ps, eps, webp. For plotting a simulation, the file extension can be
  one of png, pdf, jpg, jpeg, webp. For plotting a qgraph graph, the
  file extension can be one of png, pdf, svg, ps, eps, jpg, jpeg, tiff,
  bmp. If no file extension is specified, it will default to png.

- width:

  Width of image in units.

- height:

  Height of image in units.

- units:

  Units in which width and height are specified. Either "cm", "in", or
  "px".

- dpi:

  Resolution of image. Only used if units is not "px".

- font_family:

  Font family used for qgraph exports. For PDF/PS/EPS exports, this is
  applied when the graphics device is opened.

## Value

Returns `NULL` invisibly, called for side effects.

## Examples

``` r

# Only if dependencies are installed
if (requireNamespace("DiagrammeRsvg", quietly = TRUE) &&
  requireNamespace("rsvg", quietly = TRUE)) {
  sfm <- sdbuildR("SIR")
  file <- tempfile(fileext = ".png")
  export_plot(plot(sfm), file)

  # Remove plot
  file.remove(file)
}
#> [1] TRUE

if (FALSE) { # \dontrun{
# requires internet
# Only if suggested dependencies are installed
if (requireNamespace("htmlwidgets", quietly = TRUE) &&
  requireNamespace("webshot2", quietly = TRUE)) {
  # Requires Chrome to save plotly plot:
  sim <- simulate(sfm)
  export_plot(plot(sim), file)

  # Remove plot
  file.remove(file)
}
} # }
```
