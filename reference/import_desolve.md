# Import a deSolve model

Convert a model written for
[deSolve](https://cran.r-project.org/package=deSolve) into a
stock-and-flow model of class
[`sdbuildR`](https://kcevers.github.io/sdbuildR/reference/sdbuildR.md).

## Usage

``` r
import_desolve(model, params, init, times, method = "lsoda", name = NULL)
```

## Arguments

- model:

  A deSolve-style ODE function with arguments `(t, state, parameters)`.

- params:

  Named numeric vector of model parameters (constants).

- init:

  Named numeric vector of initial state values (stocks).

- times:

  Numeric vector of time points. Must be evenly spaced (e.g., from
  `seq(start, stop, by = dt)`).

- method:

  Integration method. Defaults to `"lsoda"`. See
  [`sim_methods()`](https://kcevers.github.io/sdbuildR/reference/sim_methods.md).

- name:

  Optional model name. Character scalar.

## Value

A stock-and-flow model of class
[`sdbuildR`](https://kcevers.github.io/sdbuildR/reference/sdbuildR.md).

## Details

The model function must follow the canonical deSolve convention:

    model <- function(t, state, parameters) {
      with(as.list(c(state, parameters)), {
        dX <- <rate expression>   # d<VarName> for each state in init
        list(c(dX))
      })
    }

State variable names are taken from `names(init)`, parameter names from
`names(params)`. Each `d<VarName>` assignment inside the
[`with()`](https://rdrr.io/r/base/with.html) block is parsed as the net
rate of change for stock `VarName` and becomes a flow in the sfm. Any
other assignments in the [`with()`](https://rdrr.io/r/base/with.html)
block (intermediate calculations) are imported as auxiliary variables in
the order they appear.

## See also

[`import_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/import_insightmaker.md),
[`export_model()`](https://kcevers.github.io/sdbuildR/reference/export_model.md),
[`update()`](https://rdrr.io/r/stats/update.html)

## Examples

``` r
logistic_model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN <- r * N * (1 - N / K)
    list(c(dN))
  })
}
sfm <- import_desolve(
  model  = logistic_model,
  params = c(r = 0.3, K = 100),
  init   = c(N = 10),
  times  = seq(0, 50, by = 0.1),
  method = "lsoda",
  name   = "Logistic growth"
)
sim <- simulate(sfm)
plot(sim)

{"x":{"visdat":{"237b3753278d":["function () ","plotlyVisDat"],"237b62092c6b":["function () ","data"]},"cur_data":"237b62092c6b","attrs":{"237b62092c6b":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"legendgroup":{},"type":"scatter","mode":"lines","opacity":1,"colors":"#C87A8A","showlegend":true,"visible":true,"inherit":true}},"layout":{"margin":{"b":50,"l":50,"t":50,"r":50},"legend":{"traceorder":"reversed","font":{"size":14}},"title":"Logistic growth","xaxis":{"domain":[0,1],"automargin":true,"title":"Time (seconds)","font":{"size":16}},"yaxis":{"domain":[0,1],"automargin":true,"title":"","font":{"size":16}},"font":{"family":"Times New Roman","size":16},"showlegend":true,"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[0,5,10,15,20,25,30,35,40,45,50],"y":[10,33.242806347179787,69.056805194174558,90.910671270326162,97.817807064025928,99.504690050826696,99.889054503030167,99.975223358454372,99.994470519677336,99.998766153167907,99.999724688919741],"legendgroup":"N","type":"scatter","mode":"lines","opacity":1,"showlegend":true,"visible":true,"name":"N","marker":{"color":"rgba(200,122,138,1)","line":{"color":"rgba(200,122,138,1)"}},"textfont":{"color":"rgba(200,122,138,1)"},"error_y":{"color":"rgba(200,122,138,1)"},"error_x":{"color":"rgba(200,122,138,1)"},"line":{"color":"rgba(200,122,138,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
