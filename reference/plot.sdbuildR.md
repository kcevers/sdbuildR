# Plot stock-and-flow diagram

Visualize a stock-and-flow diagram using the R package DiagrammeR.
Stocks are represented as boxes. Flows are represented as arrows between
stocks and/or double circles, where the latter represent what it outside
of the model boundary. Thin grey edges indicate dependencies between
variables. By default, constants (indicated by italic labels) are not
shown. Hover over the variables to see their equations.

## Usage

``` r
# S3 method for class 'sdbuildR'
plot(
  x,
  vars = NULL,
  format_label = TRUE,
  wrap_width = 20,
  font_size = 18,
  font_family = "Times New Roman",
  stock_col = "#83d3d4",
  flow_col = "#f48153",
  dependency_col = "#999999",
  show_dependencies = TRUE,
  show_constants = FALSE,
  show_aux = TRUE,
  minlen = 2,
  pad = 0.1,
  nodesep = 0.3,
  ...
)
```

## Arguments

- x:

  A stock-and-flow model object of class
  [`sdbuildR`](https://kcevers.github.io/sdbuildR/reference/sdbuildR.md).

- vars:

  Variables to plot. Defaults to NULL to plot all variables.

- format_label:

  If TRUE, apply default formatting (removing periods and underscores)
  to labels if labels are the same as variable names.

- wrap_width:

  Width of text wrapping for labels. Must be an integer. Defaults to 20.

- font_size:

  Font size. Defaults to 18.

- font_family:

  Font name. Defaults to "Times New Roman".

- stock_col:

  Colour of stocks. Defaults to "#83d3d4".

- flow_col:

  Colour of flows. Defaults to "#f48153".

- dependency_col:

  Colour of dependency arrows. Defaults to "#999999".

- show_dependencies:

  If TRUE, show dependencies between variables. Defaults to TRUE.

- show_constants:

  If TRUE, show constants. Defaults to FALSE.

- show_aux:

  If TRUE, show auxiliary variables. Defaults to TRUE.

- minlen:

  Minimum length of edges; must be an integer. Defaults to 2.

- pad:

  Padding around the graph. Defaults to 0.1.

- nodesep:

  Minimum distance between nodes. Defaults to 0.3.

- ...:

  Optional arguments

## Value

Stock-and-flow diagram

## See also

[`import_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/import_insightmaker.md),
[`sdbuildR()`](https://kcevers.github.io/sdbuildR/reference/sdbuildR.md),
[`plot.simulate_sdbuildR()`](https://kcevers.github.io/sdbuildR/reference/plot.simulate_sdbuildR.md)

## Examples

``` r
sfm <- sdbuildR("SIR")
plot(sfm)

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=0.1, nodesep= 0.3]\n\n      # Shared across all nodes (persists until overridden)\n      node [fontsize=18,fontname=\"Times New Roman\"]\n\n      # Define stock nodes\n      node [shape=box,style=filled,fillcolor=\"#83d3d4\"]\n      \"infected\" [id=\"infected\",label=\"Infected\",tooltip = \"eqn = 1\"]\n\t\"recovered\" [id=\"recovered\",label=\"Recovered\",tooltip = \"eqn = 0\"]\n\t\"susceptible\" [id=\"susceptible\",label=\"Susceptible\",tooltip = \"eqn = 99999\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      node [style = \"\",shape=plaintext, fontsize=16, width=0.6, height=0.3]\n      \"new_infections\" [id=\"new_infections\",label=\"New infections\", tooltip = \"eqn = infection_rate * susceptible * infected\"]\n\t\"new_recoveries\" [id=\"new_recoveries\",label=\"New recoveries\", tooltip = \"eqn = recovery_rate * infected\"]\n\n      # Define external cloud nodes\n      \n      \n\n      # Define auxiliary nodes\n      \n      \n\n      # Define constant nodes\n      \n      \n\n      # Define flow edges (stock -> flow_node)\n      edge [style = \"\", arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=2, splines=false, tailport=\"e\", headport=\"w\"]\n      \"susceptible\" -> \"new_infections\"\n\t\"infected\" -> \"new_recoveries\"\n\n      # Define flow edges (flow_node -> stock)\n      edge [style = \"\", arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=2, splines=ortho, tailport=\"e\", headport=\"w\"]\n      \"new_infections\" -> \"infected\"\n\t\"new_recoveries\" -> \"recovered\"\n\n      # Define dependency edges\n      edge [style = \"\", color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport = \"_\", headport=\"_\"]\n      \"susceptible\" -> \"new_infections\"\n\t\"infected\" -> \"new_infections\"\n\t\"infected\" -> \"new_recoveries\"\n\n\n      # Rank groupings\n      \n\n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# Don't show constants or auxiliaries
plot(sfm, show_constants = FALSE, show_aux = FALSE)

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=0.1, nodesep= 0.3]\n\n      # Shared across all nodes (persists until overridden)\n      node [fontsize=18,fontname=\"Times New Roman\"]\n\n      # Define stock nodes\n      node [shape=box,style=filled,fillcolor=\"#83d3d4\"]\n      \"infected\" [id=\"infected\",label=\"Infected\",tooltip = \"eqn = 1\"]\n\t\"recovered\" [id=\"recovered\",label=\"Recovered\",tooltip = \"eqn = 0\"]\n\t\"susceptible\" [id=\"susceptible\",label=\"Susceptible\",tooltip = \"eqn = 99999\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      node [style = \"\",shape=plaintext, fontsize=16, width=0.6, height=0.3]\n      \"new_infections\" [id=\"new_infections\",label=\"New infections\", tooltip = \"eqn = infection_rate * susceptible * infected\"]\n\t\"new_recoveries\" [id=\"new_recoveries\",label=\"New recoveries\", tooltip = \"eqn = recovery_rate * infected\"]\n\n      # Define external cloud nodes\n      \n      \n\n      # Define auxiliary nodes\n      \n      \n\n      # Define constant nodes\n      \n      \n\n      # Define flow edges (stock -> flow_node)\n      edge [style = \"\", arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=2, splines=false, tailport=\"e\", headport=\"w\"]\n      \"susceptible\" -> \"new_infections\"\n\t\"infected\" -> \"new_recoveries\"\n\n      # Define flow edges (flow_node -> stock)\n      edge [style = \"\", arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=2, splines=ortho, tailport=\"e\", headport=\"w\"]\n      \"new_infections\" -> \"infected\"\n\t\"new_recoveries\" -> \"recovered\"\n\n      # Define dependency edges\n      edge [style = \"\", color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport = \"_\", headport=\"_\"]\n      \"susceptible\" -> \"new_infections\"\n\t\"infected\" -> \"new_infections\"\n\t\"infected\" -> \"new_recoveries\"\n\n\n      # Rank groupings\n      \n\n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# Only show specific variables
plot(sfm, vars = "susceptible")

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=0.1, nodesep= 0.3]\n\n      # Shared across all nodes (persists until overridden)\n      node [fontsize=18,fontname=\"Times New Roman\"]\n\n      # Define stock nodes\n      node [shape=box,style=filled,fillcolor=\"#83d3d4\"]\n      \"susceptible\" [id=\"susceptible\",label=\"Susceptible\",tooltip = \"eqn = 99999\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      \n      \n\n      # Define external cloud nodes\n      \n      \n\n      # Define auxiliary nodes\n      \n      \n\n      # Define constant nodes\n      \n      \n\n      # Define flow edges (stock -> flow_node)\n      \n      \n\n      # Define flow edges (flow_node -> stock)\n      \n      \n\n      # Define dependency edges\n      edge [style = \"\", color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport = \"_\", headport=\"_\"]\n      \n\n\n      # Rank groupings\n      \n\n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
```
