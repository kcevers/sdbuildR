# Plot stock-and-flow diagram

Visualize a stock-and-flow diagram using the R package DiagrammeR.
Stocks are represented as boxes. Flows are represented as arrows between
stocks and/or double circles, where the latter represent what it outside
of the model boundary. Thin grey edges indicate dependencies between
variables. By default, constants (indicated by italic labels) are not
shown. Hover over the variables to see their equations.

## Usage

``` r
# S3 method for class 'stockflow'
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
  label_col = "black",
  show_eqn = TRUE,
  show_tooltip = TRUE,
  show_dependencies = TRUE,
  show_constants = FALSE,
  show_aux = TRUE,
  minlen = 1,
  pad = 0.1,
  nodesep = 0.3,
  direction = "LR",
  align = NULL,
  order = NULL,
  ...
)
```

## Arguments

- x:

  A stock-and-flow model object of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).

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

- label_col:

  Colour of variable labels (and of the equation text when
  `show_eqn = TRUE`). Defaults to "black".

- show_eqn:

  If `TRUE`, show each variable's equation on a new line beneath its
  label, in a smaller font and the same colour as the label
  (`label_col`). Defaults to `TRUE`.

- show_tooltip:

  If `TRUE`, show each variable's equation as a tooltip when hovering
  over it. Defaults to `TRUE`.

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

- direction:

  Overall flow direction of the layout, passed to Graphviz's `rankdir`.
  One of `"LR"` (left-to-right, the default), `"TB"` (top-to-bottom),
  `"RL"` (right-to-left), or `"BT"` (bottom-to-top).

- align:

  Optional alignment of variables *across* the flow direction. A
  character vector of variable names, or a list of such vectors. Each
  group is placed on the same Graphviz rank (`{rank=same; ...}`), so its
  members line up (vertically when `direction = "LR"`, horizontally when
  `direction = "TB"`). Works for any variable (stocks, flows,
  auxiliaries, constants), not only stocks. Names that are not currently
  drawn (hidden by `vars`, `show_constants`, or `show_aux`) are ignored
  with a warning; unknown names raise an error. Defaults to `NULL`.

- order:

  Optional ordering of variables *along* the flow direction. A character
  vector of variable names, or a list of such vectors, giving the
  desired sequence. Implemented as invisible edges between consecutive
  names, so it acts as a soft hint that Graphviz balances against the
  real flows rather than a hard constraint. On its own it sequences
  variables into successive ranks (e.g. separate columns when
  `direction = "LR"`); to instead line variables up in a single rank and
  control their order *within* it, combine `order` with `align` (the
  `align` group sets the rank, `order` sets the position within it).
  Same validation as `align`. Defaults to `NULL`.

- ...:

  Optional arguments

## Value

Stock-and-flow diagram

## See also

[`import_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/import_insightmaker.md),
[`stockflow()`](https://kcevers.github.io/sdbuildR/reference/stockflow.md),
[`plot.simulate_stockflow()`](https://kcevers.github.io/sdbuildR/reference/plot.simulate_stockflow.md)

## Examples

``` r
sfm <- stockflow("sir")
plot(sfm)

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=0.1, nodesep=0.3, splines = true, concentrate = false]\n\n      # Shared across all nodes (persists until overridden)\n      node [fontsize=18,fontname=\"Times New Roman\",fontcolor=\"black\"]\n\n      # Define stock nodes\n      node [shape=box,style=filled,fillcolor=\"#83d3d4\"]\n      \"infected\" [id=\"infected\",label=<Infected<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 1<\/FONT>>, tooltip = \"Stock: Infected\\nName: infected\\nInitial value: 1\\nInflows: New infections\\nOutflows: New recoveries\"]\n\t\"recovered\" [id=\"recovered\",label=<Recovered<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 0<\/FONT>>, tooltip = \"Stock: Recovered\\nName: recovered\\nInitial value: 0\\nInflows: New recoveries\\nOutflows: —\"]\n\t\"susceptible\" [id=\"susceptible\",label=<Susceptible<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 99999<\/FONT>>, tooltip = \"Stock: Susceptible\\nName: susceptible\\nInitial value: 99999\\nInflows: —\\nOutflows: New infections\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      node [style = \"\",shape=plaintext, fontsize=16, width=0.6, height=0.3]\n      \"new_infections\" [id=\"new_infections\",label=<New infections<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = infection_rate<BR/>* susceptible *<BR/>infected<\/FONT>>, tooltip = \"Flow: New infections\\nName: new_infections\\nEquation: infection_rate * susceptible * infected\\nFrom: Susceptible\\nTo: Infected\"]\n\t\"new_recoveries\" [id=\"new_recoveries\",label=<New recoveries<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = recovery_rate *<BR/>infected<\/FONT>>, tooltip = \"Flow: New recoveries\\nName: new_recoveries\\nEquation: recovery_rate * infected\\nFrom: Infected\\nTo: Recovered\"]\n\n      # Define external cloud nodes\n      \n      \n\n      # Define auxiliary nodes\n      \n      \n\n      # Define constant nodes\n      \n      \n\n      # Define flow edges (stock -> flow_node)\n      edge [style = \"\", arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"susceptible\" -> \"new_infections\"\n\t\"infected\" -> \"new_recoveries\"\n\n      # Define flow edges (flow_node -> stock)\n      edge [style = \"\", arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"new_infections\" -> \"infected\"\n\t\"new_recoveries\" -> \"recovered\"\n\n      # Define dependency edges\n      edge [style = \"\", color=\"#999999\", arrowsize=0.8, penwidth=1, constraint=false, tailport = \"_\", headport=\"_\"]\n      \"susceptible\" -> \"new_infections\" [headport = \"s\", tailport = \"s\"]\n\t\"infected\" -> \"new_infections\" [headport = \"n\", tailport = \"n\"]\n\t\"infected\" -> \"new_recoveries\" [headport = \"s\", tailport = \"s\"]\n\n      \n\n\n      # Rank groupings\n      \n\n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# Don't show constants or auxiliaries
plot(sfm, show_constants = FALSE, show_aux = FALSE)

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=0.1, nodesep=0.3, splines = true, concentrate = false]\n\n      # Shared across all nodes (persists until overridden)\n      node [fontsize=18,fontname=\"Times New Roman\",fontcolor=\"black\"]\n\n      # Define stock nodes\n      node [shape=box,style=filled,fillcolor=\"#83d3d4\"]\n      \"infected\" [id=\"infected\",label=<Infected<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 1<\/FONT>>, tooltip = \"Stock: Infected\\nName: infected\\nInitial value: 1\\nInflows: New infections\\nOutflows: New recoveries\"]\n\t\"recovered\" [id=\"recovered\",label=<Recovered<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 0<\/FONT>>, tooltip = \"Stock: Recovered\\nName: recovered\\nInitial value: 0\\nInflows: New recoveries\\nOutflows: —\"]\n\t\"susceptible\" [id=\"susceptible\",label=<Susceptible<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 99999<\/FONT>>, tooltip = \"Stock: Susceptible\\nName: susceptible\\nInitial value: 99999\\nInflows: —\\nOutflows: New infections\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      node [style = \"\",shape=plaintext, fontsize=16, width=0.6, height=0.3]\n      \"new_infections\" [id=\"new_infections\",label=<New infections<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = infection_rate<BR/>* susceptible *<BR/>infected<\/FONT>>, tooltip = \"Flow: New infections\\nName: new_infections\\nEquation: infection_rate * susceptible * infected\\nFrom: Susceptible\\nTo: Infected\"]\n\t\"new_recoveries\" [id=\"new_recoveries\",label=<New recoveries<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = recovery_rate *<BR/>infected<\/FONT>>, tooltip = \"Flow: New recoveries\\nName: new_recoveries\\nEquation: recovery_rate * infected\\nFrom: Infected\\nTo: Recovered\"]\n\n      # Define external cloud nodes\n      \n      \n\n      # Define auxiliary nodes\n      \n      \n\n      # Define constant nodes\n      \n      \n\n      # Define flow edges (stock -> flow_node)\n      edge [style = \"\", arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"susceptible\" -> \"new_infections\"\n\t\"infected\" -> \"new_recoveries\"\n\n      # Define flow edges (flow_node -> stock)\n      edge [style = \"\", arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"new_infections\" -> \"infected\"\n\t\"new_recoveries\" -> \"recovered\"\n\n      # Define dependency edges\n      edge [style = \"\", color=\"#999999\", arrowsize=0.8, penwidth=1, constraint=false, tailport = \"_\", headport=\"_\"]\n      \"susceptible\" -> \"new_infections\" [headport = \"s\", tailport = \"s\"]\n\t\"infected\" -> \"new_infections\" [headport = \"n\", tailport = \"n\"]\n\t\"infected\" -> \"new_recoveries\" [headport = \"s\", tailport = \"s\"]\n\n      \n\n\n      # Rank groupings\n      \n\n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# Only show specific variables
plot(sfm, vars = "susceptible")

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=0.1, nodesep=0.3, splines = true, concentrate = false]\n\n      # Shared across all nodes (persists until overridden)\n      node [fontsize=18,fontname=\"Times New Roman\",fontcolor=\"black\"]\n\n      # Define stock nodes\n      node [shape=box,style=filled,fillcolor=\"#83d3d4\"]\n      \"susceptible\" [id=\"susceptible\",label=<Susceptible<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 99999<\/FONT>>, tooltip = \"Stock: Susceptible\\nName: susceptible\\nInitial value: 99999\\nInflows: —\\nOutflows: —\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      \n      \n\n      # Define external cloud nodes\n      \n      \n\n      # Define auxiliary nodes\n      \n      \n\n      # Define constant nodes\n      \n      \n\n      # Define flow edges (stock -> flow_node)\n      \n      \n\n      # Define flow edges (flow_node -> stock)\n      \n      \n\n      # Define dependency edges\n      edge [style = \"\", color=\"#999999\", arrowsize=0.8, penwidth=1, constraint=false, tailport = \"_\", headport=\"_\"]\n      \n\n      \n\n\n      # Rank groupings\n      \n\n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# Hide the equations shown beneath each label
plot(sfm, show_eqn = FALSE)

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=0.1, nodesep=0.3, splines = true, concentrate = false]\n\n      # Shared across all nodes (persists until overridden)\n      node [fontsize=18,fontname=\"Times New Roman\",fontcolor=\"black\"]\n\n      # Define stock nodes\n      node [shape=box,style=filled,fillcolor=\"#83d3d4\"]\n      \"infected\" [id=\"infected\",label=\"Infected\", tooltip = \"Stock: Infected\\nName: infected\\nInitial value: 1\\nInflows: New infections\\nOutflows: New recoveries\"]\n\t\"recovered\" [id=\"recovered\",label=\"Recovered\", tooltip = \"Stock: Recovered\\nName: recovered\\nInitial value: 0\\nInflows: New recoveries\\nOutflows: —\"]\n\t\"susceptible\" [id=\"susceptible\",label=\"Susceptible\", tooltip = \"Stock: Susceptible\\nName: susceptible\\nInitial value: 99999\\nInflows: —\\nOutflows: New infections\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      node [style = \"\",shape=plaintext, fontsize=16, width=0.6, height=0.3]\n      \"new_infections\" [id=\"new_infections\",label=\"New infections\", tooltip = \"Flow: New infections\\nName: new_infections\\nEquation: infection_rate * susceptible * infected\\nFrom: Susceptible\\nTo: Infected\"]\n\t\"new_recoveries\" [id=\"new_recoveries\",label=\"New recoveries\", tooltip = \"Flow: New recoveries\\nName: new_recoveries\\nEquation: recovery_rate * infected\\nFrom: Infected\\nTo: Recovered\"]\n\n      # Define external cloud nodes\n      \n      \n\n      # Define auxiliary nodes\n      \n      \n\n      # Define constant nodes\n      \n      \n\n      # Define flow edges (stock -> flow_node)\n      edge [style = \"\", arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"susceptible\" -> \"new_infections\"\n\t\"infected\" -> \"new_recoveries\"\n\n      # Define flow edges (flow_node -> stock)\n      edge [style = \"\", arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"new_infections\" -> \"infected\"\n\t\"new_recoveries\" -> \"recovered\"\n\n      # Define dependency edges\n      edge [style = \"\", color=\"#999999\", arrowsize=0.8, penwidth=1, constraint=false, tailport = \"_\", headport=\"_\"]\n      \"susceptible\" -> \"new_infections\" [headport = \"s\", tailport = \"s\"]\n\t\"infected\" -> \"new_infections\" [headport = \"n\", tailport = \"n\"]\n\t\"infected\" -> \"new_recoveries\" [headport = \"s\", tailport = \"s\"]\n\n      \n\n\n      # Rank groupings\n      \n\n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# Hide the equation tooltips shown on hover
plot(sfm, show_tooltip = FALSE)

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=0.1, nodesep=0.3, splines = true, concentrate = false]\n\n      # Shared across all nodes (persists until overridden)\n      node [fontsize=18,fontname=\"Times New Roman\",fontcolor=\"black\"]\n\n      # Define stock nodes\n      node [shape=box,style=filled,fillcolor=\"#83d3d4\"]\n      \"infected\" [id=\"infected\",label=<Infected<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 1<\/FONT>>]\n\t\"recovered\" [id=\"recovered\",label=<Recovered<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 0<\/FONT>>]\n\t\"susceptible\" [id=\"susceptible\",label=<Susceptible<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 99999<\/FONT>>]\n\n      # Define flow nodes (intermediate nodes for flows)\n      node [style = \"\",shape=plaintext, fontsize=16, width=0.6, height=0.3]\n      \"new_infections\" [id=\"new_infections\",label=<New infections<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = infection_rate<BR/>* susceptible *<BR/>infected<\/FONT>>]\n\t\"new_recoveries\" [id=\"new_recoveries\",label=<New recoveries<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = recovery_rate *<BR/>infected<\/FONT>>]\n\n      # Define external cloud nodes\n      \n      \n\n      # Define auxiliary nodes\n      \n      \n\n      # Define constant nodes\n      \n      \n\n      # Define flow edges (stock -> flow_node)\n      edge [style = \"\", arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"susceptible\" -> \"new_infections\"\n\t\"infected\" -> \"new_recoveries\"\n\n      # Define flow edges (flow_node -> stock)\n      edge [style = \"\", arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"new_infections\" -> \"infected\"\n\t\"new_recoveries\" -> \"recovered\"\n\n      # Define dependency edges\n      edge [style = \"\", color=\"#999999\", arrowsize=0.8, penwidth=1, constraint=false, tailport = \"_\", headport=\"_\"]\n      \"susceptible\" -> \"new_infections\" [headport = \"s\", tailport = \"s\"]\n\t\"infected\" -> \"new_infections\" [headport = \"n\", tailport = \"n\"]\n\t\"infected\" -> \"new_recoveries\" [headport = \"s\", tailport = \"s\"]\n\n      \n\n\n      # Rank groupings\n      \n\n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# Custom label colour
plot(sfm, label_col = "#333333")

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=0.1, nodesep=0.3, splines = true, concentrate = false]\n\n      # Shared across all nodes (persists until overridden)\n      node [fontsize=18,fontname=\"Times New Roman\",fontcolor=\"#333333\"]\n\n      # Define stock nodes\n      node [shape=box,style=filled,fillcolor=\"#83d3d4\"]\n      \"infected\" [id=\"infected\",label=<Infected<BR/><FONT POINT-SIZE=\"13\" COLOR=\"#333333\">eqn = 1<\/FONT>>, tooltip = \"Stock: Infected\\nName: infected\\nInitial value: 1\\nInflows: New infections\\nOutflows: New recoveries\"]\n\t\"recovered\" [id=\"recovered\",label=<Recovered<BR/><FONT POINT-SIZE=\"13\" COLOR=\"#333333\">eqn = 0<\/FONT>>, tooltip = \"Stock: Recovered\\nName: recovered\\nInitial value: 0\\nInflows: New recoveries\\nOutflows: —\"]\n\t\"susceptible\" [id=\"susceptible\",label=<Susceptible<BR/><FONT POINT-SIZE=\"13\" COLOR=\"#333333\">eqn = 99999<\/FONT>>, tooltip = \"Stock: Susceptible\\nName: susceptible\\nInitial value: 99999\\nInflows: —\\nOutflows: New infections\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      node [style = \"\",shape=plaintext, fontsize=16, width=0.6, height=0.3]\n      \"new_infections\" [id=\"new_infections\",label=<New infections<BR/><FONT POINT-SIZE=\"13\" COLOR=\"#333333\">eqn = infection_rate<BR/>* susceptible *<BR/>infected<\/FONT>>, tooltip = \"Flow: New infections\\nName: new_infections\\nEquation: infection_rate * susceptible * infected\\nFrom: Susceptible\\nTo: Infected\"]\n\t\"new_recoveries\" [id=\"new_recoveries\",label=<New recoveries<BR/><FONT POINT-SIZE=\"13\" COLOR=\"#333333\">eqn = recovery_rate *<BR/>infected<\/FONT>>, tooltip = \"Flow: New recoveries\\nName: new_recoveries\\nEquation: recovery_rate * infected\\nFrom: Infected\\nTo: Recovered\"]\n\n      # Define external cloud nodes\n      \n      \n\n      # Define auxiliary nodes\n      \n      \n\n      # Define constant nodes\n      \n      \n\n      # Define flow edges (stock -> flow_node)\n      edge [style = \"\", arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"susceptible\" -> \"new_infections\"\n\t\"infected\" -> \"new_recoveries\"\n\n      # Define flow edges (flow_node -> stock)\n      edge [style = \"\", arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"new_infections\" -> \"infected\"\n\t\"new_recoveries\" -> \"recovered\"\n\n      # Define dependency edges\n      edge [style = \"\", color=\"#999999\", arrowsize=0.8, penwidth=1, constraint=false, tailport = \"_\", headport=\"_\"]\n      \"susceptible\" -> \"new_infections\" [headport = \"s\", tailport = \"s\"]\n\t\"infected\" -> \"new_infections\" [headport = \"n\", tailport = \"n\"]\n\t\"infected\" -> \"new_recoveries\" [headport = \"s\", tailport = \"s\"]\n\n      \n\n\n      # Rank groupings\n      \n\n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# Lay the model out top-to-bottom instead of left-to-right
plot(sfm, direction = "TB")

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = TB, center=true, outputorder=\"edgesfirst\", pad=0.1, nodesep=0.3, splines = true, concentrate = false]\n\n      # Shared across all nodes (persists until overridden)\n      node [fontsize=18,fontname=\"Times New Roman\",fontcolor=\"black\"]\n\n      # Define stock nodes\n      node [shape=box,style=filled,fillcolor=\"#83d3d4\"]\n      \"infected\" [id=\"infected\",label=<Infected<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 1<\/FONT>>, tooltip = \"Stock: Infected\\nName: infected\\nInitial value: 1\\nInflows: New infections\\nOutflows: New recoveries\"]\n\t\"recovered\" [id=\"recovered\",label=<Recovered<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 0<\/FONT>>, tooltip = \"Stock: Recovered\\nName: recovered\\nInitial value: 0\\nInflows: New recoveries\\nOutflows: —\"]\n\t\"susceptible\" [id=\"susceptible\",label=<Susceptible<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 99999<\/FONT>>, tooltip = \"Stock: Susceptible\\nName: susceptible\\nInitial value: 99999\\nInflows: —\\nOutflows: New infections\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      node [style = \"\",shape=plaintext, fontsize=16, width=0.6, height=0.3]\n      \"new_infections\" [id=\"new_infections\",label=<New infections<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = infection_rate<BR/>* susceptible *<BR/>infected<\/FONT>>, tooltip = \"Flow: New infections\\nName: new_infections\\nEquation: infection_rate * susceptible * infected\\nFrom: Susceptible\\nTo: Infected\"]\n\t\"new_recoveries\" [id=\"new_recoveries\",label=<New recoveries<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = recovery_rate *<BR/>infected<\/FONT>>, tooltip = \"Flow: New recoveries\\nName: new_recoveries\\nEquation: recovery_rate * infected\\nFrom: Infected\\nTo: Recovered\"]\n\n      # Define external cloud nodes\n      \n      \n\n      # Define auxiliary nodes\n      \n      \n\n      # Define constant nodes\n      \n      \n\n      # Define flow edges (stock -> flow_node)\n      edge [style = \"\", arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=1, tailport=\"s\", headport=\"n\"]\n      \"susceptible\" -> \"new_infections\"\n\t\"infected\" -> \"new_recoveries\"\n\n      # Define flow edges (flow_node -> stock)\n      edge [style = \"\", arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=1, tailport=\"s\", headport=\"n\"]\n      \"new_infections\" -> \"infected\"\n\t\"new_recoveries\" -> \"recovered\"\n\n      # Define dependency edges\n      edge [style = \"\", color=\"#999999\", arrowsize=0.8, penwidth=1, constraint=false, tailport = \"_\", headport=\"_\"]\n      \"susceptible\" -> \"new_infections\" [headport = \"s\", tailport = \"s\"]\n\t\"infected\" -> \"new_infections\" [headport = \"n\", tailport = \"n\"]\n\t\"infected\" -> \"new_recoveries\" [headport = \"s\", tailport = \"s\"]\n\n      \n\n\n      # Rank groupings\n      \n\n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# Align variables across the flow direction (same Graphviz rank)
plot(sfm, align = c("susceptible", "recovered"))

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=0.1, nodesep=0.3, splines = true, concentrate = false]\n\n      # Shared across all nodes (persists until overridden)\n      node [fontsize=18,fontname=\"Times New Roman\",fontcolor=\"black\"]\n\n      # Define stock nodes\n      node [shape=box,style=filled,fillcolor=\"#83d3d4\"]\n      \"infected\" [id=\"infected\",label=<Infected<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 1<\/FONT>>, tooltip = \"Stock: Infected\\nName: infected\\nInitial value: 1\\nInflows: New infections\\nOutflows: New recoveries\"]\n\t\"recovered\" [id=\"recovered\",label=<Recovered<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 0<\/FONT>>, tooltip = \"Stock: Recovered\\nName: recovered\\nInitial value: 0\\nInflows: New recoveries\\nOutflows: —\"]\n\t\"susceptible\" [id=\"susceptible\",label=<Susceptible<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 99999<\/FONT>>, tooltip = \"Stock: Susceptible\\nName: susceptible\\nInitial value: 99999\\nInflows: —\\nOutflows: New infections\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      node [style = \"\",shape=plaintext, fontsize=16, width=0.6, height=0.3]\n      \"new_infections\" [id=\"new_infections\",label=<New infections<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = infection_rate<BR/>* susceptible *<BR/>infected<\/FONT>>, tooltip = \"Flow: New infections\\nName: new_infections\\nEquation: infection_rate * susceptible * infected\\nFrom: Susceptible\\nTo: Infected\"]\n\t\"new_recoveries\" [id=\"new_recoveries\",label=<New recoveries<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = recovery_rate *<BR/>infected<\/FONT>>, tooltip = \"Flow: New recoveries\\nName: new_recoveries\\nEquation: recovery_rate * infected\\nFrom: Infected\\nTo: Recovered\"]\n\n      # Define external cloud nodes\n      \n      \n\n      # Define auxiliary nodes\n      \n      \n\n      # Define constant nodes\n      \n      \n\n      # Define flow edges (stock -> flow_node)\n      edge [style = \"\", arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"susceptible\" -> \"new_infections\"\n\t\"infected\" -> \"new_recoveries\"\n\n      # Define flow edges (flow_node -> stock)\n      edge [style = \"\", arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"new_infections\" -> \"infected\"\n\t\"new_recoveries\" -> \"recovered\"\n\n      # Define dependency edges\n      edge [style = \"\", color=\"#999999\", arrowsize=0.8, penwidth=1, constraint=false, tailport = \"_\", headport=\"_\"]\n      \"susceptible\" -> \"new_infections\" [headport = \"s\", tailport = \"s\"]\n\t\"infected\" -> \"new_infections\" [headport = \"n\", tailport = \"n\"]\n\t\"infected\" -> \"new_recoveries\" [headport = \"s\", tailport = \"s\"]\n\n      \n\n\n      # Rank groupings\n      \n      {rank=same; \"susceptible\"; \"recovered\" }\n\n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# Order variables along the flow direction (soft hint via invisible edges)
plot(sfm, order = c("susceptible", "infected", "recovered"))

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=0.1, nodesep=0.3, splines = true, concentrate = false]\n\n      # Shared across all nodes (persists until overridden)\n      node [fontsize=18,fontname=\"Times New Roman\",fontcolor=\"black\"]\n\n      # Define stock nodes\n      node [shape=box,style=filled,fillcolor=\"#83d3d4\"]\n      \"infected\" [id=\"infected\",label=<Infected<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 1<\/FONT>>, tooltip = \"Stock: Infected\\nName: infected\\nInitial value: 1\\nInflows: New infections\\nOutflows: New recoveries\"]\n\t\"recovered\" [id=\"recovered\",label=<Recovered<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 0<\/FONT>>, tooltip = \"Stock: Recovered\\nName: recovered\\nInitial value: 0\\nInflows: New recoveries\\nOutflows: —\"]\n\t\"susceptible\" [id=\"susceptible\",label=<Susceptible<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = 99999<\/FONT>>, tooltip = \"Stock: Susceptible\\nName: susceptible\\nInitial value: 99999\\nInflows: —\\nOutflows: New infections\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      node [style = \"\",shape=plaintext, fontsize=16, width=0.6, height=0.3]\n      \"new_infections\" [id=\"new_infections\",label=<New infections<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = infection_rate<BR/>* susceptible *<BR/>infected<\/FONT>>, tooltip = \"Flow: New infections\\nName: new_infections\\nEquation: infection_rate * susceptible * infected\\nFrom: Susceptible\\nTo: Infected\"]\n\t\"new_recoveries\" [id=\"new_recoveries\",label=<New recoveries<BR/><FONT POINT-SIZE=\"13\" COLOR=\"black\">eqn = recovery_rate *<BR/>infected<\/FONT>>, tooltip = \"Flow: New recoveries\\nName: new_recoveries\\nEquation: recovery_rate * infected\\nFrom: Infected\\nTo: Recovered\"]\n\n      # Define external cloud nodes\n      \n      \n\n      # Define auxiliary nodes\n      \n      \n\n      # Define constant nodes\n      \n      \n\n      # Define flow edges (stock -> flow_node)\n      edge [style = \"\", arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"susceptible\" -> \"new_infections\"\n\t\"infected\" -> \"new_recoveries\"\n\n      # Define flow edges (flow_node -> stock)\n      edge [style = \"\", arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=1, tailport=\"e\", headport=\"w\"]\n      \"new_infections\" -> \"infected\"\n\t\"new_recoveries\" -> \"recovered\"\n\n      # Define dependency edges\n      edge [style = \"\", color=\"#999999\", arrowsize=0.8, penwidth=1, constraint=false, tailport = \"_\", headport=\"_\"]\n      \"susceptible\" -> \"new_infections\" [headport = \"s\", tailport = \"s\"]\n\t\"infected\" -> \"new_infections\" [headport = \"n\", tailport = \"n\"]\n\t\"infected\" -> \"new_recoveries\" [headport = \"s\", tailport = \"s\"]\n\n      # Ordering hints (invisible edges)\n      edge [style=invis, constraint=true]\n      \"susceptible\" -> \"infected\"\n      \"infected\" -> \"recovered\"\n\n\n      # Rank groupings\n      \n\n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
```
