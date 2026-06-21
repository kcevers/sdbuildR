# ============================================================================
# BASIC FUNCTIONALITY TESTS
# ============================================================================

test_that("plot() warns on empty model", {
  sfm <- stockflow()

  expect_warning(plot(sfm), "Model contains no variables")
})

test_that("plot() method exists for stockflow objects", {
  # Check that plot method exists
  expect_true("plot.stockflow" %in% methods("plot"))
})

test_that("plot() returns DiagrammeR grViz object", {
  sfm <- stockflow("SIR")

  result <- plot(sfm)

  # Should return an htmlwidget (DiagrammeR graph)
  expect_true("grViz" %in% class(result))
  expect_true("htmlwidget" %in% class(result))
})

# ============================================================================
# PARAMETER VALIDATION TESTS
# ============================================================================

test_that("plot() checks vars argument", {
  sfm <- stockflow("SIR")

  expect_error(
    plot(sfm, vars = 123),
    "vars"
  )
  expect_error(
    plot(sfm, vars = character(0)),
    "Invalid"
  )
  expect_error(
    plot(sfm, vars = c("susceptible", "NonExistentVar")),
    "NonExistentVar.*not.*variable"
  )
})

# ============================================================================
# VISUAL REGRESSION TESTS (expect_snapshot_plot)
# ============================================================================

test_that("plot() creates diagram for SIR template", {
  sfm <- stockflow("SIR")
  pl <- plot(sfm, show_aux = FALSE, show_constants = FALSE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  df <- as.data.frame(sfm, properties = "eqn")
  var_names <- df$name[df$type %in% c("stock", "flow")]
  expect_setequal(nodes$name, var_names)
  expect_true(all(edges$from %in% var_names))
  expect_true(all(edges$to %in% var_names))
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  expect_snapshot_plot(
    "stockflow-SIR-model-diagram",
    pl
  )
})

test_that("plot() creates diagram for simple single-stock model", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock", label = "Population")
  sfm2 <- update(sfm1, "Flow1", type = "flow", label = "Birth", from = "Stock1")
  pl <- plot(sfm2, show_aux = FALSE, show_constants = FALSE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  var_names <- c("Stock1", "Flow1", "Cloud1")
  expect_setequal(nodes$name, var_names)
  expect_true(all(edges$from %in% var_names))
  expect_true(all(edges$to %in% var_names))
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  expect_snapshot_plot("stockflow-simple-stock-flow", pl)
})

test_that("plot() creates diagram with auxiliary variables and dependencies", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "S", type = "stock")
  sfm2 <- update(sfm1, "I", type = "stock")
  sfm3 <- update(sfm2, "infection_rate", type = "aux", eqn = "S * I * 0.001")

  pl <- plot(sfm3, show_aux = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  var_names <- c("S", "I", "infection_rate")
  expect_setequal(nodes$name, var_names)
  expect_true(all(edges$from %in% var_names))
  expect_true(all(edges$to %in% var_names))
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  expect_snapshot_plot("stockflow-diagram-with-dependencies", pl)
})

test_that("plot() with show_dependencies = FALSE hides dependency arrows", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "S", type = "stock")
  sfm2 <- update(sfm1, "aux1", type = "aux", eqn = "S * 2")

  pl <- plot(sfm2, show_dependencies = FALSE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  var_names <- c("S", "aux1")
  expect_setequal(nodes$name, var_names)
  expect_true(length(unique(nodes$id)) == nrow(nodes))
  expect_true(nrow(edges) == 0)

  expect_snapshot_plot("stockflow-no-dependencies", pl)
})

test_that("plot() with show_constants = TRUE displays constants", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- update(sfm1, "const1", type = "constant", eqn = "5")

  pl <- plot(sfm2, show_constants = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  var_names <- c("Stock1", "const1")
  expect_setequal(nodes$name, var_names)
  expect_true(nrow(edges) == 0)

  expect_snapshot_plot("stockflow-with-constants", pl)
})

test_that("plot() with show_constants = FALSE hides constants", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- update(sfm1, "const1", type = "constant", eqn = "5")
  pl <- plot(sfm2, show_constants = FALSE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  var_names <- c("Stock1")
  expect_setequal(nodes$name, var_names)
  expect_true(nrow(edges) == 0)
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  expect_snapshot_plot("stockflow-without-constants", pl)
})

test_that("plot() with show_aux = FALSE hides auxiliary variables", {
  sfm <- stockflow("SIR")
  pl <- plot(sfm, show_aux = FALSE, show_constants = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  df <- as.data.frame(sfm, properties = "eqn")
  var_names <- df$name[df$type %in% c("stock", "flow", "constant")]
  expect_setequal(nodes$name, var_names)
  expect_true(all(edges$from %in% var_names))
  expect_true(all(edges$to %in% var_names))
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  expect_snapshot_plot("stockflow-no-auxiliaries", pl)
})

test_that("plot() filters variables correctly", {
  sfm <- stockflow("SIR")
  var_names <- c("susceptible", "infected")
  pl <- plot(sfm, vars = var_names, show_aux = TRUE, show_constants = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  expect_setequal(nodes$name, var_names)
  expect_true(all(edges$from %in% var_names))
  expect_true(all(edges$to %in% var_names))
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  pl_filtered <- pl

  var_names <- c("susceptible")
  pl <- plot(sfm, vars = var_names, show_aux = TRUE, show_constants = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  expect_setequal(nodes$name, var_names)
  expect_true(length(unique(nodes$id)) == nrow(nodes))
  expect_true(nrow(edges) == 0)

  expect_snapshot_plot(
    c("stockflow-filtered-variables", "stockflow-single-variable-filter"),
    list(pl_filtered, pl)
  )
})

test_that("plot() applies custom stock color", {
  sfm <- stockflow("SIR")
  stock_color <- "#FF6B6B"
  df <- as.data.frame(sfm, properties = "type")
  stock_names <- df$name[df$type == "stock"]
  pl <- plot(sfm, stock_col = stock_color)
  nodes <- extract_diagram_nodes(pl)
  stock_nodes <- nodes[nodes$name %in% stock_names, ]
  expect_true(all(stock_nodes$shape == "rectangle"))
  expect_true(all(stock_nodes$color == stock_color))

  expect_snapshot_plot("stockflow-custom-stock-color", pl)
})

test_that("plot() applies custom flow color", {
  sfm <- stockflow("SIR")
  flow_color <- "#4ECDC4"
  df <- as.data.frame(sfm, properties = "type")
  flow_names <- df$name[df$type == "flow"]
  pl <- plot(sfm, flow_col = flow_color)
  nodes <- extract_diagram_nodes(pl)
  flow_nodes <- nodes[nodes$name %in% flow_names, ]
  expect_true(all(flow_nodes$color == flow_color))

  expect_snapshot_plot("stockflow-custom-flow-color", pl)
})

test_that("plot() applies custom dependency color", {
  sfm <- stockflow("SIR")
  dependency_color <- "#FFE66D"
  df <- as.data.frame(sfm, properties = "type")
  pl <- plot(sfm, dependency_col = dependency_color, show_dependencies = TRUE)
  edges <- extract_diagram_edges(pl)
  # dependency_edges <- edges[edges$rel == "dependency", ]
  # expect_true(all(dependency_edges$color == dependency_color))

  expect_snapshot_plot("stockflow-custom-dependency-color", pl)
})

test_that("plot() with custom font size", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock1", type = "stock", label = "Population")
  font_size <- 12
  pl <- plot(sfm1, font_size = font_size)
  nodes <- extract_diagram_nodes(pl)
  expect_true(all(nodes$font.size == font_size))

  expect_snapshot_plot("stockflow-large-font", pl)
})

test_that("plot.stockflow() with custom wrap width", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "VeryLongStockNameThatShouldWrap",
    type = "stock", label = "Very Long Stock Name That Should Wrap"
  )

  pl <- plot(sfm1, wrap_width = 10, show_eqn = FALSE)
  nodes <- extract_diagram_nodes(pl)
  expect_true(grepl("\n", nodes$label, fixed = TRUE)) # check that label contains a newline (indicating wrapping)

  expect_snapshot_plot("stockflow-wrap-width-small", pl)
})

test_that("plot.stockflow() with format_label = FALSE preserves original labels", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock_1", type = "stock", label = "Stock_1")

  pl <- plot(sfm1, format_label = FALSE, show_eqn = FALSE)
  nodes <- extract_diagram_nodes(pl)
  expect_true(nodes$label == "Stock_1")
  expect_snapshot_plot("stockflow-format-label-false", pl)
})

test_that("plot.stockflow() with format_label = TRUE removes underscores", {
  sfm <- stockflow()
  sfm1 <- update(sfm, "Stock_1", type = "stock", label = "Stock_1")

  pl <- plot(sfm1, format_label = TRUE, show_eqn = FALSE)
  nodes <- extract_diagram_nodes(pl)
  expect_true(nodes$label == "Stock 1")
  expect_snapshot_plot("stockflow-format-label-true", pl)
})

# ============================================================================
# show_eqn AND label_col TESTS
# ============================================================================

test_that("plot() with show_eqn = TRUE (default) shows equations beneath labels", {
  sfm <- stockflow("SIR")
  pl <- plot(sfm, show_constants = TRUE)
  d <- pl[["x"]][["diagram"]]

  # Equations are rendered as a smaller FONT line, prefixed with "eqn = ".
  expect_true(grepl("FONT POINT-SIZE", d, fixed = TRUE))
  expect_true(grepl("eqn = ", d, fixed = TRUE))
  # HTML-like labels (label=< ... >) are used when show_eqn = TRUE.
  expect_true(grepl("label=<", d, fixed = TRUE))

  expect_snapshot_plot("stockflow-show-eqn", pl)
})

test_that("plot() with show_eqn = FALSE does not show equations in labels", {
  sfm <- stockflow("SIR")
  pl <- plot(sfm, show_eqn = FALSE)
  d <- pl[["x"]][["diagram"]]

  expect_false(grepl("FONT POINT-SIZE", d, fixed = TRUE))
  expect_false(grepl("label=<", d, fixed = TRUE))
})

test_that("plot.stockflow() show_tooltip = TRUE (default) adds equation tooltips", {
  sfm <- stockflow("SIR")
  pl <- plot(sfm, show_constants = TRUE, show_tooltip = TRUE)
  d <- pl[["x"]][["diagram"]]

  expect_true(grepl('tooltip', d, fixed = TRUE))
  expect_snapshot_plot("stockflow-tooltip", pl)
})

test_that("plot.stockflow() with show_tooltip = FALSE omits tooltips", {
  sfm <- stockflow("SIR")
  pl <- plot(sfm, show_tooltip = FALSE, show_constants = TRUE)
  d <- pl[["x"]][["diagram"]]

  expect_false(grepl("tooltip", d, fixed = TRUE))
  expect_snapshot_plot("stockflow-no-tooltip", pl)
})

test_that("plot.stockflow() validates show_tooltip", {
  sfm <- stockflow("SIR")
  expect_error(plot(sfm, show_tooltip = "yes"), "show_tooltip")
  expect_error(plot(sfm, show_tooltip = NA), "show_tooltip")
})

test_that("plot.stockflow() tooltips describe type, name, equation, and structure", {
  sfm <- stockflow("SIR")
  pl <- plot(sfm, show_constants = TRUE, show_tooltip = TRUE)
  nodes <- extract_diagram_nodes(pl)

  stock <- nodes[nodes$name == "susceptible", ]
  expect_true(grepl("Stock: Susceptible", stock$tooltip, fixed = TRUE))
  expect_true(grepl("Initial value: 99999", stock$tooltip, fixed = TRUE))
  expect_true(grepl("Outflows: New infections", stock$tooltip, fixed = TRUE))

  flow <- nodes[nodes$name == "new_infections", ]
  expect_true(grepl("Flow: New infections", flow$tooltip, fixed = TRUE))
  expect_true(grepl("From: Susceptible", flow$tooltip, fixed = TRUE))
  expect_true(grepl("To: Infected", flow$tooltip, fixed = TRUE))

  const <- nodes[nodes$name == "recovery_rate", ]
  expect_true(grepl("Constant: Recovery rate", const$tooltip, fixed = TRUE))
  expect_true(grepl("Value: 0.1", const$tooltip, fixed = TRUE))
})

test_that("plot.stockflow() omits the Name line when name equals the label", {
  sfm <- stockflow()
  sfm <- update(sfm, "S", type = "stock")
  pl <- plot(sfm, show_tooltip = TRUE)
  nodes <- extract_diagram_nodes(pl)
  stock <- nodes[nodes$name == "S", ]

  expect_true(grepl("Stock: S", stock$tooltip, fixed = TRUE))
  expect_false(grepl("Name:", stock$tooltip, fixed = TRUE))
})

test_that("plot.stockflow() cloud tooltips state they are outside the model boundary", {
  sfm <- stockflow()
  sfm <- update(sfm, "Population", type = "stock")
  sfm <- update(sfm, "births", type = "flow", to = "Population")
  pl <- plot(sfm, show_tooltip = TRUE)
  nodes <- extract_diagram_nodes(pl)
  cloud <- nodes[grepl("Cloud", nodes$name), ]

  expect_true(nrow(cloud) >= 1)
  expect_true(any(grepl("Outside model boundary", cloud$tooltip, fixed = TRUE)))
  # A flow that enters the model from outside makes the cloud its source.
  expect_true(any(grepl("Source of: births", cloud$tooltip, fixed = TRUE)))
})

test_that("plot() show_eqn uses label_col for the equation text", {
  sfm <- stockflow("SIR")
  label_color <- "#123456"
  pl <- plot(sfm, show_eqn = TRUE, label_col = label_color)
  d <- pl[["x"]][["diagram"]]

  # Equation FONT colour and node fontcolor both use label_col.
  expect_true(grepl(paste0("COLOR=\"", label_color, "\""), d, fixed = TRUE))
  expect_true(grepl(label_color, d, fixed = TRUE))

  expect_snapshot_plot("stockflow-show-eqn-label-col", pl)
})

test_that("plot() show_eqn wraps long equations to wrap_width", {
  sfm <- stockflow("SIR")
  pl <- plot(sfm, show_eqn = TRUE, wrap_width = 8)
  d <- pl[["x"]][["diagram"]]

  # A long equation wrapped to a narrow width contains a line break.
  expect_true(grepl("eqn =.*<BR/>", d))
})

test_that("plot() validates show_eqn", {
  sfm <- stockflow("SIR")
  expect_error(plot(sfm, show_eqn = "yes"), "show_eqn")
  expect_error(plot(sfm, show_eqn = NA), "show_eqn")
})

test_that("plot() applies label_col to node fontcolor", {
  sfm <- stockflow("SIR")
  label_color <- "#654321"
  pl <- plot(sfm, label_col = label_color)
  d <- pl[["x"]][["diagram"]]

  expect_true(grepl(label_color, d, fixed = TRUE))

  expect_snapshot_plot("stockflow-label-col", pl)
})
