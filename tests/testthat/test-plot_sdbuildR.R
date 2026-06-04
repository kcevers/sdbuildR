# ============================================================================
# BASIC FUNCTIONALITY TESTS
# ============================================================================

test_that("plot() warns on empty model", {
  sfm <- sdbuildR()

  expect_warning(plot(sfm), "Model contains no variables")
})

test_that("plot() method exists for sdbuildR objects", {
  # Check that plot method exists
  expect_true("plot.sdbuildR" %in% methods("plot"))
})

test_that("plot() returns DiagrammeR grViz object", {
  sfm <- sdbuildR("SIR")

  result <- plot(sfm)

  # Should return an htmlwidget (DiagrammeR graph)
  expect_true("grViz" %in% class(result))
  expect_true("htmlwidget" %in% class(result))
})

# ============================================================================
# PARAMETER VALIDATION TESTS
# ============================================================================

test_that("plot() checks vars argument", {
  sfm <- sdbuildR("SIR")

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
  sfm <- sdbuildR("SIR")
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
    "sdbuildR-SIR-model-diagram",
    pl
  )
})

test_that("plot() creates diagram for simple single-stock model", {
  sfm <- sdbuildR()
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

  expect_snapshot_plot("sdbuildR-simple-stock-flow", pl)
})

test_that("plot() creates diagram with auxiliary variables and dependencies", {
  sfm <- sdbuildR()
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

  expect_snapshot_plot("sdbuildR-diagram-with-dependencies", pl)
})

test_that("plot() with show_dependencies = FALSE hides dependency arrows", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "S", type = "stock")
  sfm2 <- update(sfm1, "aux1", type = "aux", eqn = "S * 2")

  pl <- plot(sfm2, show_dependencies = FALSE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  var_names <- c("S", "aux1")
  expect_setequal(nodes$name, var_names)
  expect_true(length(unique(nodes$id)) == nrow(nodes))
  expect_true(nrow(edges) == 0)

  expect_snapshot_plot("sdbuildR-no-dependencies", pl)
})

test_that("plot() with show_constants = TRUE displays constants", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- update(sfm1, "const1", type = "constant", eqn = "5")

  pl <- plot(sfm2, show_constants = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  var_names <- c("Stock1", "const1")
  expect_setequal(nodes$name, var_names)
  expect_true(nrow(edges) == 0)

  expect_snapshot_plot("sdbuildR-with-constants", pl)
})

test_that("plot() with show_constants = FALSE hides constants", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- update(sfm1, "const1", type = "constant", eqn = "5")
  pl <- plot(sfm2, show_constants = FALSE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  var_names <- c("Stock1")
  expect_setequal(nodes$name, var_names)
  expect_true(nrow(edges) == 0)
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  expect_snapshot_plot("sdbuildR-without-constants", pl)
})

test_that("plot() with show_aux = FALSE hides auxiliary variables", {
  sfm <- sdbuildR("SIR")
  pl <- plot(sfm, show_aux = FALSE, show_constants = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  df <- as.data.frame(sfm, properties = "eqn")
  var_names <- df$name[df$type %in% c("stock", "flow", "constant")]
  expect_setequal(nodes$name, var_names)
  expect_true(all(edges$from %in% var_names))
  expect_true(all(edges$to %in% var_names))
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  expect_snapshot_plot("sdbuildR-no-auxiliaries", pl)
})

test_that("plot() filters variables correctly", {
  sfm <- sdbuildR("SIR")
  var_names <- c("susceptible", "infected")
  pl <- plot(sfm, vars = var_names, show_aux = TRUE, show_constants = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  expect_setequal(nodes$name, var_names)
  expect_true(all(edges$from %in% var_names))
  expect_true(all(edges$to %in% var_names))
  expect_true(length(unique(nodes$id)) == nrow(nodes))

  expect_snapshot_plot(
    "sdbuildR-filtered-variables",
    pl
  )
  
  var_names <- c("susceptible")
  pl <- plot(sfm, vars = var_names, show_aux = TRUE, show_constants = TRUE)
  nodes <- extract_diagram_nodes(pl)
  edges <- extract_diagram_edges(pl)
  expect_setequal(nodes$name, var_names)
  expect_true(length(unique(nodes$id)) == nrow(nodes))
  expect_true(nrow(edges) == 0)

  expect_snapshot_plot(
    "sdbuildR-single-variable-filter",
    pl
  )
})

test_that("plot() applies custom stock color", {
  sfm <- sdbuildR("SIR")
  stock_color <- "#FF6B6B"
  df <- as.data.frame(sfm, properties = "type")
  stock_names <- df$name[df$type == "stock"]
  pl <- plot(sfm, stock_col = stock_color)
  nodes <- extract_diagram_nodes(pl)
  stock_nodes <- nodes[nodes$name %in% stock_names, ]
  expect_true(all(stock_nodes$shape == "rectangle"))
  expect_true(all(stock_nodes$color == stock_color))

  expect_snapshot_plot("sdbuildR-custom-stock-color", pl)
})

test_that("plot() applies custom flow color", {
  sfm <- sdbuildR("SIR")
  flow_color <- "#4ECDC4"
  df <- as.data.frame(sfm, properties = "type")
  flow_names <- df$name[df$type == "flow"]
  pl <- plot(sfm, flow_col = flow_color)
  nodes <- extract_diagram_nodes(pl)
  flow_nodes <- nodes[nodes$name %in% flow_names, ]
  expect_true(all(flow_nodes$color == flow_color))

  expect_snapshot_plot("sdbuildR-custom-flow-color", pl)
})

test_that("plot() applies custom dependency color", {
  sfm <- sdbuildR("SIR")
  dependency_color <- "#FFE66D"
  df <- as.data.frame(sfm, properties = "type")
  pl <- plot(sfm, dependency_col = dependency_color, show_dependencies = TRUE)
  edges <- extract_diagram_edges(pl)
  # dependency_edges <- edges[edges$rel == "dependency", ]
  # expect_true(all(dependency_edges$color == dependency_color))

  expect_snapshot_plot("sdbuildR-custom-dependency-color", pl)
})

test_that("plot() with custom font size", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock", label = "Population")
  font_size <- 12
  pl <- plot(sfm1, font_size = font_size)
  nodes <- extract_diagram_nodes(pl)
  expect_true(all(nodes$font.size == font_size))

  expect_snapshot_plot("sdbuildR-large-font", pl)
})

test_that("plot.sdbuildR() with custom wrap width", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "VeryLongStockNameThatShouldWrap",
    type = "stock", label = "Very Long Stock Name That Should Wrap"
  )

  pl <- plot(sfm1, wrap_width = 10)
  nodes <- extract_diagram_nodes(pl)
  expect_true(grepl("\n", nodes$label, fixed = TRUE))  # check that label contains a newline (indicating wrapping)
  
  expect_snapshot_plot("sdbuildR-wrap-width-small", pl)
})

test_that("plot.sdbuildR() with format_label = FALSE preserves original labels", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock_1", type = "stock", label = "Stock_1")

  pl <- plot(sfm1, format_label = FALSE)
  nodes <- extract_diagram_nodes(pl)
  expect_true(nodes$label == "Stock_1")
  expect_snapshot_plot("sdbuildR-format-label-false", pl)
})

test_that("plot.sdbuildR() with format_label = TRUE removes underscores", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock_1", type = "stock", label = "Stock_1")

  pl <- plot(sfm1, format_label = TRUE)
  nodes <- extract_diagram_nodes(pl)
  expect_true(nodes$label == "Stock 1")
  expect_snapshot_plot("sdbuildR-format-label-true", pl)
})
