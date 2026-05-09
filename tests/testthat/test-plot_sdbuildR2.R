test_that("plot_sdbuildR2() warns on empty model", {
  skip_if_not_installed("qgraph")

  sfm <- sdbuildR()
  expect_warning(plot_sdbuildR2(sfm), "Model contains no variables")
})


test_that("plot_sdbuildR2() returns qgraph object", {
  skip_if_not_installed("qgraph")

  sfm <- sdbuildR("SIR")
  result <- plot_sdbuildR2(sfm)

  expect_true("qgraph" %in% class(result))
})


test_that("plot_sdbuildR2() validates vars argument", {
  skip_if_not_installed("qgraph")

  sfm <- sdbuildR("SIR")

  expect_error(plot_sdbuildR2(sfm, vars = 123), "vars")
  expect_error(plot_sdbuildR2(sfm, vars = character(0)), "length zero")
  expect_error(
    plot_sdbuildR2(sfm, vars = c("Susceptible", "MissingVar")),
    "MissingVar"
  )
})


test_that("plot_sdbuildR2() can hide dependencies", {
  skip_if_not_installed("qgraph")

  sfm <- sdbuildR()
  sfm <- update(sfm, "S", type = "stock")
  sfm <- update(sfm, "aux1", type = "aux", eqn = "S * 2")

  p_no_dep <- plot_sdbuildR2(sfm, show_dependencies = FALSE)
  p_dep <- plot_sdbuildR2(sfm, show_dependencies = TRUE)

  expect_true("qgraph" %in% class(p_no_dep))
  expect_true("qgraph" %in% class(p_dep))
  expect_gte(length(p_dep$Edgelist$from), length(p_no_dep$Edgelist$from))
})
