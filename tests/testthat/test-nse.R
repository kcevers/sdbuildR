# Tests for Non-Standard Evaluation (NSE) support in update() and related functions

# ==============================================================================
# .expr_to_char() helper tests
# ==============================================================================

test_that(".expr_to_char() handles NULL", {
  expect_null(.expr_to_char(NULL))
})

test_that(".expr_to_char() passes through character strings", {
  expect_equal(.expr_to_char("hello"), "hello")
  expect_equal(.expr_to_char(c("a", "b")), c("a", "b"))
})

test_that(".expr_to_char() converts numeric to character", {
  expect_equal(.expr_to_char(100), "100")
  expect_equal(.expr_to_char(0.5), "0.5")
})

test_that(".expr_to_char() converts logical to character", {
  expect_equal(.expr_to_char(TRUE), "TRUE")
})

test_that(".expr_to_char() converts symbols to names", {
  expect_equal(.expr_to_char(quote(population)), "population")
  expect_equal(.expr_to_char(quote(stock)), "stock")
})

test_that(".expr_to_char() handles c() calls", {
  expect_equal(.expr_to_char(quote(c(a, b, c))), c("a", "b", "c"))
  expect_equal(.expr_to_char(quote(c("x", "y"))), c("x", "y"))
  # Mixed: bare symbols and strings
  expect_equal(.expr_to_char(quote(c("x", y))), c("x", "y"))
})

test_that(".expr_to_char() deparses general expressions", {
  result <- .expr_to_char(quote(a * b + 1))
  expect_true(is.character(result))
  expect_equal(length(result), 1)
  # Deparsed form of a * b + 1
  expect_true(grepl("a", result))
  expect_true(grepl("b", result))
})

test_that(".expr_to_char() handles negative numbers", {
  result <- .expr_to_char(quote(-1))
  expect_true(is.character(result))
  expect_true(grepl("-1", result))
})


# ==============================================================================
# update() with NSE
# ==============================================================================

test_that("update() accepts bare symbol for name", {
  sfm <- sdbuildR()
  sfm <- update(sfm, population, type = "stock")
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "population")
})

test_that("update() accepts bare symbol for type", {
  sfm <- sdbuildR()
  sfm <- update(sfm, x, type = stock)
  vars <- as.data.frame(sfm)
  expect_equal(vars[["type"]], "stock")
})

test_that("update() accepts bare symbols for name and type", {
  sfm <- sdbuildR()
  sfm <- update(sfm, x, stock)
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "x")
  expect_equal(vars[["type"]], "stock")
})

test_that("update() accepts bare expression for eqn", {
  sfm <- sdbuildR()
  sfm <- update(sfm, x, stock, eqn = 100)
  sfm <- update(sfm, y, aux, eqn = x * 0.1 + 2)
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "x", "eqn"], "100")
  expect_equal(vars[vars[["name"]] == "y", "eqn"], "x * 0.1 + 2")
})

test_that("update() accepts bare symbols for to/from", {
  sfm <- sdbuildR()
  sfm <- update(sfm, pop, stock, eqn = 100)
  sfm <- update(sfm, births, flow, eqn = pop * 0.1, to = pop)
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "births", "to"], "pop")
})

test_that("update() accepts bare symbol for source", {
  sfm <- sdbuildR()
  sfm <- update(sfm, my_lookup, lookup,
    xpts = c(0, 5, 10), ypts = c(0, 10, 15), source = t
  )
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "my_lookup", "source"], "t")
})

test_that("update() vectorized with bare symbols in c()", {
  sfm <- sdbuildR()
  sfm <- update(sfm, c(a, b), stock, eqn = c(1, 2))
  vars <- as.data.frame(sfm)
  expect_equal(sort(vars[["name"]]), c("a", "b"))
  expect_equal(vars[vars[["name"]] == "a", "eqn"], "1")
  expect_equal(vars[vars[["name"]] == "b", "eqn"], "2")
})

test_that("update() backward compat: strings still work", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "population", "stock", eqn = "100")
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "population")
  expect_equal(vars[["type"]], "stock")
  expect_equal(vars[["eqn"]], "100")
})

test_that("update() backward compat: do.call still works", {
  sfm <- sdbuildR()
  sfm <- do.call(update.sdbuildR, list(object = sfm, name = "x", type = "stock", eqn = "10"))
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "x")
  expect_equal(vars[["eqn"]], "10")
})

test_that("update() with !! injection for name", {
  sfm <- sdbuildR()
  my_name <- "population"
  sfm <- update(sfm, !!my_name, stock, eqn = 100)
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "population")
})

test_that("update() with !! injection for eqn", {
  sfm <- sdbuildR()
  my_eqn <- "a * b + 1"
  sfm <- update(sfm, x, aux, eqn = !!my_eqn)
  vars <- as.data.frame(sfm)
  expect_equal(vars[["eqn"]], "a * b + 1")
})

test_that("update() modifying existing variable with NSE name", {
  sfm <- sdbuildR()
  sfm <- update(sfm, x, stock, eqn = 10)
  sfm <- update(sfm, x, eqn = 20)
  vars <- as.data.frame(sfm)
  expect_equal(vars[["eqn"]], "20")
})

test_that("update() with df argument still works (no NSE interference)", {
  df <- data.frame(
    type = c("stock", "flow"),
    name = c("S", "In_flow"),
    eqn = c("5", "S * 0.1"),
    to = c(NA, "S"),
    stringsAsFactors = FALSE
  )
  sfm <- sdbuildR()
  sfm <- update(sfm, df = df)
  vars <- as.data.frame(sfm)
  expect_equal(nrow(vars), 2)
})


# ==============================================================================
# Wrapper functions with NSE
# ==============================================================================

test_that("stock() accepts bare symbols", {
  sfm <- sdbuildR()
  sfm <- stock(sfm, population, eqn = 100, label = "Population")
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "population")
  expect_equal(vars[["eqn"]], "100")
  expect_equal(vars[["label"]], "Population")
})

test_that("flow() accepts bare symbols for name, to, from", {
  sfm <- sdbuildR()
  sfm <- stock(sfm, pop, eqn = 100)
  sfm <- flow(sfm, births, eqn = pop * 0.1, to = pop)
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "births", "to"], "pop")
  # eqn deparsed from expression
  expect_equal(vars[vars[["name"]] == "births", "eqn"], "pop * 0.1")
})

test_that("constant() accepts bare symbols", {
  sfm <- sdbuildR()
  sfm <- constant(sfm, growth_rate, eqn = 0.1)
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "growth_rate")
  expect_equal(vars[["eqn"]], "0.1")
})

test_that("auxiliary() / aux() accepts bare symbols", {
  sfm <- sdbuildR() |>
    stock("pop", eqn = "100") |>
    constant("K", eqn = "1000")
  sfm <- auxiliary(sfm, density, eqn = pop / K)
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "density", "eqn"], "pop / K")
})

test_that("lookup() accepts bare symbols", {
  sfm <- sdbuildR()
  sfm <- lookup(sfm, my_gf,
    xpts = c(0, 5, 10), ypts = c(0, 10, 15),
    source = t
  )
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "my_gf")
  expect_equal(vars[["source"]], "t")
})


# ==============================================================================
# discard() with NSE
# ==============================================================================

test_that("discard() accepts bare symbol for name", {
  sfm <- sdbuildR() |>
    update("x", "stock", eqn = "1")
  sfm <- discard(sfm, x)
  vars <- as.data.frame(sfm)
  expect_equal(nrow(vars), 0)
})

test_that("discard() with c() of bare symbols", {
  sfm <- sdbuildR() |>
    update("a", "stock") |>
    update("b", "stock")
  sfm <- discard(sfm, c(a, b))
  vars <- as.data.frame(sfm)
  expect_equal(nrow(vars), 0)
})


# ==============================================================================
# change_name() with NSE
# ==============================================================================

test_that("change_name() accepts bare symbols", {
  sfm <- sdbuildR() |>
    update("old_name", "stock")
  sfm <- change_name(sfm, old_name, new_name = new_name_var)
  vars <- as.data.frame(sfm)
  expect_equal(vars[["name"]], "new_name_var")
})


# ==============================================================================
# change_type() with NSE
# ==============================================================================

test_that("change_type() accepts bare symbols", {
  sfm <- sdbuildR() |>
    update("delta", "constant", eqn = "0.025")
  sfm <- change_type(sfm, delta, new_type = aux)
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "delta", "type"], "aux")
})


# ==============================================================================
# Pipe chains with full NSE
# ==============================================================================

test_that("full pipe chain with NSE works end-to-end", {
  sfm <- sdbuildR() |>
    stock(prey, eqn = 50) |>
    stock(predator, eqn = 10) |>
    flow(prey_births, eqn = alpha * prey, to = prey) |>
    flow(prey_deaths, eqn = beta * prey * predator, from = prey) |>
    constant(alpha, eqn = 0.5) |>
    constant(beta, eqn = 0.05)

  vars <- as.data.frame(sfm)
  expect_equal(nrow(vars), 6)
  expect_true("prey" %in% vars[["name"]])
  expect_true("predator" %in% vars[["name"]])
  expect_equal(vars[vars[["name"]] == "prey_births", "to"], "prey")
  expect_equal(vars[vars[["name"]] == "prey_births", "eqn"], "alpha * prey")
})


# ==============================================================================
# sdbuildR() template NSE tests
# ==============================================================================

test_that("sdbuildR() accepts bare symbol for template", {
  expect_s3_class(sdbuildR(SIR), "sdbuildR")
})

test_that("sdbuildR() accepts quoted string for template (backward compat)", {
  expect_s3_class(sdbuildR("SIR"), "sdbuildR")
})

test_that("sdbuildR() with no template still returns empty model", {
  sfm <- sdbuildR()
  expect_s3_class(sfm, "sdbuildR")
  expect_equal(nrow(sfm[["variables"]]), 0L)
})
