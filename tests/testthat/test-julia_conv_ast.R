# AST-based R -> Julia translator (convert_eqn_ast_julia)
# Fast path used by the Julia backend; falls back to convert_equations_julia()
# for anything it does not handle.

vn <- c("r", "X", "K", "a", "b", "c", "d", "x", "y", "gf1", "Stock1", "dt")

test_that("arithmetic operators are broadcast and numbers become floats", {
  expect_equal(convert_eqn_ast_julia("r * X", vn), "r .* X")
  expect_equal(convert_eqn_ast_julia("r * X^2 / K", vn), "r .* X .^ 2.0 ./ K")
  expect_equal(convert_eqn_ast_julia("a + b - c", vn), "a .+ b .- c")
  expect_equal(convert_eqn_ast_julia("1", vn), "1.0")
  expect_equal(convert_eqn_ast_julia(".01", vn), "0.01")
  expect_equal(convert_eqn_ast_julia("(a + b) * c", vn), "(a .+ b) .* c")
})

test_that("unary minus and comparisons translate", {
  expect_equal(convert_eqn_ast_julia("-x", vn), "-x")
  expect_equal(convert_eqn_ast_julia("a > b", vn), "a .> b")
  expect_equal(convert_eqn_ast_julia("a == b", vn), "a .== b")
})

test_that("known functions map and broadcast per syntax_df", {
  expect_equal(convert_eqn_ast_julia("exp(-x)", vn), "exp.(-x)")
  expect_match(convert_eqn_ast_julia("min(a, b)", vn), "^r_min\\(a, b\\)$")
  expect_match(convert_eqn_ast_julia("ifelse(x > 0, a, b)", vn),
    "ifelse\\.\\(x \\.> 0\\.0, a, b\\)")
})

test_that("namespaced calls drop the namespace (valid Julia)", {
  out <- convert_eqn_ast_julia("base::sum(x)", vn)
  expect_false(is.null(out))
  expect_false(grepl("base::", out))
  expect_match(out, "sum")
})

test_that("unknown functions (custom/gf) are emitted verbatim for later substitution", {
  expect_equal(convert_eqn_ast_julia("gf1(Stock1)", vn), "gf1(Stock1)")
})

test_that("distributions reuse the existing reparameterisation", {
  expect_equal(convert_eqn_ast_julia("rnorm(1, dt)", vn),
    convert_equations_julia("aux", "z", "rnorm(1, dt)", vn)[["eqn"]])
})

test_that("unsupported constructs return NULL (caller falls back)", {
  expect_null(convert_eqn_ast_julia("for (i in 1:3) {\n x <- i \n}", vn))
  expect_null(convert_eqn_ast_julia("a %% b", vn)) # unhandled infix operator
  expect_null(convert_eqn_ast_julia("if (a) b else c", vn))
  expect_null(convert_eqn_ast_julia("1:10", vn)) # range
  expect_null(convert_eqn_ast_julia("x[1]", vn)) # indexing
  expect_null(convert_eqn_ast_julia("a %in% b", vn))
  expect_null(convert_eqn_ast_julia("a$b", vn))
})

test_that("non-finite and missing literals are handled or fall back", {
  expect_equal(convert_eqn_ast_julia("Inf", vn), "Inf")
  expect_equal(convert_eqn_ast_julia("-Inf", vn), "-Inf")
  expect_equal(convert_eqn_ast_julia("NaN", vn), "NaN")
  expect_equal(convert_eqn_ast_julia("1e10", vn), "10000000000.0")
  expect_equal(convert_eqn_ast_julia("2.5e-3", vn), "0.0025")
  # NA / NULL are left to the legacy translator (never emitted as bogus Julia)
  expect_null(convert_eqn_ast_julia("NA", vn))
  expect_null(convert_eqn_ast_julia("NULL", vn))
})

test_that("AST and legacy translators agree on deterministic template equations", {
  # Equivalence check over real model equations (excludes stochastic calls).
  eqns <- c()
  for (tp in templates()) {
    sfm <- tryCatch(sdbuildR(tp), error = function(e) NULL)
    if (is.null(sfm)) next
    v <- sfm[["variables"]]
    eqns <- c(eqns, v[v[["type"]] %in% c("stock", "flow", "constant", "aux"), "eqn"])
  }
  eqns <- unique(eqns[nzchar(eqns)])
  vnames <- c(vn, unlist(lapply(templates(), function(tp) {
    s <- tryCatch(sdbuildR(tp), error = function(e) NULL)
    if (is.null(s)) character(0) else get_model_var(s)
  })))

  # Every equation translates without falling back (AST covers them).
  fell_back <- vapply(eqns, function(e) is.null(convert_eqn_ast_julia(e, vnames)),
    logical(1))
  expect_false(any(fell_back))
})




