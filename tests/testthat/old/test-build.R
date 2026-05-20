test_that("sdbuildR() works", {
  expect_equal(class(sdbuildR()), "sdbuildR")
  expect_s3_class(sdbuildR(), "sdbuildR")

  # Non existing template
  expect_error(sdbuildR("A"), "A is not an available template. The available templates are")
  expect_error(sdbuildR(""), "is not an available template. The available templates are")
  expect_error(sdbuildR(" "), "  is not an available template. The available templates are")
})

test_that("update() with only sfm does nothing", {
  sfm <- sdbuildR() |> update("a", "stock")
  sfm2 <- expect_no_error(expect_no_message(update(sfm, "a")))
  expect_equal(sfm, sfm2)
})


test_that("type in update()", {
  sfm <- sdbuildR()
  expect_error(update(sfm, "a", "stockss"), "type needs to be one of 'stock', 'flow', 'constant', 'aux', or 'gf'")

  sfm1 <- update(sfm, "a", "stocks")
  expect_equal(as.data.frame(sfm1)[["name"]], "a")

  sfm1 <- update(sfm, "a", "flows")
  expect_equal(as.data.frame(sfm1)[["name"]], "a")

  sfm1 <- update(sfm, "a", "auxiliary")
  expect_equal(as.data.frame(sfm1)[["name"]], "a")
})


test_that("modify to and from", {
  sfm <- sdbuildR("SIR")

  sfm2 <- expect_silent(update(sfm, "Infection_Rate", from = ""))
  df <- sfm2[["variables"]]
  expect_equal(df[df[["name"]] == "Infection_Rate", "from"], "")

  sfm2 <- expect_silent(update(sfm, "Infection_Rate", from = NULL))
  df <- sfm2[["variables"]]
  expect_equal(df[df[["name"]] == "Infection_Rate", "from"], "")

  sfm2 <- expect_silent(update(sfm, "Infection_Rate", from = NA))
  df <- sfm2[["variables"]]
  expect_equal(df[df[["name"]] == "Infection_Rate", "from"], "")

  sfm2 <- expect_silent(update(sfm, "Recovery_Rate", to = ""))
  df <- sfm2[["variables"]]
  expect_equal(df[df[["name"]] == "Recovery_Rate", "to"], "")

  sfm2 <- expect_silent(update(sfm, "Recovery_Rate", to = NULL))
  df <- sfm2[["variables"]]
  expect_equal(df[df[["name"]] == "Recovery_Rate", "to"], "")

  sfm2 <- expect_silent(update(sfm, "Recovery_Rate", to = NA))
  df <- sfm2[["variables"]]
  expect_equal(df[df[["name"]] == "Recovery_Rate", "to"], "")
})


test_that("sim_specs() works", {
  # Ensure that default simulation specifications are with digits
  sfm <- sdbuildR()
  expect_equal(sfm$sim_specs$start, "0")
  expect_equal(sfm$sim_specs$stop, "100.0")
  expect_equal(sfm$sim_specs$dt, "0.01")
  expect_equal(sfm$sim_specs$save_at, "0.01")
  expect_equal(sfm$sim_specs$save_from, sfm$sim_specs$start)

  # Check that empty sim_specs() doesn't change anything
  sfm <- sdbuildR()
  sfm <- sfm |> sim_specs()
  expect_equal(sfm$sim_specs$start, "0")

  # Check that start and stop are set correctly
  sfm <- sdbuildR()
  expect_error(sfm |> sim_specs(start = 2000, stop = 1990), "Start time must be smaller than stop time!")
  expect_error(sfm |> sim_specs(stop = -100), "Start time must be smaller than stop time!")
  expect_error(sfm |> sim_specs(start = 200), "Start time must be smaller than stop time!")

  # Check start, stop, dt - no scientific notation
  expect_equal(sim_specs(sfm, start = 1990, stop = 2000)$sim_specs$start, "1990.0")
  expect_equal(sim_specs(sfm, start = 1, stop = 1e+06)$sim_specs$stop, "1000000.0")
  expect_equal(sim_specs(sfm, dt = 1e-08)$sim_specs$dt, "0.00000001")
  expect_warning(sim_specs(sfm, dt = .1, save_at = .01), "dt must be smaller or equal to save_at! Setting save_at equal to dt")


  # Check that seed must be an integer
  expect_error(sfm |> sim_specs(seed = "a"), "seed must be an integer!")
  expect_error(sfm |> sim_specs(seed = 1.5), "seed must be an integer!")

  # Check that empty seed becomes NULL
  expect_equal(sim_specs(sfm, seed = "")$sim_specs$seed, NULL)

  # Check that removing seed works
  sfm <- sfm |> sim_specs(seed = 1)
  expect_equal(sim_specs(sfm, seed = NULL)$sim_specs$seed, NULL)

  # Check that dt must be smaller than stop - start
  expect_error(
    sdbuildR() |> sim_specs(start = 0, stop = .05, dt = .1),
    "dt must be smaller than the difference between start and stop!"
  )
  expect_error(
    sdbuildR() |> sim_specs(start = 0, stop = 1, save_at = 2),
    "save_at must be smaller than the difference between start and stop!"
  )

  # save_at and dt
  expect_no_error(sdbuildR() |> sim_specs(dt = .1))
  sfm <- sdbuildR() |> sim_specs(dt = 0.1)
  expect_equal(sfm$sim_specs$dt, "0.1")
  expect_equal(sfm$sim_specs$save_at, "0.1")

  # save_from
  sfm <- sdbuildR() |> sim_specs(start = 0, stop = 100, dt = .01, save_at = .1)
  expect_error(
    sfm |> sim_specs(save_from = -1),
    "save_from must be within the start \\(0\\) and stop \\(100\\) time of the simulation"
  )
  expect_error(
    sfm |> sim_specs(save_from = 101),
    "save_from must be within the start \\(0\\) and stop \\(100\\) time of the simulation"
  )
  sfm <- expect_no_error(sfm |> sim_specs(save_from = 10))
  expect_equal(sfm$sim_specs$save_from, "10.0")

  # Check that save_at is updated if start and stop are updated
  sfm <- sdbuildR("SIR") |>
    sim_specs(start = 0, stop = 20) |>
    sim_specs(save_at = 0.1, dt = 0.001, start = 100, stop = 200)
  expect_equal(sfm$sim_specs$save_from, "100.0")
  sfm <- sdbuildR("SIR") |>
    sim_specs(start = 50, stop = 100) |>
    sim_specs(save_at = 0.1, dt = 0.001, start = 0, stop = 75)
  expect_equal(sfm$sim_specs$save_from, "0.0")
  sfm <- sdbuildR("coffee_cup") |>
    sim_specs(start = 0, stop = 100) |>
    sim_specs(save_at = 0.1, dt = 0.001, start = 100, stop = 200)
  expect_equal(sfm$sim_specs$save_from, "100.0")

  # warning for large dt
  expect_warning(
    sdbuildR() |> sim_specs(dt = 2),
    "Detected use of large timestep dt = 2\\. This will likely lead to inaccuracies in the simulation"
  )
  expect_warning(
    sdbuildR() |> sim_specs(dt = .5),
    "Detected use of large timestep dt = 0.5\\. This will likely lead to inaccuracies in the simulation"
  )

  # method cannot be NULL
  expect_error(sim_specs(sfm, method = NULL), "method must be a single string")
  expect_error(sim_specs(sfm, method = NA), "method must be a single string")
  expect_error(sim_specs(sfm, method = 9), "method must be a single string")
  expect_error(sim_specs(sfm, method = c("a", "b")), "method must be a single string")
})


test_that("flow cannot have same stock as to and from", {
  sfm <- sdbuildR() |> update("a", "stock")
  expect_error(
    sfm |> update("b", "flow", to = "a", from = "a", eqn = "1"),
    "A flow cannot flow to and from the same stock"
  )

  # Check that this works with multiple variables
  expect_error(
    sfm |> update(c("b", "c"), c("flow", "flow"),
      to = c("a", "d"), from = c("a", "a")
    ),
    "A flow cannot flow to and from the same stock"
  )
  expect_error(
    sfm |> update(c("b", "c"), c("flow", "flow"),
      to = c("a", "d"), from = c("a", "d")
    ),
    "A flow cannot flow to and from the same stock"
  )

  # Check that this works when adding to or from later
  sfm <- sdbuildR() |> update("b", "flow", to = "a")
  expect_warning(
    sfm |> update("b", from = "a"),
    "b is flowing to and from the same variable \\(a\\)"
  )

  sfm <- sdbuildR() |> update("b", "flow", from = "a")
  expect_warning(
    sfm |> update("b", to = "a"),
    "b is flowing to and from the same variable \\(a\\)"
  )

  # Flow cannot flow to itself
  sfm <- sdbuildR()
  expect_error(
    sfm |> update("b", "flow", to = "b"),
    "A flow cannot flow to itself"
  )
  expect_error(
    sfm |> update("b", "flow", from = "b"),
    "A flow cannot flow from itself"
  )
  expect_error(
    sfm |> update(c("a", "b", "c"), "flow", to = "b"),
    "A flow cannot flow to itself"
  )
  expect_error(
    sfm |> update(c("a", "b", "c"), "flow", from = "b"),
    "A flow cannot flow from itself"
  )
  expect_no_error(
    sfm |> update(c("a", "b", "c"), "flow", to = "d")
  )
})


test_that("add and change variable with update() simultaneously", {
  sfm <- sdbuildR() |>
    update("a", "stock")

  expect_error(sfm |> update(c("a", "b"), eqn = 10), "The variable b does not exist in your model")
  sfm <- expect_no_error(sfm |> update(c("a", "b"), "stock", eqn = 10))
  expect_equal(sort(get_names(sfm)[["name"]]), c("a", "b"))
  expect_equal(sfm$model$variables$stock$a$eqn, "10")
  expect_equal(sfm$model$variables$stock$b$eqn, "10")
})


test_that("overwriting to and from of a flow works", {
  sfm <- sdbuildR() |>
    update("a", "stock") |>
    update("b", "flow", to = "a") |>
    update("b", to = "c", from = "d")
  expect_equal(sfm$model$variables$flow$b$to, "c")
  expect_equal(sfm$model$variables$flow$b$from, "d")
})


test_that("incorrect equations throw error", {
  expect_error(
    {
      sdbuildR() |> update("a", "aux", eqn = "a + (1,")
    },
    "Parsing equation of \"a\" failed"
  )
  expect_error(
    {
      sdbuildR() |> update("a", "aux", eqn = "<- (1)")
    },
    "Parsing equation of \"a\" failed"
  )
})


test_that("change_name and change_type in update()", {
  # Change name
  sfm <- sdbuildR() |> update("a", "aux", eqn = 10)
  expect_no_error(sfm |> update("a", change_name = "b"))
  expect_no_message(sfm |> update("a", change_name = "b"))
  sfm <- sfm |> update("a", change_name = "b")
  expect_equal(names(sfm$model$variables$aux), "b")
  expect_equal(as.data.frame(sfm)$name, "b")
  expect_equal(sfm$model$variables$aux$b$name, "b")
  expect_equal(sfm$model$variables$aux$b$label, "b")
  expect_equal(sfm$model$variables$aux$a, NULL)

  # Check update() with change_name and other modified properties
  sfm <- sfm |> update("b", change_name = "c", eqn = "100")
  expect_equal(names(sfm$model$variables$aux), "c")
  expect_equal(sfm$model$variables$aux$b, NULL)
  expect_equal(as.data.frame(sfm)$name, "c")
  expect_equal(sfm$model$variables$aux$c$eqn, "100")
  expect_equal(sfm$model$variables$aux$c$label, "c")

  # Check update() with change_type; are the properties copied?
  sfm <- sdbuildR() |> update("K", "aux", eqn = 100, label = "K")
  expect_no_error(expect_no_message(sfm |> update("K", change_type = "stock")))
  sfm <- sfm |> update("K", change_type = "stock")
  expect_equal(sfm$model$variables$aux$K, NULL)
  expect_equal(sfm$model$variables$stock$K$eqn, "100")
  expect_equal(sfm$model$variables$stock$K$label, "K")
  expect_equal(sfm$model$variables$stock$K$from, NULL)
  expect_equal(sfm$model$variables$stock$K$to, NULL)
  expect_equal(as.data.frame(sfm)$type, "stock")

  # Check update() with change_type and change_name
  sfm <- sdbuildR() |> update("H", "flow", eqn = 100)
  sfm <- sfm |> update("H", change_type = "aux", change_name = "H_new")
  expect_equal(sfm$model$variables$flow[["H"]], NULL)
  expect_equal(sfm$model$variables$flow[["H_new"]], NULL)
  expect_equal(sfm$model$variables$aux[["H"]], NULL)
  expect_equal(sfm$model$variables$aux$H_new$eqn, "100")
  expect_no_error(expect_no_message(as.data.frame(sfm)))
  df <- as.data.frame(sfm)
  expect_equal(as.data.frame(sfm)$name, "H_new")
  expect_equal(as.data.frame(sfm)$type, "aux")

  # Check that changing type from stock -> aux removes the stock from `to` and `from` properties of flows
  sfm <- sdbuildR() |>
    update("G", "stock") |>
    update("to_G", "flow", to = "G")
  expect_warning(
    sfm |> update("G", change_type = "aux"),
    "to_G is flowing to a variable which is not a stock \\(G\\)"
  )
  suppressWarnings({
    sfm <- sfm |> update("G", change_type = "aux")
  })
  expect_equal(sfm$model$variables$flow$to_G$to, "")
  expect_equal(sfm$model$variables$stock$G, NULL)
  expect_equal(sfm$model$variables$aux$G$eqn, "0")

  # Test that properties are not affected if no change is made
  sfm <- sdbuildR() |>
    update("abc", "aux", eqn = "def") |>
    update("abc")
  expect_equal(sfm$model$variables$aux$abc$eqn, "def")

  sfm <- sdbuildR() |>
    update("abc", "aux", eqn = "def") |>
    update("abc", "aux")
  expect_equal(sfm$model$variables$aux$abc$eqn, "def")


  # Check multiple change names
  sfm <- templates("predator_prey")
  expect_error(sfm |> update("prey", change_name = c("prey1", "prey2")), "You can only change the name of one variable at a time")
  expect_error(sfm |> update(c("prey", "predator"), change_name = c("prey1", "prey2")), "You can only change the name of one variable at a time")

  expect_error(sfm |> update("prey", change_type = c("stock", "aux")), "You can only change the type of one variable at a time")
  expect_error(sfm |> update(c("prey", "predator"), change_type = "stock"), "You can only change the type of one variable at a time")

  sfm <- sfm |>
    update("prey", change_name = "frustration") |>
    update("predator", change_name = "drugs")
  df <- as.data.frame(sfm, type = "stock")
  expect_equal(sort(df$name), c("drugs", "frustration"))
  expect_equal(sort(df$label), c("Predator", "Prey")) # Label is unchanged
  df <- as.data.frame(sfm)
  expect_true(grepl("frustration", df[df$name == "predator_births", "eqn"]))
  expect_false(grepl("prey", df[df$name == "predator_births", "eqn"]))

  expect_true(grepl("drugs", df[df$name == "predator_deaths", "eqn"]))
  expect_false(grepl("predator", df[df$name == "predator_deaths", "eqn"]))

  expect_true(grepl("frustration", df[df$name == "prey_births", "eqn"]))
  expect_false(grepl("prey", df[df$name == "prey_births", "eqn"]))

  expect_true(grepl("frustration", df[df$name == "prey_deaths", "eqn"]))
  expect_false(grepl("prey", df[df$name == "prey_deaths", "eqn"]))

  expect_true(grepl("drugs", df[df$name == "prey_deaths", "eqn"]))
  expect_false(grepl("predator", df[df$name == "prey_deaths", "eqn"]))


  # Check that no message or warning is thrown
  sfm <- sdbuildR() |> update("a", "aux")

  expect_no_error(expect_no_warning(expect_no_message(sfm |>
    update("a", change_type = "flow", from = "b"))))

  # Check that label is retained
  sfm <- sfm |> update("a", change_type = "flow", label = "B")
  expect_equal(sfm$model$variables$flow$a$label, "B")

  # Check that source is updated with change_name
  sfm <- sdbuildR() |>
    update("a", "lookup", xpts = 1, ypts = 1, source = "b") |>
    update("b", "stock") |>
    update("b", change_name = "c")

  expect_equal(sfm$model$variables$gf$a$source, "c")
})


test_that("from and to can only be stocks", {
  sfm <- expect_silent(sdbuildR() |> update("a", "flow", to = "b"))
  sfm <- expect_warning(
    sfm |> update("b", "flow"),
    "a is flowing to a variable which is not a stock \\(b\\)! Removing b from `to`"
  )
  expect_null(sfm[["variables"]][sfm[["variables"]][["name"]] == "a" & sfm[["variables"]][["type"]] == "flow", "to"][[1]])

  sfm <- expect_no_error(expect_no_message(expect_no_warning(sdbuildR() |> update("a", "flow", from = "b"))))
  sfm <- expect_warning(
    sfm |> update("b", "flow"),
    "a is flowing from a variable which is not a stock \\(b\\)! Removing b from `from`"
  )
  expect_null(sfm[["variables"]][sfm[["variables"]][["name"]] == "a" & sfm[["variables"]][["type"]] == "flow", "from"][[1]])

  expect_error(
    sdbuildR() |> update("a", "flow", from = "b", to = "b"),
    "A flow cannot flow to and from the same stock"
  )
})


test_that("discard in update() works", {
  sfm <- sdbuildR("Lorenz")
  sfm <- expect_no_error(expect_no_message(sfm |> update("x", discard = TRUE)))
  expect_equal(sfm$model$variables$stock$x, NULL)
  expect_equal(sfm$model$variables$flow$dx_dt$to, NULL)
  df <- as.data.frame(sfm, type = "stock")
  expect_equal(sort(df$name), c("y", "z"))

  # discard while specifying wrong type
  expect_error(
    sfm |> update("dy_dt", "stock", discard = TRUE),
    "These variables exist in your model but not as the type specified"
  )

  # discard while specifying type and other properties; these should be ignored
  expect_no_error(expect_no_message(sfm |> update("z", "stock", eqn = "10", discard = TRUE)))
  sfm <- sfm |> update("sigma", "constant", eqn = "10", discard = TRUE)
  expect_equal(sfm$model$variables$constant$sigma, NULL)
  df <- as.data.frame(sfm)
  expect_equal("sigma" %in% df$name, FALSE)

  # discard removes the erased variable source
  sfm <- update(sfm, "lookup", "lookup", xpts = 1, ypts = 1, source = "y") |>
    update("y", discard = TRUE)
  expect_equal(sfm$model$variables$gf$gf$source, NULL)
})


test_that("inappropriate properties throw warning", {
  expect_warning(
    sdbuildR() |> update("c", "aux", from = "a"),
    "These properties are not appropriate for the specified type"
  )
  sfm <- suppressWarnings(sdbuildR() |> update("c", "aux", from = "a"))
  expect_equal(sfm$model$variables$aux$c$from, NULL)
  expect_equal(sfm$model$variables$aux$c$to, NULL)
  expect_equal(sfm$model$variables$aux$c$eqn, "0")

  expect_warning(
    sdbuildR() |> update(c("a", "b", "c"), c("aux", "stock", "flow"),
      eqn = 1,
      from = "d"
    ),
    "These properties are not appropriate for all specified types \\(aux, stock, flow\\):\\n- from\nThese will be ignored"
  )
  sfm <- suppressWarnings(sdbuildR() |> update(c("a", "b", "c"),
    c("aux", "stock", "flow"),
    eqn = 1,
    from = "d"
  ))
  expect_null(sfm$model$variables$aux$a$from)
  expect_null(sfm$model$variables$stock$b$from)
  expect_equal(sfm$model$variables$flow$c$from, "d")
  df <- data.frame(sfm)
  expect_true(is.na(df[df$name == "a", "from"]))
  expect_true(is.na(df[df$name == "b", "from"]))
  expect_equal(df[df$name == "c", "from"], "d")
})


test_that("Julia equations are added in update()", {
  # Ensure Julia equations are added immediately
  sfm <- sdbuildR() |> update("c", "aux", eqn = "a")
  expect_true("eqn" %in% names(sfm$model$variables$aux$c))
  expect_equal(sfm$model$variables$aux$c$eqn, "a")

  # Ensure Julia equation is updated with changed equation
  sfm <- sfm |> update("c", eqn = "90")
  expect_equal(sfm$model$variables$aux$c$eqn, "90.0")

  # Ensure this works with multiple variables
  sfm <- sdbuildR() |> update(c("a", "b", "c"), c("stock", "flow", "aux"), eqn = c("1", "2", "3"))
  expect_equal(sfm$model$variables$stock$a$eqn, "1.0")
  expect_equal(sfm$model$variables$flow$b$eqn, "2.0")
  expect_equal(sfm$model$variables$aux$c$eqn, "3.0")

  # Ensure Julia equation is updated with changed equation
  sfm <- sfm |> update(c("a", "b", "c"), eqn = c("40", "50", "60"))
  expect_equal(sfm$model$variables$stock$a$eqn, "40.0")
  expect_equal(sfm$model$variables$flow$b$eqn, "50.0")
  expect_equal(sfm$model$variables$aux$c$eqn, "60.0")
})


test_that("vectorized adding variables works in update()", {
  sfm <- sdbuildR() |> update(c("a", "b", "c"), c("stock", "flow", "aux"), eqn = c("1", "2", "3"))
  expect_equal(sfm$model$variables$stock$a$eqn, "1")
  expect_equal(sfm$model$variables$flow$b$eqn, "2")
  expect_equal(sfm$model$variables$aux$c$eqn, "3")
  expect_equal(sfm$model$variables$stock$a$units, "1")
  expect_equal(sfm$model$variables$flow$b$units, "1")
  expect_equal(sfm$model$variables$aux$c$units, "1")
  expect_equal(sfm$model$variables$stock$a$label, "a")
  expect_equal(sfm$model$variables$flow$b$label, "b")
  expect_equal(sfm$model$variables$aux$c$label, "c")
  expect_equal(sfm$model$variables$stock$a$from, NULL)
  expect_equal(sfm$model$variables$flow$b$from, NULL)
  df <- as.data.frame(sfm)
  expect_equal(sort(df$name), c("a", "b", "c"))
  expect_equal(sort(df$type), c("aux", "flow", "stock"))

  # Add some vectorized properties and some single properties, as well as "wrong" properties for that type
  expect_warning(
    sdbuildR() |> update(c("x", "y", "z"), "stock",
      eqn = 300,
      units = c("kilograms", "10 meters", "3 Sec"),
      label = c("X", "Y", "Z"), from = c("a", "b", "c"),
      xpts = c(1, 2, 3), ypts = c(4, 5, 6)
    ),
    "These properties are not appropriate for the specified type \\(stock\\):\\n- from, xpts, ypts"
  )
  sfm <- suppressWarnings(sdbuildR() |> update(c("x", "y", "z"), "stock",
    eqn = 300,
    units = c("kilograms", "10 meters", "3 Sec"),
    label = c("X", "Y", "Z"), from = c("a", "b", "c"),
    xpts = c(1, 2, 3), ypts = c(4, 5, 6)
  ))
  expect_equal(sfm$model$variables$stock$x$eqn, "300")
  expect_equal(sfm$model$variables$stock$y$eqn, "300")
  expect_equal(sfm$model$variables$stock$z$eqn, "300")
  expect_equal(sfm$model$variables$stock$x$units, "kg")
  expect_equal(sfm$model$variables$stock$y$units, "10m")
  expect_equal(sfm$model$variables$stock$z$units, "3s")
  expect_equal(sfm$model$variables$stock$x$from, NULL)
  expect_equal(sfm$model$variables$stock$y$from, NULL)
  expect_equal(sfm$model$variables$stock$z$from, NULL)
  expect_equal(sfm$model$variables$stock$x$xpts, NULL)
  expect_equal(sfm$model$variables$stock$y$xpts, NULL)
  expect_equal(sfm$model$variables$stock$z$xpts, NULL)
  expect_equal(sfm$model$variables$stock$x$ypts, NULL)
  expect_equal(sfm$model$variables$stock$y$ypts, NULL)
  expect_equal(sfm$model$variables$stock$z$ypts, NULL)
})


test_that("flows always have a from and to property", {
  sfm <- sdbuildR() |> update("a", "flow")
  expect_true("from" %in% names(sfm$model$variables$flow$a))
  expect_true("to" %in% names(sfm$model$variables$flow$a))

  sfm <- sdbuildR() |> update("a", "flow", to = "b")
  expect_true("from" %in% names(sfm$model$variables$flow$a))
  expect_true("to" %in% names(sfm$model$variables$flow$a))
})


test_that("update() works", {
  # Empty update() gives error
  expect_error(
    {
      update()
    },
    "No model specified"
  )
  expect_error(
    {
      sdbuildR() |> update()
    },
    "name must be specified"
  )

  sfm <- sdbuildR()
  sfm <- sfm |> update("a", "aux", eqn = 10)

  # Try to add wrong type
  expect_error(
    {
      sfm |> update("a", "Non")
    },
    "type needs to be one of 'stock', 'flow', 'constant', 'aux', or 'gf'"
  )

  # Add auxiliary
  expect_equal(names(sfm$model$variables$aux), "a")
  expect_equal(sfm$model$variables$aux$a$eqn, "10")

  # Try to modify variable and add new variable simultaneously
  sfm <- sdbuildR() |> update("a", "stock")
  sfm <- sfm |> update(c("a", "b"), c("stock", "flow"), eqn = c("100", "1000"), units = "seconds")

  expect_equal(sfm$model$variables$stock$a$eqn, "100")
  expect_equal(sfm$model$variables$flow$b$eqn, "1000")
  expect_equal(sfm$model$variables$stock$a$units, "s")
  expect_equal(sfm$model$variables$flow$b$units, "s")

  # Try to overwrite existing variable whilst specifying the type
  sfm <- sfm |> update("a", "stock", eqn = "10000")
  expect_equal(sfm$model$variables$stock$a$eqn, "10000")

  # Try to overwrite existing variable whilst specifying the wrong type
  expect_error(
    {
      sfm |> update("a", "flow", eqn = "90")
    },
    "These variables already exist in your model, but not as the type specified"
  )
  expect_equal(sfm$model$variables$stock$a$eqn, "10000")

  # Try to modify non-existing variable
  expect_error(sdbuildR() |> update("b", change_name = "c"), "b does not exist in your model!")

  # Ensure NULL changes to "0"
  expect_warning(sdbuildR() |> update("c", "aux", eqn = NULL), "eqn cannot be NULL!")
  suppressWarnings({
    sfm <- sdbuildR() |> update("c", "aux", eqn = NULL)
  })
  expect_equal(sfm$model$variables$aux$c$eqn, "0")
  expect_equal(sfm$model$variables$aux$c$eqn, "0")

  # Ensure empty equation changes to "0"
  expect_warning(sdbuildR() |> update("c", "aux", eqn = ""), "eqn cannot be empty!")
  suppressWarnings({
    sfm <- sdbuildR() |> update("c", "aux", eqn = "")
  })
  expect_equal(sfm$model$variables$aux$c$eqn, "0")
  expect_equal(sfm$model$variables$aux$c$eqn, "0")

  # Ensure equation with missing brackets throws useful error
  expect_error(
    sdbuildR() |> update("c", "aux", eqn = "a + (1, "),
    "Parsing equation of \"c\" failed"
  )

  # Multiple equations but only one name should throw error
  expect_error(
    {
      sdbuildR() |> update("a", "constant", eqn = c("1", "2"))
    },
    "The length of eqn = 1, 2 must be either 1 or equal to the length of name = a."
  )
})


test_that("ensure_length() works", {
  # Check that error is thrown when length(arg) != length(target)
  eqn <- c("1", "2", "3")
  name <- c("a")
  expect_error(
    ensure_length(eqn, name),
    "The length of eqn = 1, 2, 3 must be either 1 or equal to the length of name = a"
  )

  # Should work when length(arg) == length(target)
  eqn <- c("1", "2", "3")
  name <- c("a", "b", "c")
  expect_no_error(ensure_length(eqn, name))

  eqn <- c("1")
  name <- c("a", "b", "c")
  expect_no_error(ensure_length(eqn, name))

  # Check whether length of arg is changed
  eqn <- c("1")
  name <- c("a", "b", "c")
  expect_equal(ensure_length(eqn, name), c("1", "1", "1"))
})


test_that("diagnose() works", {
  expect_message(
    diagnose(sdbuildR("SIR")),
    "No problems detected"
  )
  expect_message(diagnose(sdbuildR("SIR")), "These variables have an equation of 0:\\n- Recovered")
  expect_message(diagnose(sdbuildR("predator_prey")), "No problems detected!")
  expect_message(diagnose(sdbuildR("logistic_model")), "No problems detected!")
  expect_message(diagnose(sdbuildR("Crielaard2022")), "No problems detected!")

  # Detect absence of stocks or flows
  expect_message(diagnose(sdbuildR()), "Your model has no stocks")

  # Detect stocks without inflows or outflows
  expect_message(diagnose(sdbuildR() |> update("Prey", "stock")), "Your model has no flows.")

  # Detect one stock without inflows or outflows
  sfm <- sdbuildR() |>
    update("Prey", "stock") |>
    update("Predator", "stock") |>
    update("births", "flow", eqn = "0.1 * Predator", to = "Prey")
  expect_message(diagnose(sfm), "These stocks are not connected to any flows:\\n- Predator")

  # Detect circularities in equation definitions
  expect_message(
    {
      diagnose(sfm = sdbuildR() |> update("Prey", "stock", eqn = "Predator") |>
        update("Predator", "stock", eqn = "Prey"))
    },
    "Circular dependencies detected involving variables: Predator, Prey"
  )

  sfm <- sdbuildR("logistic_model")
  sfm <- sfm |>
    update("X", change_name = "tasks") |>
    update("K", label = "Resource") |>
    update("K", change_type = "stock")
  expect_message(diagnose(sfm), "These stocks are not connected to any flows:\\n- K")

  # Dependency on itself
  sfm <- sdbuildR() |> update("a", "stock", eqn = "cos(a)")
  expect_warning(
    sim <- simulate(sfm),
    "The variable 'a' is referenced in a\\$eqn but hasn't been defined"
  )
  expect_false(sim$success)
})


test_that("detect_undefined_var() works", {
  # Check that undefined variables are detected
  sfm <- sdbuildR() |> update("a", "aux", eqn = "b + c")
  out <- detect_undefined_var(sfm)
  expect_equal(grepl("Define these missing variables or correct any spelling mistakes", out$msg), TRUE)

  # Check that no error is thrown for defined variables
  sfm <- sdbuildR() |>
    update("a", "aux", eqn = "b + c") |>
    update("b", "aux") |>
    update("c", "aux")
  out <- detect_undefined_var(sfm)
  expect_equal(out$issue, FALSE)

  #** macros should also be found
})




test_that("as.data.frame(sfm) works", {
  # Check that as.data.frame() works
  sfm <- sdbuildR()
  expect_equal(class(as.data.frame(sfm)), "data.frame")
  expect_equal(nrow(as.data.frame(sfm)), 0)

  # Check that as.data.frame() works with variables
  sfm <- sfm |> update("a", "aux", eqn = "10")
  df <- as.data.frame(sfm)
  expect_equal(class(df), "data.frame")
  expect_equal(df[["type"]], "aux")
  expect_true(all(c("type", "name", "eqn", "label") %in% names(df)))
  expect_false(any(c("intermediary", "func") %in% names(df)))

  # Check that it works with templates
  sfm <- sdbuildR("predator_prey")
  expect_true(nrow(as.data.frame(sfm)) > 0)

  # Specify type
  sfm <- sdbuildR("predator_prey")
  expect_error(as.data.frame(sfm, type = "variable"), "type needs to be one or more of")
  expect_no_error(as.data.frame(sfm, type = "auxiliary")) # works with spelling out aux
  expect_no_error(as.data.frame(sfm, type = "auxiliaries")) # works with spelling out aux
  expect_no_error(as.data.frame(sfm, type = "Stock")) # works with capital letters
  expect_no_error(as.data.frame(sfm, type = c("stock", "lookup"))) # works with multiple types
  expect_no_error(as.data.frame(sfm, type = c("lookup"))) # works when type doesn't exist
  expect_equal(nrow(as.data.frame(sfm, type = c("lookup"))), 0) # works when type doesn't exist
  expect_no_error(as.data.frame(sfm, type = c("lookup"))) 
  
  # Specify name
  sfm <- sdbuildR("Lorenz")
  expect_error(as.data.frame(sfm, name = "a"), "a does not exist in your model")
  expect_error(as.data.frame(sfm, name = c("sigma", "rho", "X")), "X does not exist in your model")
  expect_error(as.data.frame(sfm, name = ""), "At least one name must be specified")
  expect_no_error(as.data.frame(sfm, name = "x"))
  expect_no_error(as.data.frame(sfm, name = c("x", "dy_dt", "sigma")))

  # Specify properties
  sfm <- sdbuildR("predator_prey")
  expect_error(as.data.frame(sfm, properties = "a"), "a is not an existing property")
  expect_error(as.data.frame(sfm, properties = c("a", "b")), "a, b are not existing properties")
  expect_error(as.data.frame(sfm, properties = c("a", "eqn")), "a is not an existing property")
  expect_error(as.data.frame(sfm, properties = ""), "At least one property must be specified")

  # Works with macros
  sfm <- sdbuildR() |>
    macro("a", eqn = "1", doc = "a macro") |>
    macro("b") |>
    macro("c")
  expect_no_error(as.data.frame(sfm))
  df <- as.data.frame(sfm)
  expect_true(all(df$type == "macro"))
  expect_true(all(c("a", "b", "c") %in% df$name))
  expect_equal(as.data.frame(sfm, name = "a")$name, "a")
  expect_equal(names(as.data.frame(sfm, properties = "eqn")), c("type", "name", "eqn"))
  expect_equal(nrow(as.data.frame(sfm, type = c("aux"))), 0)

  # Combine type, name, properties
  sfm <- sdbuildR("Lorenz")
  expect_no_error(as.data.frame(sfm, name = c("x", "y", "z"), type = c("stock", "flow", "aux")))
  expect_no_error(as.data.frame(sfm, name = c("x", "y", "z"), type = c("stock", "flow", "aux"), properties = c("eqn", "label", "doc", "from")))

  df <- as.data.frame(sfm, name = c("x", "y", "z"), type = c("stock", "flow", "aux"), properties = c("eqn", "label", "doc", "from"))
  expect_equal(names(df), c("type", "name", "eqn", "label")) # "doc", "from" are not recorded for these variables
  expect_equal(nrow(df), 3)

  # Check with Julia properties
  expect_no_error(as.data.frame(sdbuildR("SIR"), properties = c("type", "name", "eqn")))
  expect_equal(colnames(as.data.frame(sdbuildR("SIR"), properties = c("eqn"))), c("type", "name", "eqn"))
  expect_equal(sort(colnames(as.data.frame(sdbuildR("SIR"), properties = c("eqn", "eqn")))), c("eqn", "eqn", "name", "type"))
})


test_that("summary() works", {
  sfm <- sdbuildR("SIR")
  ans <- summary(sfm)
  expect_true(length(ans) > 0)
  expect_no_error(expect_no_warning(print(summary(sfm))))
})


test_that("macro() works", {
  # No name throws error
  expect_error(sdbuildR() |> macro(), "name must be specified!")

  # Default properties
  sfm <- sdbuildR() |> macro("abc")
  expect_equal(sfm$macro$abc$eqn, "0")
  expect_equal(sfm$macro$abc$doc, "")

  # Test that properties are not affected if no change is made
  sfm <- sdbuildR() |>
    macro("abc", eqn = "def") |>
    macro("abc")
  expect_equal(sfm$macro$abc$eqn, "def")

  sfm <- sdbuildR() |>
    macro("abc", eqn = "def", doc = "Don't edit") |>
    macro("abc", doc = "Edit me")
  expect_equal(sfm$macro$abc$eqn, "def")
  expect_equal(sfm$macro$abc$doc, "Edit me")

  # Multiple macros
  sfm <- sdbuildR() |> macro(c("abc", "def"), eqn = c("1", "2"), doc = c("A", "B"))
  expect_equal(sfm$macro$abc$eqn, "1")
  expect_equal(sfm$macro$abc$eqn, "abc = 1.0")
  expect_equal(sfm$macro$def$eqn, "2")
  expect_equal(sfm$macro$def$eqn, "def = 2.0")
  expect_equal(sfm$macro$abc$doc, "A")
  expect_equal(sfm$macro$def$doc, "B")

  # Check valid names
  sfm <- sdbuildR()
  expect_warning(sfm |> macro("F"), "Names were changed to be syntactically valid and/or avoid overlap: F -> F_1")
  expect_warning(sfm |> macro("TRUE"), "Names were changed to be syntactically valid and/or avoid overlap: TRUE -> TRUE_")
  expect_warning(sfm |> macro("function"), "Names were changed to be syntactically valid and/or avoid overlap: function -> function_")
  expect_warning(sfm |> macro("while"), "Names were changed to be syntactically valid and/or avoid overlap: while -> while_")
  expect_warning(sfm |> macro("for"), "Names were changed to be syntactically valid and/or avoid overlap: for -> for_")

  # Translating functions works
  sfm <- sdbuildR() |>
    update("X", "aux", eqn = "K(t)") |>
    macro("K", eqn = "function(x) 1 + x")
  expect_equal(sfm$model$variables$aux$X$eqn, "K(t)")
  expect_equal(sfm$model$variables$aux$X$eqn, "K(t)")
  expect_equal(sfm$macro$K$eqn, "function(x) 1 + x")
  expect_equal(sfm$macro$K$eqn, "function K(x)\n 1.0 .+ x\nend")

  # change_name
  sfm <- sdbuildR() |>
    macro("abc", eqn = "def") |>
    macro("abc", change_name = "xyz")
  expect_equal(sfm$macro$xyz$eqn, "def")
  expect_equal(sfm$macro$abc, NULL)
  df <- as.data.frame(sfm, type = "macro")
  expect_equal(df$name, "xyz")

  sfm <- sdbuildR() |>
    update("X", "aux", eqn = "G(t)") |>
    macro("G", eqn = "function(x) 1 + x")
  expect_equal(sfm$model$variables$aux$X$eqn, "G(t)")
  expect_equal(sfm$model$variables$aux$X$eqn, "G(t)")
  expect_equal(sfm$macro$G$eqn, "function(x) 1 + x")
  expect_equal(sfm$macro$G$eqn, "function G(x)\n 1.0 .+ x\nend")
  expect_equal(sfm$macro$F1, NULL)


  # Check that change_name is changed throughout the model
  sfm <- sdbuildR() |>
    update("X", "aux", eqn = "F1(t)") |>
    macro("F1", eqn = "function(x) 1 + x") |>
    macro("F1", change_name = "G")
  expect_equal(sfm$model$variables$aux$X$eqn, "G(t)")
  expect_equal(sfm$model$variables$aux$X$eqn, "G(t)")
  expect_equal(sfm$macro$G$eqn, "function(x) 1 + x")
  expect_equal(sfm$macro$G$eqn, "function G(x)\n 1.0 .+ x\nend")
  expect_equal(sfm$macro$F1, NULL)
})


test_that("add_from_df() works", {
  df <- data.frame(
    type = c("stock", "flow", "flow", "constant", "constant"),
    name = c("X", "inflow", "outflow", "r", "K"),
    eqn = c(.01, "r * X", "r * X^2 / K", 0.1, 1),
    label = c("Population size", "Births", "Deaths", "Growth rate", "Carrying capacity"),
    to = c(NA, "X", NA, NA, NA),
    from = c(NA, NA, "X", NA, NA)
  )

  sfm <- sdbuildR() |> sim_specs(stop = 5, dt = 1)
  sfm <- expect_no_error(expect_no_message(add_from_df(sfm, df = df)))
  expect_equal(sort(get_names(sfm)[["name"]]), sort(c("X", "inflow", "outflow", "r", "K")))
  expect_null(sfm[["variables"]][sfm[["variables"]][["name"]] == "X" & sfm[["variables"]][["type"]] == "stock", "to"][[1]])
  expect_equal(sfm[["variables"]][sfm[["variables"]][["name"]] == "inflow" & sfm[["variables"]][["type"]] == "flow", "to"][[1]], "X")
  sim <- expect_no_error(expect_no_message(simulate(sfm)))
  expect_true(nrow(sim$df) > 0)
})


test_that("graphical functions are created with correct properties", {
  sfm <- sdbuildR()

  # Basic graphical function
  sfm <- update(sfm, "lookup1", "lookup",
    xpts = c(0, 5, 10),
    ypts = c(0, 10, 20)
  )

  defaults <- as.list(formals(update.sdbuildR))

  expect_true("lookup1" %in% names(sfm$model$variables$gf))
  expect_equal(sfm$model$variables$gf$lookup1$xpts, "c(0, 5, 10)")
  expect_equal(sfm$model$variables$gf$lookup1$ypts, "c(0, 10, 20)")
  expect_equal(sfm$model$variables$gf$lookup1$interpolation, defaults[["interpolation"]])
  expect_equal(sfm$model$variables$gf$lookup1$extrapolation, defaults[["extrapolation"]])
})


test_that("graphical functions require both xpts and ypts or neither", {
  sfm <- sdbuildR()

  expect_error(
    update(sfm, "lookup", "lookup"),
    "xpts and ypts must be specified"
  )

  expect_error(
    update(sfm, "only_x", "lookup", xpts = c(0, 10)),
    "ypts must be specified"
  )

  expect_error(
    update(sfm, "only_y", "lookup", ypts = c(0, 100)),
    "xpts must be specified"
  )
})


test_that("graphical functions reject mismatched xpts and ypts lengths", {
  sfm <- sdbuildR()

  expect_error(
    update(sfm, "bad_gf", "lookup",
      xpts = c(0, 5, 10),
      ypts = c(0, 10)
    ),
    "length of xpts must match that of ypts"
  )
})

test_that("graphical functions support different interpolation and extrapolation methods", {
  sfm <- sdbuildR()

  # Linear interpolation
  sfm <- update(sfm, "linear_gf", "lookup",
    xpts = c(0, 10),
    ypts = c(0, 100),
    interpolation = "linear"
  )
  expect_equal(sfm$model$variables$gf$linear_gf$interpolation, "linear")

  # Constant interpolation
  sfm <- update(sfm, "constant_gf", "lookup",
    xpts = c(0, 10),
    ypts = c(0, 100),
    interpolation = "constant"
  )
  expect_equal(sfm$model$variables$gf$constant_gf$interpolation, "constant")

  sfm <- sdbuildR()

  # Nearest extrapolation
  sfm <- update(sfm, "nearest_gf", "lookup",
    xpts = c(0, 10),
    ypts = c(0, 100),
    extrapolation = "nearest"
  )
  expect_equal(sfm$model$variables$gf$nearest_gf$extrapolation, "nearest")

  # NA extrapolation
  sfm <- update(sfm, "na_gf", "lookup",
    xpts = c(0, 10),
    ypts = c(0, 100),
    extrapolation = "NA"
  )
  expect_equal(sfm$model$variables$gf$na_gf$extrapolation, "NA")

  expect_error(
    update(sfm, "bad_interp", "lookup",
      xpts = c(0, 10),
      ypts = c(0, 100),
      interpolation = "invalid"
    ),
    "interpolation must be 'linear' or 'constant'"
  )

  expect_error(
    update(sfm, "bad_extrap", "lookup",
      xpts = c(0, 10),
      ypts = c(0, 100),
      extrapolation = "invalid"
    ),
    "extrapolation must be either 'nearest' or 'NA'"
  )
})


test_that("graphical functions accept source parameter", {
  sfm <- sdbuildR() |> sim_specs(stop = 5, dt = 1)
  sfm <- update(sfm, "stock1", "stock", eqn = 10)

  sfm <- update(sfm, "lookup_with_source", "lookup",
    xpts = c(0, 10),
    ypts = c(0, 100),
    source = "stock1"
  )

  expect_equal(sfm$model$variables$gf$lookup_with_source$source, "stock1")

  # graphical functions with undefined source throw error at simulation
  sim <- expect_no_error(expect_no_message(simulate(sfm)))

  sfm <- update(sfm, "lookup_with_source", source = "stock2")

  expect_warning(
    sim <- simulate(sfm),
    "The variable 'stock2' is referenced in lookup_with_source\\$source but hasn't been defined"
  )
  expect_false(sim$success)
})

test_that("graphical functions reject multiple sources", {
  sfm <- sdbuildR()

  expect_error(
    update(sfm, "multi_source", "lookup",
      xpts = c(0, 10),
      ypts = c(0, 100),
      source = c("var1", "var2")
    ),
    "source must be a single value"
  )
})


test_that("graphical functions don't support vectorized building", {
  sfm <- sdbuildR()

  expect_error(
    update(sfm, c("gf1", "gf2"), "lookup",
      xpts = c(0, 10),
      ypts = c(0, 100)
    ),
    "Vectorized building is not supported for graphical functions"
  )
})

test_that("graphical functions handle character string input for pts", {
  sfm <- sdbuildR()

  sfm <- update(sfm, "string_gf", "lookup",
    xpts = "c(0, 5, 10)",
    ypts = "c(0, 10, 20)"
  )

  expect_equal(sfm$model$variables$gf$string_gf$xpts, "c(0, 5, 10)")
  expect_equal(sfm$model$variables$gf$string_gf$ypts, "c(0, 10, 20)")
})

test_that("prep_equations_variables generates correct approxfun for gf", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "test_gf", "lookup",
    xpts = c(0, 5, 10),
    ypts = c(0, 50, 100),
    interpolation = "linear",
    extrapolation = "nearest"
  )
  sfm <- sim_specs(sfm, keep_nonnegative_flow = TRUE)
  sfm <- prep_equations_variables(sfm)

  eqn_str <- sfm$model$variables$gf$test_gf$eqn_str

  expect_true(grepl("stats::approxfun", eqn_str))
  expect_true(grepl("method = 'linear'", eqn_str))
  expect_true(grepl("rule = 2", eqn_str))
  expect_true(grepl("x = c\\(0, 5, 10\\)", eqn_str))
  expect_true(grepl("y = c\\(0, 50, 100\\)", eqn_str))
})

test_that("prep_equations_variables handles NA extrapolation correctly", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "na_extrap_gf", "lookup",
    xpts = c(0, 10),
    ypts = c(0, 100),
    extrapolation = "NA"
  )
  sfm <- sim_specs(sfm, keep_nonnegative_flow = TRUE)
  sfm <- prep_equations_variables(sfm)

  eqn_str <- sfm$model$variables$gf$na_extrap_gf$eqn_str
  expect_true(grepl("rule = 1", eqn_str))
})

test_that("prep_equations_variables handles constant interpolation", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "const_gf", "lookup",
    xpts = c(0, 10),
    ypts = c(0, 100),
    interpolation = "constant"
  )
  sfm <- sim_specs(sfm, keep_nonnegative_flow = TRUE)
  sfm <- prep_equations_variables(sfm)

  eqn_str <- sfm$model$variables$gf$const_gf$eqn_str
  expect_true(grepl("method = 'constant'", eqn_str))
})

test_that("graphical functions can be modified", {
  sfm <- sdbuildR()

  sfm <- update(sfm, "modify_gf", "lookup",
    xpts = c(0, 10),
    ypts = c(0, 100)
  )

  # Modify interpolation
  sfm <- update(sfm, "modify_gf",
    interpolation = "constant"
  )

  expect_equal(sfm$model$variables$gf$modify_gf$interpolation, "constant")

  sfm <- update(sfm, "modify_gf",
    xpts = c(10, 20)
  )

  expect_equal(sfm$model$variables$gf$modify_gf$xpts, "c(10, 20)")

  expect_error(update(sfm, "modify_gf",
    xpts = c(10, 20, 30)
  ), "For graphical functions, the length of xpts must match that of ypts")
})
