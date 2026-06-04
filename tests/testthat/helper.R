# Test Helper Functions
expect_snapshot_plot <- function(name, code, fileext = NULL, width = 4, height = 4) {
  # Announce the file before touching skips or running `code`. This way,
  # if the skips are active, testthat will not auto-delete the corresponding snapshot file.
  withr::local_pdf(NULL)

  pl <- code
  if (is.null(fileext)) {
    if (inherits(pl, "plotly")) {
      fileext <- ".png"
      plotly_object <- TRUE
    } else if (inherits(pl, "grViz")) {
      fileext <- ".svg"
      plotly_object <- FALSE
    } else {
      stop("Unable to determine file extension for plot snapshot. Please specify fileext.")
    }
  }

  if (plotly_object) {
    announce_snapshot_file(name = name)
    announce_snapshot_file(paste0(name, ".png"))
  }

  skip_on_cran()
  skip_if(
    plotly_object && !has_internet(),
    "No internet connection for plot snapshot test"
  )

  if (!plotly_object) {
    # DiagrammeR
    expect_snapshot_value(pl[["x"]][["diagram"]], style = "json")

  } else {

    skip_on_os("mac") # floating point differences cause snapshot failures on GitHub Actions macOS runners
    skip_on_os("linux")

    # Stable assertion on structure
    json <- normalize_plotly(pl) |> jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

    json_path <- tempfile(fileext = ".json")
    writeLines(json, json_path)
    expect_snapshot_file(json_path, name = name)

    # Visual artifact for manual inspection, never fails
    skip_if(Sys.getenv("SDBUILDR_CREATE_TEST_FIGS") != "true", "Skipping plot snapshot creation (SDBUILDR_CREATE_TEST_FIGS not set to 'true')")

    img_path <- tempfile(fileext = ".png")
    tryCatch({
      export_plot(pl, file = img_path, width = width, height = height)
      expect_snapshot_file(img_path,
      name = paste0(name, ".png"),
      compare = function(old, new) TRUE
    )
    },
      stockflow_export_error = function(e) {
        testthat::skip(paste("Plot export unavailable:", conditionMessage(e)))
      }
    )
    
  }

  invisible()
}

normalize_plotly <- function(pl, digits = 2) {
      # # normalize plotly's random internal IDs
    # json <- gsub('"[a-f0-9]{8,}"', '"ID"', json)
  b <- plotly::plotly_build(pl)$x
  rn <- function(z) if (is.numeric(z)) round(z, digits)
                    else if (is.list(z)) lapply(z, rn) else z
  list(data = lapply(b$data, rn), layout = rn(b$layout))
}

#' Helper to skip test if Julia is not ready
#'
skip_if_julia_not_ready <- function() {
  testthat::skip_on_cran()

  env_setup <- tryCatch(
    {
      suppressWarnings({
        is_julia_env_setup()
      })
    },
    error = function(e) {
      return(FALSE)
    }
  )

  if (!env_setup) {
    testthat::skip()
  }

  invisible()
}


#' Expect successful simulation
#'
#' Helper to verify a simulation completes successfully
#'
#' @param sfm A stock-and-flow model
#' @param ... Additional arguments passed to simulate()
#' @returns Simulation result
expect_successful_simulation <- function(sfm, ...) {
  sim <- expect_no_error(simulate(sfm, ...))
  expect_true(sim$success)
  expect_true(nrow(sim$df) > 0)
  expect_true("time" %in% colnames(sim$df))

  # Time range should be correct
  expect_equal(max(sim$df$time), as.numeric(sfm$sim_settings$stop))

  invisible(sim)
}

#' Execute code quietly (suppressing messages and warnings)
#' 
#' @noRd 
silence <- function(expr) {
  suppressMessages(suppressWarnings(expr))
}


#' Simulate the SIR model
#'
#' Creates and simulates the SIR model, allowing overrides such as only_stocks.
sir_sim <- function(..., only_stocks = TRUE, seed = 123) {
  simulate(sdbuildR("SIR"), only_stocks = only_stocks, seed = seed, ...)
}


#' Create a basic stock-and-flow model for tests
#'
#' Returns an sdbuildR model with one stock S (eqn=1) and one flow Flow1 (eqn=S, to=S)
#' to reduce duplication across tests. Note: uses "Flow1" instead of "F" to avoid
#' name conflict with R's FALSE constant.
make_basic_sfm <- function() {
  sdbuildR() |>
    update("S", type = "stock", eqn = "1") |>
    update("Flow1", type = "flow", eqn = "S", to = "S")
}


# Skip tests if internet not available or on CRAN
skip_if_no_internet <- function() {
  if (!has_internet()) {
    skip("No internet connection")
  }
  skip_on_cran()
}


# Local helper: model with stock, flow, constant, language = Julia
# Validation in ensemble() fails before Julia execution, so no Julia needed
make_ensemble_error_sfm <- function() {
  sdbuildR() |>
    update("S", type = "stock", eqn = "1") |>
    update("Flow1", type = "flow", eqn = "S", to = "S") |>
    update("k", type = "constant", eqn = "0.5") |>
    sim_settings(language = "Julia")
}


# Helper: standard sfm for method tests
make_jl_ensemble_sfm <- function() {
  sdbuildR("Crielaard2022") |>
    sim_settings(start = 0, stop = 10, dt = 0.1, save_at = 1, language = "Julia")
}



make_r_ensemble_random_sfm <- function() {
  sdbuildR("SIR") |>
    update(c(susceptible, infected, recovered), eqn = "runif(1, 1, 1000)") |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1, save_at = 1, seed = 42)
}


# Helper: a small model with a stock, flow, and constant
# Pass language = "Julia" to use the Julia backend instead of R.
make_verifiable_sfm <- function(language = "R") {
  sdbuildR() |>
    update("S", type = "stock", eqn = runif(1, 1, 100)) |>
    update("drain", type = "flow", eqn = "rate * S", from = "S") |>
    update("rate", type = "constant", eqn = "0.1") |>
    sim_settings(stop = 10, dt = 0.1, save_at = 1, language = language, seed = 123)
}


# Helper: build a verify_sdbuildR result with configurable tests.
#   n_tests = 1 : one test ("S non-negative")
#   n_tests = 2 : adds a conditioned test ("S constant at zero rate")
#   with_fail   : adds an intentionally failing test instead of the conditioned one
make_verify_model <- function(n_tests = 1, with_fail = FALSE) {
  sfm <- make_verifiable_sfm() |>
    update(S, eqn = 10) |>
    unit_test(label = "S non-negative", expr = "all(S >= 0)")

  if (n_tests >= 2 && !with_fail) {
    sfm <- sfm |> unit_test(
      label = "S constant at zero rate",
      expr = "all(diff(S) == 0)",
      conditions = list(rate = 0)
    )
  } else if (with_fail) {
    sfm <- sfm |> unit_test(label = "S always zero", expr = "all(S == 0)")
  }

  silence(verify(sfm))
}


# Helper: deterministic R ensemble (seed fixed for snapshot reproducibility).
# save_sims = TRUE  → individual trajectories are stored in the result.
# conditions          → optional named list passed to ensemble().
make_r_ens <- function(n = 5, save_sims = FALSE, conditions = NULL, ...) {
  sfm <- make_r_ensemble_random_sfm()
  args <- list(sfm, n = n, save_sims = save_sims, verbose = FALSE, ...)
  if (!is.null(conditions)) args$conditions <- conditions
  silence(do.call(ensemble, args))
}

make_r_ens_2cond <- function(n = 3, ...) {
  make_r_ens(n = n, conditions = list("contact_rate" = c(1.5, 2.5)), ...)
}

# ##### Plotly attribute helpers #####

#' Expect a plotly object
#'
#' Small helper to assert an object is a plotly visualization.
expect_plotly <- function(x) {
  testthat::expect_s3_class(x, "plotly")
}

##### Plotly attribute helpers #####

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Normalize a Plotly color string to #RRGGBB uppercase when possible
#'
#' Accepts hex (#RRGGBB[AA]), rgb()/rgba() and named R colours; returns
#' uppercase #RRGGBB (alpha stripped) or NA_character_.
normalize_color_string <- function(col) {
  if (is.null(col)) return(NA_character_)
  col_chr <- as.character(col)[1L]
  if (!nzchar(col_chr)) return(NA_character_)

  if (grepl("^rgba?\\(", col_chr)) {                       # rgb()/rgba()
    nums <- as.numeric(strsplit(gsub("rgba?\\(|\\)", "", col_chr), ",")[[1L]])
    if (length(nums) >= 3) {
      rgb <- nums[1:3]
      if (max(rgb, na.rm = TRUE) <= 1) rgb <- round(rgb * 255)
      return(toupper(grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)))
    }
  }
  if (grepl("^#", col_chr)) {                              # hex, strip any alpha
    return(toupper(if (nchar(col_chr) >= 7) substr(col_chr, 1, 7) else col_chr))
  }
  v <- tryCatch(grDevices::col2rgb(col_chr), error = function(e) NULL)  # named
  if (!is.null(v)) return(toupper(grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255)))
  NA_character_
}

#' One comprehensive trace-level data frame (colour column normalized)
plotly_traces <- function(pl) {
  traces <- plotly::plotly_build(pl)[["x"]][["data"]]
  if (!length(traces)) return(data.frame())

  scal <- function(x) if (is.null(x) || !nzchar(as.character(x)[1L])) NA_character_ else as.character(x)[1L]

  data.frame(
    trace       = seq_along(traces),
    name        = vapply(traces, function(t) scal(t[["name"]]), character(1)),
    legendgroup = vapply(traces, function(t) scal(t[["legendgroup"]]), character(1)),
    type        = vapply(traces, function(t) scal(t[["type"]]), character(1)),
    mode        = vapply(traces, function(t) scal(t[["mode"]]), character(1)),
    showlegend  = vapply(traces, function(t) isTRUE(t[["showlegend"]]), logical(1)),
    visible     = vapply(traces, function(t) {
                    v <- t[["visible"]]
                    if (is.null(v)) "TRUE"
                    else if (is.logical(v)) if (isTRUE(v[1L])) "TRUE" else "FALSE"
                    else as.character(v)[1L]
                  }, character(1)),
    xaxis       = vapply(traces, function(t) t[["xaxis"]] %||% "x", character(1)),
    yaxis       = vapply(traces, function(t) t[["yaxis"]] %||% "y", character(1)),
    color       = vapply(traces, function(t) {
                    col <- t[["line"]][["color"]] %||% t[["marker"]][["color"]] %||% t[["fillcolor"]]
                    normalize_color_string(col)
                  }, character(1)),
    stringsAsFactors = FALSE
  )
}

#' Built layout as a list, with any color-named field normalized
plotly_layout <- function(pl) {
  norm <- function(x) {
    if (is.list(x)) {
      nms <- names(x) %||% rep("", length(x))
      x[] <- Map(function(val, nm)
        if (is.character(val) && length(val) == 1L && grepl("color", nm, ignore.case = TRUE))
          normalize_color_string(val) else norm(val),
        x, nms)
    }
    x
  }
  norm(plotly::plotly_build(pl)[["x"]][["layout"]])
}

#' Check that each legend swatch matches the traces it represents
#' (and, optionally, an expected named palette)
plotly_check_legend_colors <- function(pl, expected = NULL) {
  df <- plotly_traces(pl)
  if (!nrow(df)) return(data.frame())

  df$group <- ifelse(is.na(df$legendgroup), df$name, df$legendgroup)
  df <- df[!is.na(df$group) & !is.na(df$color), , drop = FALSE]
  if (!nrow(df)) return(data.frame())

  res <- do.call(rbind, lapply(split(df, df$group), function(rows) {
    g            <- rows$group[1L]
    legend_color <- unique(rows$color[rows$showlegend])
    trace_colors <- unique(rows$color)
    out <- data.frame(
      group        = g,
      legend_color = paste(legend_color, collapse = "|"),
      trace_colors = paste(trace_colors, collapse = "|"),
      n_legend     = sum(rows$showlegend),
      ok           = length(legend_color) == 1L && all(trace_colors == legend_color),
      stringsAsFactors = FALSE
    )
    if (!is.null(expected)) {
      exp <- if (g %in% names(expected)) normalize_color_string(expected[[g]]) else NA_character_
      out$expected         <- exp
      out$matches_expected <- length(legend_color) == 1L && identical(legend_color, exp)
    }
    out
  }))
  rownames(res) <- NULL
  res
}

#' Subplot grid properties from a built plotly object
#'
#' shareX collapses x-axes within a COLUMN (observable only with >1 row);
#' shareY collapses y-axes within a ROW (observable only with >1 column).
#' Returns NA where sharing has no observable effect.
plotly_subplot_grid <- function(pl) {
  b   <- plotly::plotly_build(pl)[["x"]]
  lay <- b[["layout"]]

  xkeys <- grep("^xaxis", names(lay), value = TRUE)
  ykeys <- grep("^yaxis", names(lay), value = TRUE)
  n_xaxes <- length(xkeys)
  n_yaxes <- length(ykeys)

  # distinct horizontal extents = columns; distinct vertical extents = rows
  dom   <- function(keys) unique(lapply(keys, function(k) round(lay[[k]][["domain"]] %||% c(0, 1), 6)))
  ncols <- max(length(dom(xkeys)), 1L)
  nrows <- max(length(dom(ykeys)), 1L)

  # one panel = one distinct (xaxis, yaxis) reference
  pairs <- unique(vapply(b[["data"]], function(tr)
    paste(tr[["xaxis"]] %||% "x", tr[["yaxis"]] %||% "y"), character(1)))
  n_panels <- length(pairs)

  shareX <- if (nrows > 1L) n_xaxes == ncols else NA
  shareY <- if (ncols > 1L) n_yaxes == nrows else NA

  list(
    is_subplot = n_panels > 1L,
    nrows = nrows, ncols = ncols, n_panels = n_panels,
    n_xaxes = n_xaxes, n_yaxes = n_yaxes,
    shareX = shareX, shareY = shareY
  )
}

plotly_dedupe_legend <- function(traces) {
  if (!nrow(traces)) return(traces)

  # legend identity: legendgroup, else name; anonymous traces stay distinct
  key <- ifelse(!is.na(traces$legendgroup), traces$legendgroup, traces$name)
  na_key <- is.na(key)
  key[na_key] <- paste0("__trace", traces$trace[na_key])

  # order groups by first appearance, then put showlegend = TRUE first within each
  ord  <- order(match(key, unique(key)), !traces$showlegend)
  t2   <- traces[ord, , drop = FALSE]
  out  <- t2[!duplicated(key[ord]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

##### Helpers for plot.sdbuildR() #####
#' Extract node info from a plot.sdbuildR() diagram
#'
#' @param pl Output of plot.sdbuildR() (a DiagrammeR htmlwidget)
#' @return A data.frame, one row per node, with a `name` column plus one
#'   column per attribute found (id, label, xlabel, tooltip, ...). Missing
#'   attributes are NA.
extract_diagram_nodes <- function(pl) {
  diagram <- pl[["x"]][["diagram"]]
  if (is.null(diagram)) stop("No diagram string found in plot object.")

  # Match node definitions "name" [ ... ] across the whole string. The bracket
  # body may span multiple lines because long (wrapped) labels can contain
  # embedded newlines. Edges ("a" -> "b") have no bracket, and style lines
  # (node [...]/edge [...]) have no quoted name, so both are excluded.
  defs <- regmatches(
    diagram,
    gregexpr('"[^"]+"\\s*\\[[^\\]]*\\]', diagram, perl = TRUE)
  )[[1]]
  if (length(defs) == 0L) return(data.frame())

  parsed <- lapply(defs, function(def) {
    parts     <- regmatches(def, regexec('(?s)^"([^"]+)"\\s*\\[(.*)\\]$', def, perl = TRUE))[[1]]
    name      <- parts[2]
    attrs_str <- parts[3]

    # key = value, where value is "...", '...', or <...>. A negated class like
    # [^"] also matches newlines, so values with embedded \n are captured whole.
    pat <- '(\\w+)\\s*=\\s*("[^"]*"|\'[^\']*\'|<[^>]*>)'
    m   <- regmatches(attrs_str, gregexpr(pat, attrs_str, perl = TRUE))[[1]]

    kv <- list(name = name)
    for (pair in m) {
      key <- trimws(sub('(?s)\\s*=.*$', "", pair, perl = TRUE))
      val <- sub('(?s)^[^=]*=\\s*', "", pair, perl = TRUE)
      val <- sub('(?s)^["\'<](.*)["\'>]$', "\\1", val, perl = TRUE)  # strip delimiters
      # NB: embedded newlines in (wrapped) labels are preserved, so callers can
      # verify wrapping, e.g. grepl("\n", nodes$label).
      kv[[key]] <- trimws(val)
    }
    kv
  })

  # Bind rows on the union of all attribute names
  all_cols <- unique(c("name", unlist(lapply(parsed, names))))
  df <- do.call(rbind, lapply(parsed, function(x) {
    row <- setNames(rep(NA_character_, length(all_cols)), all_cols)
    row[names(x)] <- unlist(x)
    as.data.frame(as.list(row), stringsAsFactors = FALSE)
  }))
  rownames(df) <- NULL
  df
}


#' Extract edge info from a plot.sdbuildR() diagram
#'
#' @param pl Output of plot.sdbuildR()
#' @return A data.frame with `from`, `to`, an edge `type` (from the section
#'   comment), plus one column per shared edge attribute (color, arrowhead, ...).
extract_diagram_edges <- function(pl) {
  diagram <- pl[["x"]][["diagram"]]
  if (is.null(diagram)) stop("No diagram string found in plot object.")

  lines <- trimws(unlist(strsplit(diagram, "\n")))

  parse_attrs <- function(attrs_str) {
    pat <- '(\\w+)\\s*=\\s*("[^"]*"|\'[^\']*\'|<[^>]*>|[^,\\]]+)'
    m   <- regmatches(attrs_str, gregexpr(pat, attrs_str, perl = TRUE))[[1]]
    kv <- list()
    for (pair in m) {
      key <- trimws(sub('\\s*=.*$', "", pair))
      val <- sub('^[^=]*=\\s*', "", pair)
      val <- sub('^["\'<](.*)["\'>]$', "\\1", val)
      kv[[key]] <- trimws(val)
    }
    kv
  }

  current_type  <- NA_character_
  current_attrs <- list()
  rows <- list()

  for (line in lines) {
    if (grepl("^#", line)) {                         # section comment -> edge type
      current_type <- trimws(sub("^#\\s*", "", line))
    } else if (grepl("^edge\\s*\\[", line)) {        # shared style for the group
      current_attrs <- parse_attrs(sub('^edge\\s*\\[(.*)\\]\\s*$', "\\1", line))
    } else if (grepl('"[^"]+"\\s*->\\s*"[^"]+"', line)) {  # an actual edge
      m <- regmatches(line, regexec('"([^"]+)"\\s*->\\s*"([^"]+)"', line))[[1]]
      rows[[length(rows) + 1L]] <- c(
        list(from = m[2], to = m[3], type = current_type),
        current_attrs
      )
    }
  }
  if (length(rows) == 0L) return(data.frame())

  all_cols <- unique(unlist(lapply(rows, names)))
  df <- do.call(rbind, lapply(rows, function(x) {
    row <- setNames(rep(NA_character_, length(all_cols)), all_cols)
    row[names(x)] <- unlist(x)
    as.data.frame(as.list(row), stringsAsFactors = FALSE)
  }))
  rownames(df) <- NULL
  df
}



# Helper models used across import-export tests
logistic_model_deSolve <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN <- r * N * (1 - N / K)
    list(c(dN))
  })
}

sir_model_deSolve <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    SI <- beta * S * I / N
    IR <- gamma * I
    dS <- -SI
    dI <- SI - IR
    dR <- IR
    list(c(dS, dI, dR))
  })
}
