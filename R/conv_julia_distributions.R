#' Distribution and sequence conversion helpers for Julia
#'
#' Internal helper functions for converting random distributions and sequences
#' from R to Julia code


#' Reparameterize rate-based R distributions for Julia
#'
#' R's `rexp()`/`rgamma()` (and their `d`/`p`/`q` variants) use a `rate`
#' parameter, whereas Julia's `Distributions.Exponential`/`Distributions.Gamma`
#' use `scale = 1 / rate`. `sort_args()` already resolves Gamma's `scale` (its
#' formals include `scale = 1 / rate`), so we drop the now-redundant `rate`.
#' Exponential has no `scale` formal, so we invert `rate` directly.
#'
#' Operates on a *named* argument list (the real conversion path). It is a
#' no-op for unnamed input, e.g. when `conv_distribution_julia()` is called
#' directly with a plain character vector in unit tests.
#'
#' @param arg List of arguments, named in R's formal order
#' @param distribution String with Julia distribution call
#'
#' @returns List of arguments with Julia-compatible parameterization
#' @noRd
#'
reparam_rate_distribution <- function(arg, distribution) {
  if (endsWith(distribution, "Exponential") && is_defined(arg[["rate"]])) {
    # Julia's Exponential takes scale = 1 / rate
    arg[["rate"]] <- sprintf("1 / (%s)", arg[["rate"]])
  } else if (endsWith(distribution, "Gamma")) {
    # sort_args() already computed scale = 1 / rate; drop the redundant rate
    arg[["rate"]] <- NULL
  }
  arg
}


#' Convert random number generation in R to Julia
#'
#' @inheritParams sort_args
#' @param julia_func String with Julia function
#' @param R_func String with R function, e.g., "rnorm()".
#' @param distribution String with Julia distribution call
#'
#' @returns String with Julia code
#' @noRd
#' @keywords internal
#'
conv_distribution <- function(arg, R_func, julia_func, distribution) {
  conv_distribution_julia(arg, R_func, julia_func, distribution)
}


#' Convert random number generation in R to Julia
#'
#' @inheritParams sort_args
#' @param julia_func String with Julia function
#' @param R_func String with R function, e.g., "runif", "rnorm"
#' @param distribution String with Julia distribution call
#' @param arg Character vector with parsed arguments
#'
#' @returns String with Julia code
#' @noRd
#'
conv_distribution_julia <- function(arg, R_func, julia_func, distribution) {
  # The first argument must be an integer
  arg <- as.list(arg)
  arg[[1]] <- safe_convert(arg[[1]], "integer")

  if (!is.integer(arg[[1]])) {
    cli::cli_abort(c(
      "x" = "Invalid first argument of {.fn {R_func}}.",
      "i" = "Must be {.cls integer}."
    ), call. = FALSE)
  }

  # R parameterizes Exponential/Gamma by rate; Julia uses scale = 1 / rate.
  # Acts on the named argument list; a no-op for unnamed input.
  arg <- reparam_rate_distribution(arg, distribution)
  arg <- unname(arg)

  # If n = 1, don't include it, as rand(..., 1) generates a vector. n is the first argument.
  julia_str <- sprintf(
    "%s(%s(%s), %d)",
    julia_func, distribution,
    # Don't include names of arguments
    paste0(arg[-1], collapse = ", "), arg[[1]]
  )

  if (arg[1] == 1 && julia_func == "rand") {
    julia_str <- sprintf(
      "%s(%s(%s))",
      julia_func, distribution,
      # Don't include names of arguments
      paste0(arg[-1], collapse = ", ")
    )
  } else if (julia_func == "Distributions.cdf.") {
    # log = TRUE
    if (arg[length(arg)] == "TRUE") {
      julia_str <- sprintf(
        "log%s(%s(%s), %d)",
        julia_func, distribution,
        # Don't include names of arguments; skip log
        paste0(arg[-c(1, length(arg) - 1, length(arg))], collapse = ", "), arg[[1]]
      )
    } else {
      julia_str <- sprintf(
        "%s(%s(%s), %d)",
        julia_func, distribution,
        # Don't include names of arguments; skip log
        paste0(arg[-c(1, length(arg) - 1, length(arg))], collapse = ", "), arg[[1]]
      )
    }
  } else if (julia_func == "Distributions.pdf.") {
    # log.p = TRUE
    if (arg[length(arg)] == "TRUE") {
      julia_str <- sprintf(
        "log%s(%s(%s), %d)",
        julia_func, distribution,
        # Don't include names of arguments; skip lower.tail and log.p
        paste0(arg[-c(1, length(arg))], collapse = ", "), arg[[1]]
      )
    } else {
      julia_str <- sprintf(
        "%s(%s(%s), %d)",
        julia_func, distribution,
        # Don't include names of arguments; skip lower.tail and log.p
        paste0(arg[-c(1, length(arg))], collapse = ", "), arg[[1]]
      )
    }
  } else if (julia_func == "Distributions.quantile.") {
    # log = TRUE
    if (arg[length(arg)] == "TRUE") {
      julia_str <- sprintf(
        "invlogcdf(%s(%s), %d)",
        distribution,
        # Don't include names of arguments; skip lower.tail and log.p
        paste0(arg[-c(1, length(arg) - 1, length(arg))], collapse = ", "), arg[[1]]
      )
    } else {
      julia_str <- sprintf(
        "%s(%s(%s), %d)",
        julia_func, distribution,
        # Don't include names of arguments; skip lower.tail and log.p
        paste0(arg[-c(1, length(arg) - 1, length(arg))], collapse = ", "), arg[[1]]
      )
    }
  }

  return(julia_str)
}


#' Convert sequence in R to Julia
#'
#' @param arg Named list with parsed arguments
#' @param R_func String with R function, e.g., "seq", "seq_along"
#' @param julia_func String with Julia function
#'
#' @returns String with Julia code
#' @noRd
#'
conv_seq_julia <- function(arg, R_func, julia_func) {
  if (R_func == "seq_along") {
    julia_str <- paste0(julia_func, "(1.0, length(", arg[["along.with"]], "))")
  } else if (R_func == "seq_len") {
    julia_str <- paste0(julia_func, "(1.0, ", arg[["length.out"]], ")")
  } else {
    # If nothing is specified, specify by
    if (!is_defined(arg[["by"]]) && !is_defined(arg[["length.out"]]) &&
      !is_defined(arg[["along.with"]])) {
      arg[["by"]] <- "1.0" # Default value for by
    }

    if (is_defined(arg[["by"]])) {
      julia_str <- sprintf(
        "%s(%s, %s, step=%s)",
        julia_func, arg[["from"]], arg[["to"]], arg[["by"]]
      )
    } else if (is_defined(arg[["length.out"]])) {
      # Julia throws an error in this case
      if (as.numeric(arg[["length.out"]]) == 1 &&
        as.numeric(arg[["from"]]) != as.numeric(arg[["to"]])) {
        julia_str <- arg[["from"]]
      } else {
        # length.out should be an integer
        julia_str <- sprintf(
          "%s(%s, %s, round_(%s))",
          julia_func, arg[["from"]], arg[["to"]], arg[["length.out"]]
        )
      }
    } else if (is_defined(arg[["along.with"]])) {
      julia_str <- sprintf(
        "%s(%s, %s, length(%s))",
        julia_func, arg[["from"]], arg[["to"]], arg[["along.with"]]
      )
    }
  }

  return(julia_str)
}


#' Convert sequence in R to Julia
#'
#' @inheritParams sort_args
#' @param R_func String with R function, e.g., "seq", "seq_along"
#' @param julia_func String with Julia function
#'
#' @returns String with Julia code
#' @noRd
#'
conv_seq <- function(arg, R_func, julia_func) {
  conv_seq_julia(arg, R_func, julia_func)
}


#' Convert R sample() to Julia StatsBase.sample()
#'
#' @param arg Named list with parsed arguments
#' @param R_func String with R function (sample or sample.int)
#' @param julia_func String with Julia function
#'
#' @returns String with Julia code
#' @noRd
#'
conv_sample_julia <- function(arg, R_func, julia_func) {
  # Order in StatsBase.sample() is different
  if (R_func == "sample.int") {
    arg[["x"]] <- paste0("seq(1.0, ", arg[["n"]], ")")
  }

  arg[["replace"]] <- ifelse(tolower(arg[["replace"]]) == "true", "true", "false")

  if (is_defined(arg[["prob"]])) {
    julia_str <- sprintf(
      "%s(%s, StatsBase.pweights(%s), round_(%s), replace=%s)",
      julia_func, arg[["x"]], arg[["prob"]], arg[["size"]], arg[["replace"]]
    )
  } else {
    julia_str <- sprintf(
      "%s(%s, round_(%s), replace=%s)",
      julia_func, arg[["x"]], arg[["size"]], arg[["replace"]]
    )
  }

  return(julia_str)
}


#' Convert R sample() to Julia StatsBase.sample()
#'
#' @inheritParams conv_seq
#'
#' @returns String with Julia code
#' @noRd
conv_sample <- function(arg, R_func, julia_func) {
  conv_sample_julia(arg, R_func, julia_func)
}
