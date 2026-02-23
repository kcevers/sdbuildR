#' Create or modify custom variables or functions
#'
#' Custom functions are user-defined functions that can be used
#' throughout a stock-and-flow model. [custom_func()] adds or changes a function. This is a convenience wrapper around [build()] with
#' `type = "func"`.
#'
#' @inheritParams build
#' @param name Name of the function variable. The equation will be assigned to this name.
#' @param eqn Equation of the function variable. A character vector. Defaults to "0.0".
#' @param doc Documentation. Defaults to "".
#'
#' @returns A stock-and-flow model object of class [`sdbuildR`][sdbuildR]
#' @seealso [build()], [discard()], [change_name()]
#' @concept build
#' @export
#'
#' @examples
#'
#' # Simple function
#' sfm <- sdbuildR() |>
#'   custom_func(double, eqn = "function(x) x * 2") |>
#'   build(a, constant, eqn = double(2))
#'
#' # Function with defaults
#' sfm <- sdbuildR() |>
#'   custom_func(scale, eqn = "function(x, factor = 10) x * factor") |>
#'   build(b, constant, eqn = scale(2))
#'
#' # If the logistic() function did not exist, you could create it yourself:
#' sfm <- sdbuildR() |>
#'        custom_func(my_logistic, eqn = "function(x, slope = 1, midpoint = .5){
#'    1 / (1 + exp(-slope*(x-midpoint)))
#'  }") |>
#'   build(c_, constant, eqn = my_logistic(2, slope = 50))
#'
custom_func <- function(sfm, name, eqn = "0.0", 
                units = "1", label = name, doc = "") {
  cl <- match.call()
  cl[[1]] <- quote(build)
  cl$type <- "func"
  eval.parent(cl)
}
