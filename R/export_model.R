#' Export a stock-and-flow model
#'
#' Export a model of class [`sdbuildR`][sdbuildR] to another format.
#' Currently supported formats: `"desolve"` and `"psychomodels"`.
#' The `format` argument is case-insensitive, so `"deSolve"` and `"Psychomodels"`
#' are also accepted.
#'
#' ## deSolve format (`format = "desolve"`)
#' Generates a ready-to-run [deSolve](https://cran.r-project.org/package=deSolve)
#' model. Constants become `params`, stock initial values become `init`, and
#' simulation time settings become `times`. Auxiliary variables appear as
#' intermediate calculations inside the `with()` block, and flows appear as
#' named intermediate variables summed into each stock's derivative.
#'
#' When `file = NULL`, returns a named list with elements `model` (an R
#' function), `params`, `init`, `times`, and `method`.
#' When `file` is provided, writes an `.R` script and returns the path invisibly.
#' If `file` has no `.R` extension, one is appended.
#'
#' ## Psychomodels format (`format = "psychomodels"`)
#' Generates a JSON record for upload to
#' [Psychomodels](https://www.psychomodels.org/models/).
#' When `file = NULL`, returns a JSON character string.
#' When `file` is provided, writes a `.json` file and returns the path invisibly.
#' If `file` has no `.json` extension, one is appended.
#'
#' @inheritParams update.sdbuildR
#' @param format Export format. One of `"desolve"` or `"psychomodels"` (case-insensitive).
#' @param file Output file path, or `NULL` to return the result directly.
#' @param title \[psychomodels\] Model title. Defaults to `object[["meta"]][["name"]]`.
#' @param description \[psychomodels\] Model description.
#'   Defaults to `object[["meta"]][["caption"]]`.
#' @param explanation \[psychomodels\] Free-text explanation. Defaults to `description`.
#' @param publication_doi \[psychomodels\] DOI for the associated publication.
#' @param publication_citation \[psychomodels\] Citation text.
#' @param framework \[psychomodels\] Modeling framework.
#'   Defaults to `"Ordinary Differential Equations"`.
#' @param programming_language \[psychomodels\] Programming language.
#' @param psychology_discipline \[psychomodels\] Discipline id(s), comma-separated.
#' @param software_package \[psychomodels\] Package id(s), comma-separated.
#' @param model_variable \[psychomodels\] Variable id(s), comma-separated.
#' @param code_repository_url \[psychomodels\] URL to code repository.
#' @param data_url \[psychomodels\] URL to model data.
#' @param submission_remarks \[psychomodels\] Optional remarks.
#' @param created_by \[psychomodels\] Identifier of creating user.
#' @param updated_by \[psychomodels\] Identifier of last updating user.
#' @param published_by \[psychomodels\] Identifier of publishing user.
#' @param published_at \[psychomodels\] Publication timestamp. Defaults to current time.
#' @param published_pending_moderation_at \[psychomodels\] Moderation timestamp.
#' @param publication_citation_fetched_at \[psychomodels\] Citation fetch timestamp.
#' @param publication_csl_fetched_at \[psychomodels\] CSL fetch timestamp.
#' @param publication_csl_json \[psychomodels\] CSL JSON text.
#' @param id \[psychomodels\] Optional record id.
#' @param slug \[psychomodels\] Optional slug. Generated from `title` if `NULL`.
#' @param include_latex \[psychomodels\] If `TRUE`, append LaTeX equations to explanation.
#' @param pretty \[psychomodels\] If `TRUE`, pretty-print output JSON.
#'
#' @returns
#' For `format = "desolve"` with `file = NULL`: a named list with elements
#' `model` (function), `params`, `init`, `times`, `method`.
#' For `format = "desolve"` with `file` specified: invisibly returns the file path.
#'
#' For `format = "psychomodels"` with `file = NULL`: a JSON character string.
#' For `format = "psychomodels"` with `file` specified: invisibly returns the file path.
#' @export
#' @concept importExport
#' @seealso [import_insightmaker()], [import_desolve()]
#'
#' @examples
#' sfm <- sdbuildR("SIR")
#'
#' # Export to deSolve (returns list)
#' ds <- export_model(sfm, format = "desolve")
#' str(ds[c("params", "init", "times", "method")])
#'
#' # Export to Psychomodels JSON
#' json <- export_model(sfm,
#'   format = "psychomodels",
#'   publication_doi = "10.0000/example"
#' )
export_model <- function(object,
                         format = c("desolve", "psychomodels"),
                         file = NULL,
                         # psychomodels-specific params
                         title = object[["meta"]][["name"]],
                         description = object[["meta"]][["caption"]],
                         explanation = description,
                         publication_doi = "",
                         publication_citation = "",
                         framework = "Ordinary Differential Equations",
                         programming_language = "R",
                         psychology_discipline = "",
                         software_package = "",
                         model_variable = "",
                         code_repository_url = "",
                         data_url = "",
                         submission_remarks = "",
                         created_by = "",
                         updated_by = "",
                         published_by = "",
                         published_at = Sys.time(),
                         published_pending_moderation_at = Sys.time(),
                         publication_citation_fetched_at = Sys.time(),
                         publication_csl_fetched_at = Sys.time(),
                         publication_csl_json = "",
                         id = NA,
                         slug = NULL,
                         include_latex = TRUE,
                         pretty = TRUE) {
  if (missing(object)) {
    missing_arg("object")
  }
  check_sdbuildR(object)

  format <- match.arg(tolower(format), c("desolve", "psychomodels"))

  switch(format,
    desolve = .export_desolve(object, file = file),
    psychomodels = export_psychomodels(
      object                            = object,
      include_latex                     = include_latex,
      destfile                          = file,
      title                             = title,
      description                       = description,
      explanation                       = explanation,
      publication_doi                   = publication_doi,
      publication_citation              = publication_citation,
      framework                         = framework,
      programming_language              = programming_language,
      psychology_discipline             = psychology_discipline,
      software_package                  = software_package,
      model_variable                    = model_variable,
      code_repository_url               = code_repository_url,
      data_url                          = data_url,
      submission_remarks                = submission_remarks,
      created_by                        = created_by,
      updated_by                        = updated_by,
      published_by                      = published_by,
      published_at                      = published_at,
      published_pending_moderation_at   = published_pending_moderation_at,
      publication_citation_fetched_at   = publication_citation_fetched_at,
      publication_csl_fetched_at        = publication_csl_fetched_at,
      publication_csl_json              = publication_csl_json,
      id                                = id,
      slug                              = slug,
      pretty                            = pretty
    )
  )
}


# ---- deSolve export internals ----

.is_numeric_eqn <- function(x) {
  !is.na(suppressWarnings(as.numeric(trimws(x))))
}

.export_desolve <- function(object, file = NULL) {
  vars <- object[["variables"]]
  specs <- object[["sim_specs"]]

  stocks <- vars[vars$type == "stock", ]
  constants <- vars[vars$type == "constant", ]
  auxvars <- vars[vars$type == "aux", ]
  flows <- vars[vars$type == "flow", ]

  method_val <- specs[["method"]] %||% "lsoda"

  # Split constants: simple (numeric literal) go in params; computed (expression)
  # go in the with() block so they can reference other named constants.
  is_simple <- vapply(constants$eqn, .is_numeric_eqn, logical(1L))
  simple_c <- constants[is_simple, ]
  computed_c <- constants[!is_simple, ]

  # ---- params vector code ----
  params_code <- if (nrow(simple_c) > 0L) {
    pairs <- paste0("  ", simple_c$name, " = ", simple_c$eqn)
    paste0("params <- c(\n", paste(pairs, collapse = ",\n"), "\n)")
  } else {
    "params <- c()"
  }

  # ---- init vector code ----
  init_code <- if (nrow(stocks) > 0L) {
    pairs <- paste0("  ", stocks$name, " = ", stocks$eqn)
    paste0("init <- c(\n", paste(pairs, collapse = ",\n"), "\n)")
  } else {
    "init <- c()"
  }

  # ---- times code ----
  times_code <- sprintf(
    "times <- seq(%s, %s, by = %s)",
    .fmt_num(specs[["start"]]),
    .fmt_num(specs[["stop"]]),
    .fmt_num(specs[["dt"]])
  )

  # ---- with() body lines: computed constants → aux → flows → derivatives ----
  computed_c_lines <- if (nrow(computed_c) > 0L) {
    paste0("    ", computed_c$name, " <- ", computed_c$eqn)
  } else {
    character(0)
  }

  aux_lines <- if (nrow(auxvars) > 0L) {
    paste0("    ", auxvars$name, " <- ", auxvars$eqn)
  } else {
    character(0)
  }

  flow_lines <- if (nrow(flows) > 0L) {
    paste0("    ", flows$name, " <- ", flows$eqn)
  } else {
    character(0)
  }

  deriv_lines <- vapply(stocks$name, function(sname) {
    in_flows <- flows$name[nzchar(flows$to) & flows$to == sname]
    out_flows <- flows$name[nzchar(flows$from) & flows$from == sname]
    parts <- c(
      in_flows,
      if (length(out_flows) > 0L) paste0("-(", out_flows, ")")
    )
    rhs <- if (length(parts) > 0L) paste(parts, collapse = " + ") else "0"
    paste0("    d", sname, " <- ", rhs)
  }, character(1L))

  deriv_names <- paste0("d", stocks$name)
  list_line <- paste0("    list(c(", paste(deriv_names, collapse = ", "), "))")

  body_lines <- c(
    if (length(computed_c_lines) > 0L) c(computed_c_lines, ""),
    if (length(aux_lines) > 0L) c(aux_lines, ""),
    if (length(flow_lines) > 0L) c(flow_lines, ""),
    deriv_lines,
    "",
    list_line
  )

  model_code <- paste0(
    "model <- function(t, state, parameters) {\n",
    "  with(as.list(c(state, parameters)), {\n",
    paste(body_lines, collapse = "\n"), "\n",
    "  })\n",
    "}"
  )

  run_code <- sprintf(
    "out <- deSolve::ode(y = init, times = times, func = model,\n       parms = params, method = %s)",
    deparse(method_val)
  )

  script <- paste(c(
    paste0("# deSolve model exported from sdbuildR"),
    paste0("# Model: ", object[["meta"]][["name"]]),
    "",
    "library(deSolve)",
    "",
    params_code,
    "",
    init_code,
    "",
    times_code,
    "",
    model_code,
    "",
    run_code
  ), collapse = "\n")

  if (!is.null(file)) {
    file <- .validate_r_path(file)
    writeLines(script, con = file)
    return(invisible(file))
  }

  list(
    model = .build_desolve_fn(stocks, computed_c, auxvars, flows),
    params = stats::setNames(as.numeric(simple_c$eqn), simple_c$name),
    init = stats::setNames(as.numeric(stocks$eqn), stocks$name),
    times = seq(as.numeric(specs[["start"]]),
      as.numeric(specs[["stop"]]),
      by = as.numeric(specs[["dt"]])
    ),
    method = method_val
  )
}


.build_desolve_fn <- function(stocks, computed_c, auxvars, flows) {
  inner_stmts <- list(as.name("{"))

  # Computed constants (expressions) first — they reference only params/state
  for (i in seq_len(nrow(computed_c))) {
    inner_stmts <- c(
      inner_stmts,
      list(call(
        "<-", as.name(computed_c$name[[i]]),
        parse(text = computed_c$eqn[[i]])[[1L]]
      ))
    )
  }

  for (i in seq_len(nrow(auxvars))) {
    inner_stmts <- c(
      inner_stmts,
      list(call(
        "<-", as.name(auxvars$name[[i]]),
        parse(text = auxvars$eqn[[i]])[[1L]]
      ))
    )
  }

  for (i in seq_len(nrow(flows))) {
    inner_stmts <- c(
      inner_stmts,
      list(call(
        "<-", as.name(flows$name[[i]]),
        parse(text = flows$eqn[[i]])[[1L]]
      ))
    )
  }

  for (sname in stocks$name) {
    in_flows <- flows$name[nzchar(flows$to) & flows$to == sname]
    out_flows <- flows$name[nzchar(flows$from) & flows$from == sname]
    parts <- c(
      in_flows,
      if (length(out_flows) > 0L) paste0("-(", out_flows, ")")
    )
    rhs_str <- if (length(parts) > 0L) paste(parts, collapse = " + ") else "0"
    inner_stmts <- c(
      inner_stmts,
      list(call(
        "<-", as.name(paste0("d", sname)),
        parse(text = rhs_str)[[1L]]
      ))
    )
  }

  deriv_syms <- lapply(paste0("d", stocks$name), as.name)
  return_expr <- as.call(c(
    list(as.name("list")),
    as.call(c(list(as.name("c")), deriv_syms))
  ))
  inner_stmts <- c(inner_stmts, list(return_expr))
  inner_block <- as.call(inner_stmts)

  with_call <- call(
    "with",
    call("as.list", call("c", as.name("state"), as.name("parameters"))),
    inner_block
  )

  fn_body <- call("{", with_call)

  fn <- function(t, state, parameters) NULL
  body(fn) <- fn_body
  fn
}


.fmt_num <- function(x) {
  x <- as.numeric(x)
  if (x == round(x)) as.character(as.integer(x)) else as.character(x)
}


.validate_r_path <- function(path) {
  if (!grepl("\\.R$", path, ignore.case = TRUE)) path <- paste0(path, ".R")
  path
}
