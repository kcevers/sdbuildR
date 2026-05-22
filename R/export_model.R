#' Export a stock-and-flow model
#'
#' Export a model of class [`sdbuildR`][sdbuildR] to another format.
#' ## Psychomodels format (`format = "psychomodels"`)
#' Generates a JSON record for upload to
#' [Psychomodels](https://www.psychomodels.org/models/).
#' When `file = NULL`, returns a JSON character string.
#' When `file` is provided, writes a `.json` file and returns the path invisibly.
#' If `file` has no `.json` extension, one is appended.
#'
#' @inheritParams update.sdbuildR
#' @param format Export format. Currently `"psychomodels"`.
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
#' @returns For `format = "psychomodels"` with `file = NULL`: a JSON character string.
#' For `format = "psychomodels"` with `file` specified: invisibly returns the file path.
#' @export
#' @concept importExport
#' @seealso [import_insightmaker()]
#'
#' @examples
#' sfm <- sdbuildR("SIR")
#'
#' # Export to Psychomodels JSON
#' json <- export_model(sfm,
#'   format = "psychomodels",
#'   publication_doi = "10.0000/example"
#' )
export_model <- function(object,
                         format = c("psychomodels"),
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

  format <- match.arg(tolower(format), c("psychomodels"))

  switch(format,
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

.fmt_num <- function(x) {
  x <- as.numeric(x)
  if (x == round(x)) as.character(as.integer(x)) else as.character(x)
}


.validate_r_path <- function(path) {
  if (!grepl("\\.R$", path, ignore.case = TRUE)) path <- paste0(path, ".R")
  path
}
