#' Export stock-and-flow model to Psychomodels JSON
#'
#' Convert a model of class [sdbuildR()] to a JSON format which can be uploaded to [Psychomodels](https://www.psychomodels.org/models/).
#' The output can be returned as JSON text or written to a `.json` file.
#' Optionally appends a LaTeX equations section to the explanation field.
#'
#' @inheritParams update.sdbuildR
#' @param title Model title. Defaults to `object[["meta"]][["name"]]`.
#' @param description Model description.
#'  Defaults to `object[["meta"]][["caption"]]`.
#' @param explanation Free-text explanation. Defaults to `description`.
#' @param publication_doi DOI for the associated publication.
#' @param publication_citation Citation text for the publication.
#' @param framework Modeling framework text.
#'  Defaults to `"Ordinary Differential Equations"`.
#' @param programming_language Programming language text.
#' @param psychology_discipline Psychology discipline id(s) as comma-separated string.
#' @param software_package Software package id(s) as comma-separated string.
#' @param model_variable Model variable id(s) as comma-separated string.
#' @param code_repository_url URL to model code repository.
#' @param data_url URL to model data.
#' @param submission_remarks Optional submission remarks.
#' @param created_by Identifier of creating user.
#' @param updated_by Identifier of last updating user.
#' @param published_by Identifier of publishing user.
#' @param published_at Publication timestamp. Defaults to current time.
#' @param published_pending_moderation_at Moderation timestamp. Defaults to current time.
#' @param publication_citation_fetched_at Citation fetch timestamp. Defaults to current time.
#' @param publication_csl_fetched_at CSL fetch timestamp. Defaults to current time.
#' @param publication_csl_json CSL JSON text.
#' @param id Optional record id.
#' @param slug Optional slug. If NULL, generated from `title`.
#' @param include_latex If `TRUE`, append generated LaTeX equations to explanation.
#' @param destfile Output file path. Must have extension `.json` or no extension.
#'  If not provided, return model in JSON format.
#' @param pretty If `TRUE`, pretty-print output JSON.
#'
#' @returns If `destfile` is not provided; JSON string.
#'  If `destfile` provided, invisibly returns destination file path.
#' @export
#'
#' @examples
#' sfm <- sdbuildR("SIR")
#' json <- sfm_to_psychomodels(
#'   sfm,
#'   publication_doi = "10.0000/example"
#' )
sfm_to_psychomodels <- function(
		object,
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
		destfile = NULL,
		pretty = TRUE
) {
	if (missing(object)) {
		missing_arg("object")
	}

	check_sdbuildR(object)

	.check_scalar_character(publication_doi, "publication_doi")
	.check_scalar_character(publication_citation, "publication_citation")
	.check_scalar_character(framework, "framework")
	.check_scalar_character(programming_language, "programming_language")
	.check_scalar_character(psychology_discipline, "psychology_discipline")
	.check_scalar_character(software_package, "software_package")
	.check_scalar_character(model_variable, "model_variable")
	.check_scalar_character(code_repository_url, "code_repository_url")
	.check_scalar_character(data_url, "data_url")
	.check_scalar_character(submission_remarks, "submission_remarks")
	.check_scalar_character(created_by, "created_by")
	.check_scalar_character(updated_by, "updated_by")
	.check_scalar_character(published_by, "published_by")
	.check_scalar_character(publication_csl_json, "publication_csl_json")

	if (!is.logical(include_latex) || length(include_latex) != 1 || is.na(include_latex)) {
		cli::cli_abort(c(
			"Invalid {.arg include_latex} argument.",
			"x" = "The {.arg include_latex} argument must be {.code TRUE} or {.code FALSE}."
		))
	}

	if (!is.logical(pretty) || length(pretty) != 1 || is.na(pretty)) {
		cli::cli_abort(c(
			"Invalid {.arg pretty} argument.",
			"x" = "The {.arg pretty} argument must be {.code TRUE} or {.code FALSE}."
		))
	}

	meta <- object[["meta"]]

	.check_scalar_character(title, "title", allow_empty = FALSE)
	.check_scalar_character(description, "description")
	.check_scalar_character(explanation, "explanation")

	if (is.null(slug)) {
		slug <- .slugify(title)
	}
	.check_scalar_character(slug, "slug", allow_empty = FALSE)

	if (include_latex) {
		eqn_block <- .psychomodels_latex_block(object)
		if (nzchar(eqn_block)) {
			explanation <- paste0(explanation, "\n\n", eqn_block)
		}
	}

	now <- Sys.time()

	record <- list(
		id = id,
		title = title,
		description = description,
		explanation = explanation,
		publication_doi = publication_doi,
		publication_citation = publication_citation,
		publication_citation_fetched_at = .format_psychomodel_time(publication_citation_fetched_at %||% now),
		publication_csl_fetched_at = .format_psychomodel_time(publication_csl_fetched_at %||% now),
		publication_csl_json = publication_csl_json,
		code_repository_url = code_repository_url,
		data_url = data_url,
		submission_remarks = submission_remarks,
		framework = framework,
		programming_language = programming_language,
		model_variable = model_variable,
		psychology_discipline = psychology_discipline,
		software_package = software_package,
		slug = slug,
		published_pending_moderation_at = .format_psychomodel_time(published_pending_moderation_at %||% now),
		published_at = .format_psychomodel_time(published_at %||% now),
		created_at = .format_psychomodel_time(meta[["created"]] %||% now),
		updated_at = .format_psychomodel_time(now),
		created_by = created_by,
		updated_by = updated_by,
		published_by = published_by
	)

	if (!is.null(destfile)) {
		destfile <- validate_json_path(destfile)
		jsonlite::write_json(
			record,
			path = destfile,
			auto_unbox = TRUE,
			pretty = pretty,
			na = "null",
			null = "null"
		)
		return(invisible(destfile))
	}

	out <- jsonlite::toJSON(
		record,
		auto_unbox = TRUE,
		pretty = pretty,
		na = "null",
		null = "null"
	)
	as.character(out)
}


.check_scalar_character <- function(x, arg_name, allow_empty = TRUE) {
	if (!is.character(x) || length(x) != 1 || is.na(x)) {
		cli::cli_abort(c(
			"Invalid {.arg {arg_name}} argument.",
			"x" = "The {.arg {arg_name}} argument must be a single {.cls character} string."
		))
	}

	if (!allow_empty && !nzchar(trimws(x))) {
		cli::cli_abort(c(
			"Invalid {.arg {arg_name}} argument.",
			"x" = "The {.arg {arg_name}} argument cannot be empty."
		))
	}

	invisible(TRUE)
}


.slugify <- function(x) {
	x <- trimws(tolower(x))
	x <- gsub("[^a-z0-9]+", "-", x)
	x <- gsub("^-+|-+$", "", x)
	if (!nzchar(x)) {
		x <- "model"
	}
	x
}


.format_psychomodel_time <- function(x) {
	# Psychomodel fixtures use a simple YYYY-mm-dd HH:MM:SS timestamp format.
	if (inherits(x, c("POSIXct", "POSIXt", "Date"))) {
		x <- as.POSIXct(x, tz = "UTC")
		return(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
	}

	if (is.character(x) && length(x) == 1 && !is.na(x) && nzchar(trimws(x))) {
		return(x)
	}

	cli::cli_abort(c(
		"Invalid timestamp value.",
		"x" = "Expected a {.cls POSIXct}, {.cls Date}, or non-empty {.cls character} timestamp string."
	))
}


.latex_escape <- function(x) {
	x <- gsub("\\\\", "\\\\textbackslash{}", x)
	x <- gsub("_", "\\\\_", x, fixed = TRUE)
	x <- gsub("%", "\\\\%", x, fixed = TRUE)
	x
}


.eqn_to_latex <- function(x) {
	x <- .latex_escape(x)
	x <- gsub("<=", "\\\\leq ", x, fixed = TRUE)
	x <- gsub(">=", "\\\\geq ", x, fixed = TRUE)
	x <- gsub("!=", "\\\\neq ", x, fixed = TRUE)
	x <- gsub("\\*", " \\\\cdot ", x)
	x
}


.psychomodels_latex_block <- function(object) {
	df <- object[["variables"]]

	if (nrow(df) == 0) {
		return("")
	}

	if (!"eqn" %in% names(df)) {
		return("")
	}

	if (!"sum_eqn" %in% names(df)) {
		df[["sum_eqn"]] <- ""
	}

	eqn <- ifelse(df[["type"]] == "stock" & nzchar(df[["sum_eqn"]]), df[["sum_eqn"]], df[["eqn"]])
	keep <- nzchar(eqn)

	if (!any(keep)) {
		return("")
	}

	lines <- vapply(which(keep), function(i) {
		lhs <- .latex_escape(df[["name"]][i])
		rhs <- .eqn_to_latex(eqn[i])
		paste0("$$", lhs, " = ", rhs, "$$")
	}, character(1))

	paste(
		c("### Model Equations (LaTeX)", lines),
		collapse = "\n"
	)
}
