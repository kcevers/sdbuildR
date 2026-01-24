#' Extract Insight Maker model from URL
#'
#' Create XML string from Insight Maker URL. For internal use; use `insightmaker_to_sfm()` to import an Insight Maker model.
#'
#' @param URL String with URL to an Insight Maker model
#' @param file If specified, file path to save Insight Maker model to. If NULL, do not save model.
#'
#' @returns XML string with Insight Maker model
#' @seealso [insightmaker_to_sfm()]
#' @export
#' @family insightmaker
#' @examplesIf has_internet()
#' URL <- "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet"
#' xml <- url_to_IM(URL)
#'
#' # Save model to file
#' file <- tempfile(fileext = ".InsightMaker")
#' xml <- url_to_IM(URL, file = file)
#' file.remove(file)
url_to_IM <- function(URL, file = NULL) {
  # Read URL
  url_data <- xml2::read_html(URL)

  # Get link from page
  iframe_src <- url_data |>
    xml2::xml_find_first(".//iframe") |>
    xml2::xml_attr("src")

  # Create absolute link out of relative link
  full_iframe_url <- xml2::url_absolute(iframe_src, URL)
  iframe_page <- xml2::read_html(full_iframe_url)

  # Get elements with <script tag
  script_texts <- xml2::xml_find_all(iframe_page, ".//script")

  # Keep script with certain keywords
  # script_model <- as.character(
  #   script_texts[stringr::str_detect(
  #     xml2::xml_text(script_texts, trim = TRUE), "model_id"
  #   ) & stringr::str_detect(xml2::xml_text(script_texts, trim = TRUE), "model_title")]
  # )
  script_texts_xml <- xml2::xml_text(script_texts, trim = TRUE)
  script_model <- as.character(
    script_texts[grepl("model_id", script_texts_xml) &
      grepl("model_title", script_texts_xml)]
  )

  # Extract part of interest
  # xml_str <- stringr::str_match_all(
  #   script_model,
  #   stringr::regex("<mxGraphModel>(.*?)</mxGraphModel>", dotall = TRUE)
  # )[[1]][1] |>
  #   stringr::str_replace_all("mxGraphModel", "insightmakermodel") |>
  #   # Remove escape characters for writing an XML file
  #   stringr::str_replace_all(stringr::fixed("\\\\\""), "\\\"") |>
  #   stringr::str_replace_all(stringr::fixed("\\\""), "\"") |>
  #   stringr::str_replace_all(stringr::fixed("\\\\n"), "\\n")
  # xml_str
  xml_str <- regmatches(
    script_model,
    gregexpr("<mxGraphModel>(.*?)</mxGraphModel>", script_model, perl = TRUE)
  )
  if (length(xml_str) == 0) {
    cli::cli_abort(c(
      "Failed to extract model from URL.",
      "x" = "Could not extract {.pkg InsightMaker} model from the provided URL.",
      "i" = "Ensure the model is public and the URL is correct.",
      ">" = "Check that the URL is accessible and points to a valid model."
    ))
  }

  xml_str <- xml_str[[1]][1]
  xml_str <- gsub("mxGraphModel", "insightmakermodel", xml_str, fixed = TRUE)
  xml_str <- gsub("\\\\\"", "\\\"", xml_str, fixed = TRUE)
  xml_str <- gsub("\\\"", "\"", xml_str, fixed = TRUE)
  xml_str <- gsub("\\\\n", "\\n", xml_str, fixed = TRUE)

  # Extract meta-data - this is embedded in the webpage, but not saved in the .InsightMaker file. Add to .InsightMaker file to preserve the original author of the model.
  header_names <- c("model_id", "model_title", "model_author_id", "model_author_name")
  header_info <- vapply(header_names, function(x) {
    stringr::str_match(
      script_model,
      sprintf("\"%s\":\"(.*?)\"", x)
    )[, 2]
  }, character(1)) |> as.list()
  header_str <- sprintf("<header> %s </header>", paste0(names(header_info),
    "=\"",
    unname(textutils::HTMLencode(header_info, encode.only = c("&", "<", ">"))),
    "\"",
    collapse = ", "
  ))

  # Insert header in xml_str
  idx_root <- stringr::str_locate(xml_str, "<root>")
  stringr::str_sub(xml_str, idx_root[, "start"], idx_root[, "end"]) <- paste0("<root> \\n", header_str)

  # Save and read .InsightMaker file to ensure it is the correct format
  if (is.null(file)) {
    delete_after <- TRUE
    file <- tempfile(fileext = ".InsightMaker")
  } else {
    delete_after <- FALSE
  }
  writeLines(xml_str, file)
  read_file <- xml2::read_xml(file)

  # If no file path was specified before, delete file
  if (delete_after) {
    file.remove(file)
    file <- NULL
  }

  return(read_file)
}


#' Read .InsightMaker or .json file
#'
#' @param file File path
#' @param fileext Allowed file extensions
#'
#' @returns Parsed file
#' @noRd
read_IM_file <- function(file, fileext) {
  ext <- tools::file_ext(file)

  if (!ext %in% fileext) {
    expected_exts <- paste0(paste0(".", fileext), collapse = " or ")
    download_instructions <- paste0(
      ifelse("InsightMaker" %in% fileext, "'Download Insight Maker File'", ""),
      ifelse(length(fileext) > 1, " or ", ""),
      ifelse("json" %in% fileext, "'ModelJSON File'", "")
    )
    cli::cli_abort(c(
      "Invalid file extension.",
      "x" = "The {.arg file} does not have the required extension {.code {expected_exts}}.",
      "i" = "Download your {.pkg InsightMaker} model from the share button (top right).",
      ">" = "Go to 'Import/Export', click the down arrow, and select {download_instructions}."
    ))
  }

  if (!file.exists(file)) {
    cli::cli_abort(c(
      "File not found.",
      "x" = "The specified {.arg file} does not exist: {.file {file}}.",
      ">" = "Check the file path and ensure the file exists."
    ))
  }

  # Read file
  read_file <- tryCatch(
    {
      if (ext == "InsightMaker") {
        xml2::read_xml(file)
      } else if (ext == "json") {
        jsonlite::fromJSON(file, simplifyVector = TRUE, flatten = TRUE)
      }
    },
    error = function(e) {
      cli::cli_abort(c(
        "Failed to parse file.",
        "x" = "Could not parse the file: {.file {file}}.",
        "i" = "Original error: {conditionMessage(e)}.",
        ">" = "Ensure the file is a valid {.pkg InsightMaker} or {.code .json} file."
      ))
    }
  )

  return(read_file)
}


get_IM_model <- function(URL, file, fileext = c("InsightMaker", "json")) {
  # Validate inputs
  URL_spec <- !missing(URL) && !is.null(URL) && !is.na(URL)
  file_spec <- !missing(file) && !is.null(file) && !is.na(file)
  if (!URL_spec && !file_spec) {
    cli::cli_abort(c(
      "Missing required arguments.",
      "x" = "Either {.arg URL} or {.arg file} must be specified.",
      ">" = "Provide one of these arguments to import a model."
    ))
  }

  if (URL_spec && file_spec) {
    cli::cli_abort(c(
      "Too many arguments specified.",
      "x" = "Both {.arg URL} and {.arg file} were specified.",
      "i" = "Only one input method can be used at a time.",
      ">" = "Specify either {.arg URL} or {.arg file}, not both."
    ))
  }

  # Load XML file
  if (URL_spec) {
    file <- NULL

    is_valid_URL <- stringr::str_detect(
      URL,
      stringr::regex("http[s]?\\:\\/\\/[www\\.]?insightmaker")
    )

    if (!is_valid_URL) {
      cli::cli_abort(c(
        "Invalid {.pkg InsightMaker} URL.",
        "x" = "The {.arg URL} is not a valid {.pkg InsightMaker} model URL.",
        "i" = "URLs must start with {.code http://insightmaker} or {.code https://insightmaker}.",
        ">" = "Provide a valid {.pkg InsightMaker} URL."
      ))
    }

    ext <- "InsightMaker"
    tryCatch(
      {
        read_file <- url_to_IM(URL, file)
      },
      error = function(e) {
        cli::cli_abort(c(
          "Failed to download model.",
          "x" = "Could not download {.pkg InsightMaker} model from the URL.",
          "i" = "Original error: {conditionMessage(e)}.",
          ">" = "Check your internet connection and ensure the URL is accessible."
        ))
      }
    )
  } else {
    # Read file
    ext <- tools::file_ext(file)
    read_file <- read_IM_file(file, fileext = fileext)
  }

  return(list(read_file = read_file, ext = ext))
}


validate_json_path <- function(path, add_fileext = TRUE) {
  # Check it's a character string
  if (!is.character(path) || length(path) != 1) {
    cli::cli_abort(c(
      "Invalid {.arg path} argument.",
      "x" = "The {.arg path} argument must be a single {.cls character} string.",
      ">" = "Provide the path as a single string."
    ))
  }

  # Check extension
  ext <- tools::file_ext(path)
  if (ext == "" && add_fileext) {
    path <- paste0(path, ".json")
  } else if (ext != "json") {
    cli::cli_abort(c(
      "Invalid file extension.",
      "x" = "The {.arg path} must have a {.code .json} extension.",
      ">" = "Specify a path ending with {.code .json}."
    ))
  }

  # Check directory exists (if creating file)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    cli::cli_abort(c(
      "Directory not found.",
      "x" = "The directory does not exist: {.file {dir_path}}.",
      ">" = "Create the directory or specify a valid path."
    ))
  }

  # # Optional: check if file already exists
  # if (file.exists(path)) {
  #   warning("File already exists and will be overwritten: ", path)
  # }

  return(path)
}


#' Convert .InsightMaker file to .json file
#'
#' Import and convert a stock-and-flow model from [Insight Maker](https://insightmaker.com/) to a .json file. Models may be your own or another user's. Importing causal loop diagrams or agent-based models is not supported.
#'
#' Insight Maker models can be imported using a URL or Insight Maker file. Ensure the URL refers to a public (not private) model. To download a model file from Insight Maker, first clone the model if it is not your own. Then, go to "Share" (top right), "Export", and "Download Insight Maker file".
#'
#' @param URL URL to Insight Maker model. Character.
#' @param file File path to Insight Maker model. Only used if URL is not specified. Needs to be a character with suffix .InsightMaker.
#' @param destfile Output file path. Must have extension .json or no extension. Overwrites file if it already exists. If not provided, return model in json format.
#'
#' @returns If destfile is not provided; object of class "json". If destfile provided, invisibly returns destfile (character string).
#' @export
#' @family insightmaker
#'
#' @examplesIf has_internet() && Sys.getenv("NOT_CRAN") == "true"
#' # Convert a model from Insight Maker to json
#' destfile <- tempfile(fileext = ".json")
#' json <- insightmaker_to_json(
#'   URL =
#'     "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet",
#'   destfile = destfile
#' )
#' file.remove(destfile)
#'
insightmaker_to_json <- function(URL, file, destfile = NULL) {
  if (!is.null(destfile)) {
    destfile <- validate_json_path(destfile)
    save_file <- TRUE
  } else {
    save_file <- FALSE
  }

  # Read .InsightMaker file
  out <- get_IM_model(URL, file, fileext = "InsightMaker")
  read_file <- out[["read_file"]]

  # Prepare .InsightMaker file into more common intermediate format
  out <- prep_IM(read_file)

  # Unpack
  settings <- out[["settings"]]
  header <- out[["header"]]
  model_elements <- out[["model_elements"]]
  macros <- out[["macros"]]
  units <- out[["units"]]


  # Find old and new names for replacement
  # old_names <- unname(get_map(model_elements, "name_insightmaker"))
  new_names <- unname(get_map(model_elements, "name"))

  json_elements <- lapply(model_elements, function(x) {
    eqn_name <- switch(x[["type"]],
      "stock" = "initial_value",
      "flow" = "value",
      "variable" = "value",
      "none"
    )

    if (eqn_name != "none") {
      eqn <- x[["eqn_insightmaker"]]

      x[["behavior"]] <- list()
      x[["behavior"]][[eqn_name]] <- eqn
      x[["behavior"]][["description"]] <- x[["doc"]]

      if (is_defined(x[["units"]])) {
        x[["behavior"]][["units"]] <- x[["units"]]
      }

      if (x[["type"]] %in% c("stock", "flow") && is_defined(x[["non_negative"]])) {
        x[["behavior"]][["non_negative"]] <- x[["non_negative"]]
      }
    }

    if (x[["type"]] == "flow") {
      # from and to should be NA (i.e. null in JSON) if empty
      if (!is_defined(x[["from"]])) {
        x[["from"]] <- NA
      }
      if (!is_defined(x[["to"]])) {
        x[["to"]] <- NA
      }
    }

    if (x[["type"]] == "converter") {
      if (is_defined(x[["source"]])) {
        if (x[["source"]] == P[["time_name"]]) {
          x[["behavior"]][["input"]] <- "TIME"
        } else {
          x[["behavior"]][["input"]] <- "ELEMENT"
          x[["behavior"]][["input_element"]] <- x[["source"]]
        }
      }

      x[["behavior"]][["interpolation"]] <- toupper(x[["interpolation"]])

      if (is_defined(x[["xpts"]]) && is_defined(x[["ypts"]])) {
        df <- data.frame(x = x[["xpts"]], y = x[["ypts"]])
        x[["behavior"]][["data"]] <- asplit(as.matrix(df), 1) # Split by row
      }

      if (is_defined(x[["units"]])) {
        x[["behavior"]][["units"]] <- x[["units"]]
      }
    }

    x[["type"]] <- toupper(x[["type"]])

    x <- x[names(x) %in% c("type", "name", "behavior", "from", "to")]

    return(x)
  })
  names(json_elements) <- NULL

  # Find dependencies
  dep <- lapply(json_elements, function(x) {
    dependencies <- c()

    eqn_name <- switch(tolower(x[["type"]]),
      "stock" = "initial_value",
      "flow" = "value",
      "variable" = "value",
      "none"
    )

    if (eqn_name != "none") {
      eqn <- x[["behavior"]][[eqn_name]]

      idx_df <- get_range_names(eqn,
        var_names = new_names,
        names_with_brackets = TRUE
      )
      dependencies <- c(dependencies, idx_df[["name"]])
    }

    if (is_defined(x[["from"]])) {
      dependencies <- c(dependencies, x[["from"]])
    }
    if (is_defined(x[["to"]])) {
      dependencies <- c(dependencies, x[["to"]])
    }
    if (is_defined(x[["input_element"]])) {
      dependencies <- c(dependencies, x[["input_element"]])
    }

    return(dependencies)
  })
  names(dep) <- new_names
  dep <- compact_(dep)

  # Create links list from dependencies
  if (length(dep) > 0) {
    l <- lapply(dep, length)
    to_vec <- rep(names(dep), l)
    from_vec <- unlist(unname(dep))
    n <- length(to_vec)
    links <- lapply(seq_len(n), function(i) {
      list(
        type = "LINK",
        from = from_vec[i],
        to = to_vec[i]
      )
    })
  } else {
    links <- list()
  }

  json_elements <- append(json_elements, links)

  # Compile json file
  json <- list()
  json[["engine"]] <- "SIMULATION_PACKAGE"
  json[["name"]] <- header[["name"]]
  json[["description"]] <- header[["caption"]]

  if (!is.null(settings)) {
    # Prepare settings with canonical names for ModelJSON
    replacements <- c(
      "algorithm" = "method", "time_start" = "start",
      "time_units" = "time_units",
      "version" = "version",
      "time_length" = "length", "time_step" = "dt"
    )
    settings <- settings[names(settings) %in% unname(replacements)]

    if (length(settings) > 0) {
      replacements <- stats::setNames(names(replacements), unname(replacements))
      names(settings) <- ifelse(names(settings) %in% names(replacements),
        replacements[names(settings)], names(settings)
      )

      if (is_defined(settings[["algorithm"]])) {
        settings[["algorithm"]] <- toupper(settings[["algorithm"]])
      }
      if (is_defined(settings[["time_units"]])) {
        settings[["time_units"]] <- toupper(settings[["time_units"]])
      }

      json[["simulation"]] <- as.list(settings)
    }
  }
  json[["elements"]] <- json_elements

  json[["engine_settings"]] <- list()

  # Macros
  if (is_defined(macros)) {
    json[["engine_settings"]][["globals"]] <- macros
  }

  # Custom units
  if (is_defined(units)) {
    units_list <- unname(split(units, seq_len(nrow(units))))

    json[["engine_settings"]][["units"]] <- units_list
  }

  if (save_file) {
    jsonlite::write_json(json, destfile,
      pretty = TRUE,
      auto_unbox = TRUE # Necessary to keep booleans JSON valid
    )
    return(invisible(destfile))
  } else {
    json_formatted <- jsonlite::toJSON(json,
      pretty = TRUE,
      auto_unbox = TRUE # Necessary to keep booleans JSON valid)
    )
    return(json_formatted)
  }
}


#' Convert XML nodes to list
#'
#' @param read_file XML structured model
#'
#' @returns List
#' @noRd
#'
prep_IM <- function(read_file) {
  type <- "InsightMaker"

  # Get the children nodes
  children <- xml2::xml_children(read_file)
  if (xml2::xml_name(children) == "root") {
    children <- xml2::xml_children(children) # Double to remove nested layer
  }

  # Get attributes, also of children
  tags <- c("Setting", "Variable", "Converter", "Stock", "Flow", "Link", "Ghost")
  node_types <- xml2::xml_name(children)

  header_str <- xml2::xml_text(children[match("header",
    node_types,
    nomatch = 0
  )[[1]]], trim = TRUE)

  # Get attributes as well as deeper attributes
  children_attrs <- lapply(children, function(x) {
    c(unlist(xml2::xml_attrs(x)), unlist(xml2::xml_attrs(xml2::xml_children(x))))
  })

  # Check whether the model has any components
  is_sfm_IM(node_types, get_map(lapply(children_attrs, as.list), "name"))

  # Only keep nodes that are in tags
  children_attrs <- lapply(children_attrs[node_types %in% tags], as.list)
  node_types <- node_types[node_types %in% tags]

  # Turn nodes and entry names to lowercase
  node_types <- tolower(node_types)
  children_attrs <- lapply(children_attrs, function(x) {
    names(x) <- tolower(names(x))
    return(x)
  })


  # Get first setting (multiple exist sometimes)
  settings <- children_attrs[[match("setting", node_types)[1]]]
  macros <- settings[["macros"]]
  units_str <- settings[["units"]]

  # Prepare settings with canonical names
  settings <- prep_settings_IM(settings, type = type)
  header <- prep_header_IM(header_str, settings, type = type)

  # In an .InsightMaker file, custom units are saved as a string in order name, factor, base
  # Create data.frame with custom units, splitting by <> to separate name, factor, and base
  units <- prep_units_IM(units_str, type = type)

  # Find source-target dictionary for changing names
  source_target_dict <- get_source_target_IM(children_attrs, node_types, type = type)

  # Prepare model elements
  model_elements <- prep_model_elements_IM(children_attrs, node_types, type = type)

  # Change links to access_ids; change names
  model_elements <- change_names_IM(model_elements, source_target_dict, type = type)


  return(
    list(
      settings = settings,
      header = header,
      model_elements = model_elements,
      macros = macros,
      units = units
    )
  )
}


prep_header_IM <- function(header_str, settings, name, caption,
                           type = c("InsightMaker", "json")) {
  if (type == "InsightMaker") {
    if (length(header_str) > 0) {
      # Step 1: Split by comma and trim spaces
      pairs <- strsplit(header_str, ",\\s*")[[1]]

      # Step 2: Split each pair into name and value, then clean up extra quotes
      header <- vapply(pairs, function(pair) {
        key_value <- strsplit(pair, "=\\s*")[[1]]
        return(gsub('\"', "", key_value[2])) # Remove extra quotes
      }, character(1))

      # Convert to named list
      names(header) <- vapply(pairs, function(pair) {
        strsplit(pair, "=\\s*")[[1]][1]
      }, character(1))
      header <- as.list(header)

      # Rename elements in header
      new_names <- names(header)
      new_names[new_names == "model_author_name"] <- "author"
      new_names[new_names == "model_author_id"] <- "insightmaker_author_id"
      new_names[new_names == "model_title"] <- "name"
      new_names[new_names == "model_id"] <- "insightmaker_model_id"
      names(header) <- new_names
    } else {
      header <- list()
    }


    # Add version to header
    header[["insightmaker_version"]] <- settings[["version"]]
    header[["insightmaker_method"]] <- settings[["method"]]
  } else if (type == "json") {
    header <- list()
    header[["name"]] <- name
    header[["caption"]] <- caption
  }

  header <- compact_(header)

  return(header)
}

prep_units_IM <- function(units, type = c("InsightMaker", "json")) {
  if (type == "InsightMaker") {
    if (nzchar(units)) {
      units <- strsplit(
        # In Insight Maker, units are not case-sensitive
        tolower(units), "\n"
      )[[1]] |>
        lapply(function(x) {
          stringr::str_split_fixed(x, "<>", n = 3)
        }) |>
        do.call(rbind, args = _) |>
        set_colnames(c("name", "factor", "base")) |>
        as.data.frame()
    } else {
      units <- NULL
    }
  } else if (type == "json") {
    if (is_defined(units)) {
      units <- bind_rows_(units)
    } else {
      units <- NULL
    }
  }

  return(units)
}


prep_settings_IM <- function(settings, type = c("InsightMaker", "json")) {
  if (!is.null(settings)) {
    if (type == "InsightMaker") {
      replacements <- c(
        "solutionalgorithm" = "method", "timestart" = "start",
        "timeunits" = "time_units",
        "timelength" = "length", "timestep" = "dt"
      )
    } else if (type == "json") {
      replacements <- c(
        "algorithm" = "method", "time_start" = "start",
        "time_length" = "length", "time_step" = "dt"
      )
    }
    names(settings) <- ifelse(names(settings) %in% names(replacements),
      replacements[names(settings)], names(settings)
    )
  }

  if (type == "InsightMaker") {
    # Check whether the model uses an early version of Insight Maker
    if (as.numeric(settings[["version"]]) < 37) {
      cli::cli_warn(c(
        "Old {.pkg InsightMaker} version detected.",
        "i" = "This model uses version {.val {settings[[\"version\"]]}} where links were bi-directional by default.",
        "!" = "This may cause issues in translating the model.",
        ">" = "Clone the model in {.pkg InsightMaker} and provide the URL to the updated version."
      ))
    }


    # Check whether model is later version of Insight Maker than the package was made for
    if (as.numeric(settings[["version"]]) > P[["insightmaker_version"]]) {
      cli::cli_warn(c(
        "Newer {.pkg InsightMaker} version detected.",
        "i" = "This model uses version {.val {settings[[\"version\"]]}}, but {.pkg sdbuildR} was based on version {.val {P[[\"insightmaker_version\"]]}}.",
        "!" = "Some features may not be available or may behave differently."
      ))
    }
  }

  return(settings)
}


prep_model_elements_IM <- function(children_attrs, node_types, type = c("InsightMaker", "json")) {
  keep_idx <- node_types %in% c("variable", "converter", "stock", "flow")
  model_elements <- children_attrs[keep_idx]
  model_element_types <- node_types[keep_idx]

  if (type == "InsightMaker") {
    eqn_name_f <- function(x) {
      switch(x,
        "variable" = "equation",
        "stock" = "initialvalue",
        "flow" = "flowrate",
        "none"
      )
    }
    doc_name <- "note"
  } else if (type == "json") {
    eqn_name_f <- function(x) {
      switch(x,
        "variable" = "value",
        "stock" = "initial_value",
        "flow" = "value",
        "none"
      )
    }
    doc_name <- "description"
  }

  model_elements <- lapply(seq_along(model_elements), function(y) {
    x <- model_elements[[y]]

    x[["type"]] <- model_element_types[y]

    eqn_name <- eqn_name_f(x[["type"]])


    if (eqn_name != "none") {
      x[["eqn_insightmaker"]] <- gsub("\\n", "\n", trimws(x[[eqn_name]]), fixed = TRUE)

      # Default is zero
      if (!is_defined(x[["eqn_insightmaker"]])) {
        x[["eqn_insightmaker"]] <- "0.0"
      }

      # Remove the element # don't to make IM_to_json() work
      # x <- x[names(x) != eqn_name]
      # x[eqn_name] <- NULL
    } else if (x[["type"]] %in% c("variable", "stock", "flow")) {
      # The default value of a stock/flow/variable is 0 - add in case left unspecified
      x[["eqn_insightmaker"]] <- "0.0"
    }

    x[["units"]] <- trimws(x[["units"]])
    if (!is_defined(x[["units"]])) {
      x[["units"]] <- "1"
    }

    # Rename note to doc
    if (doc_name %in% names(x)) {
      x[["doc"]] <- clean_doc(x[[doc_name]])
      x[[doc_name]] <- NULL
    } else {
      x[["doc"]] <- ""
    }

    # Rename constraints
    if (type == "InsightMaker") {
      # Constraints are no longer supported
      # if ("minconstraint" %in% names(x)) {
      #   x[["min"]] <- ifelse(x[["minconstraintused"]] == "true", x[["minconstraint"]], "")
      #   x[["max"]] <- ifelse(x[["maxConstraintused"]] == "true", x[["maxconstraint"]], "")
      #   x[c("minconstraint", "minconstraintused", "maxconstraint", "maxconstraintused")] <- NULL
      # }

      if (x[["type"]] == "flow") {
        if (!is_defined(x[["source"]])) {
          x[["from"]] <- ""
        } else {
          x[["from"]] <- x[["source"]]
          x[["source"]] <- NULL
        }
        if (!is_defined(x[["target"]])) {
          x[["to"]] <- ""
        } else {
          x[["to"]] <- x[["target"]]
          x[["target"]] <- NULL
        }
      }

      if (is_defined(x[["onlypositive"]])) {
        # Insight Maker both uses "true" and "false", as well as "-1" -> FALSE
        x[["non_negative"]] <- as.logical(x[["onlypositive"]])
        if (is.na(x[["non_negative"]])) {
          x[["non_negative"]] <- FALSE
        }
        x[["onlypositive"]] <- NULL
      }

      # Get x and y data for interpolation function
      if (is_defined(x[["data"]])) {
        data_split <- strsplit(x[["data"]], ";")[[1]] |>
          strsplit(",") |>
          do.call(rbind, args = _)
        x[["xpts"]] <- as.numeric(trimws(data_split[, 1]))
        x[["ypts"]] <- as.numeric(trimws(data_split[, 2]))
        x[["data"]] <- NULL
      }

      if (x[["type"]] == "converter" && is_defined(x[["source"]])) {
        if (tolower(x[["source"]]) == "time") {
          x[["source"]] <- P[["time_name"]]
        }
      }
    } else if (type == "json") {
      if (!is_defined(x[["non_negative"]])) {
        x[["non_negative"]] <- FALSE
      } else {
        x[["non_negative"]] <- as.logical(x[["non_negative"]])
        if (is.na(x[["non_negative"]])) {
          x[["non_negative"]] <- FALSE
        }
      }

      if (x[["type"]] == "flow") {
        if (!is_defined(x[["from"]])) {
          x[["from"]] <- ""
        }
        if (!is_defined(x[["to"]])) {
          x[["to"]] <- ""
        }
      }

      # Graphical source
      if (is_defined(x[["input"]])) {
        # x[["input"]] contains NA, "TIME", or "ELEMENT"; in case of the latter,
        # x[["input_element"]] then specifies which element

        if (tolower(x[["input"]]) == "element") {
          x[["input"]] <- x[["input_element"]]
        } else if (tolower(x[["input"]]) == "time") {
          x[["input"]] <- P[["time_name"]]
        }
        x[["source"]] <- x[["input"]]
      }


      # Get x and y data for interpolation function
      if (is_defined(x[["data"]])) {
        x[["xpts"]] <- as.numeric(x[["data"]][, 1])
        x[["ypts"]] <- as.numeric(x[["data"]][, 2])
        x[["data"]] <- NULL
      }
    }


    x[["units_insightmaker"]] <- x[["units"]]
    x[["id_insightmaker"]] <- x[["id"]]

    return(x)
  })

  return(model_elements)
}


#' Convert json structure to list
#'
#' @param read_file json structured model
#'
#' @returns List
#' @noRd
#'
prep_json <- function(read_file) {
  # Read JSON file; get components
  model_df <- read_file[["elements"]]
  settings <- read_file[["simulation"]]
  header <- prep_header_IM(
    name = read_file[["name"]],
    caption = read_file[["description"]], type = "json"
  )
  macros <- read_file[["engine_settings"]][["globals"]]
  units_str <- read_file[["engine_settings"]][["units"]]

  if (!is_defined(macros)) {
    macros <- ""
  }

  units <- prep_units_IM(units_str, type = "json")


  settings <- prep_settings_IM(settings, type = "json")

  # Check if file is stock-and-flow model (and not e.g., agent-based)
  is_sfm_IM(model_df[["type"]], model_df[["name"]])

  # Only keep specific types and relevant columns
  model_df[["type"]] <- tolower(model_df[["type"]])

  model_df <- model_df[
    model_df[["type"]] %in% c("stock", "flow", "variable", "converter", "link"),
    !grepl("^display", colnames(model_df))
  ]
  colnames(model_df) <- gsub("^behavior.", "", colnames(model_df))

  # Convert each row to a named list element
  model_elements <- apply(model_df, 1, as.list, simplify = FALSE)

  # Assign id
  ids <- as.character(seq_along(model_elements))
  model_elements <- Map(function(x, id) {
    x[["id"]] <- id
    return(x)
  }, model_elements, ids)

  # Find source and target dictionary for changing names
  source_target_dict <- get_source_target_IM(
    children_attrs = model_elements,
    node_types = model_df[["type"]],
    type = "json"
  )

  model_elements <- prep_model_elements_IM(
    children_attrs = model_elements,
    node_types = model_df[["type"]],
    type = "json"
  )


  # Change links to access_ids; change names
  model_elements <- change_names_IM(model_elements, source_target_dict,
    type = "json"
  )


  return(
    list(
      settings = settings,
      header = header,
      model_elements = model_elements,
      macros = macros,
      units = units
    )
  )
}


#' Convert .InsightMaker or .json file to stock-and-flow model
#'
#' @param read_file XML or json structured model
#' @param ext File extension
#'
#' @returns Stock-and-flow model of class sdbuildR_xmile with some extras (e.g., units, macros)
#' @noRd
#'
file_to_xmile <- function(read_file, ext) {
  # Prepare .InsightMaker or .json file into more common intermediate format
  if (ext == "InsightMaker") {
    out <- prep_IM(read_file)
  } else if (ext == "json") {
    out <- prep_json(read_file)
  }

  # Unpack
  settings <- out[["settings"]]
  header <- out[["header"]]
  model_elements <- out[["model_elements"]]
  macros <- out[["macros"]]
  units <- out[["units"]]

  # Remove comments from equations
  model_element_types <- names(model_elements)
  model_elements <- lapply(model_elements, function(x) {
    # Graphical functions won't have an equation
    if (is_defined(x[["eqn_insightmaker"]])) {
      out <- prep_eqn_IM(x[["eqn_insightmaker"]])
      x[["eqn_insightmaker"]] <- out[["eqn"]]
      x[["doc"]] <- paste0(x[["doc"]], out[["doc"]])
    }

    # if (is_defined(x[["units"]])){
    #   x[["units_insightmaker"]] <- x[["units"]]
    # }

    return(x)
  })

  # Converters
  idx <- model_element_types == "converter"
  model_elements[idx] <-
    lapply(model_elements[idx], function(x) {
      x[["type"]] <- "gf"

      x[["interpolation"]] <- tolower(x[["interpolation"]])
      if (x[["interpolation"]] == "none") {
        x[["interpolation"]] <- "constant"
      }
      x[["extrapolation"]] <- "nearest" # Default

      # Remove equation
      x[["eqn_insightmaker"]] <- NULL

      return(x)
    })


  # Variables -> Auxiliaries
  idx <- model_element_types == "variable"
  model_elements[idx] <- lapply(model_elements[idx], function(x) {
    x[["type"]] <- "aux"
    return(x)
  })


  # Stocks
  idx <- "stock" == model_element_types
  model_elements[idx] <-
    lapply(model_elements[idx], function(x) {
      # x[["type"]] <- "stock"

      if (is_defined(x[["stockmode"]])) {
        if (tolower(x[["stockmode"]]) == "conveyor") {
          x[["conveyor"]] <- TRUE
          x[["len"]] <- x[["delay"]] # ** check name delay in .json
        }
      }

      return(x)
    })

  # # Flows
  # idx <- "flow" == model_element_types
  # model_elements[idx] <- lapply(model_elements[idx], function(x) {
  #   # x[["type"]] <- "flow"
  #
  #   return(x)
  # })

  # Only keep selected properties
  keep_prop <- get_building_block_prop()

  all_prop <- c(
    unique(unlist(keep_prop)),
    "eqn_insightmaker", "units_insightmaker", "name_insightmaker", "id_insightmaker",
    # "type", "name", "non_negative","doc",
    # "from", "to", # flow
    # "xpts", "ypts", "interpolation", "source", "extrapolation", # gf
    "conveyor", "len" # keep for prep_conveyor_IM()
    # stock
    # "access", "access_ids",
  )

  model_elements <- lapply(model_elements, function(x) {
    return(x[names(x) %in% all_prop])
  })


  # Name elements
  model_elements <- stats::setNames(
    model_elements,
    unname(get_map(model_elements, "name"))
  )


  # Set-up basic structure
  sfm <- new_sdbuildR_xmile()
  if (!is.null(settings)) {
    sfm <- sim_specs_IM(sfm,
      method = settings[["method"]],
      time_units = settings[["time_units"]],
      start = settings[["start"]],
      length = settings[["length"]],
      dt = settings[["dt"]]
    )
  }


  if (!is.null(header)) {
    sfm[["header"]] <- utils::modifyList(sfm[["header"]], header)
  }

  # Variables - convert from nested lists to data frame rows
  # Combine all model elements into a single list
  all_elements <- c(
    model_elements["stock" == model_element_types],
    model_elements["variable" == model_element_types],
    model_elements["flow" == model_element_types],
    model_elements["converter" == model_element_types]
  )
  
  # Create data frame from model elements
  if (length(all_elements) > 0) {
    for (elem in all_elements) {
      # Add each element as a row using build()
      sfm <- do.call(build, c(list(sfm = sfm), elem))
    }
  }

  # Prepare globals/macros, conveyors, converters
  sfm <- prep_globals_IM(sfm, macros, units)
  sfm <- prep_conveyors_IM(sfm)
  sfm <- prep_converters_IM(sfm)


  if (P[["debug"]]) {
    n_stocks <- sum(sfm[["variables"]][["type"]] == "stock")
    n_flows <- sum(sfm[["variables"]][["type"]] == "flow")
    n_auxs <- sum(sfm[["variables"]][["type"]] == "aux")
    n_gfs <- sum(sfm[["variables"]][["type"]] == "gf")
    
    cli::cli_inform(c(
      "Model elements detected:",
      "i" = "Stocks: {.val {n_stocks}}",
      "i" = "Flows: {.val {n_flows}}",
      "i" = "Auxiliaries: {.val {n_auxs}}",
      "i" = "Graphical Functions: {.val {n_gfs}}"
    ))

    if (nzchar(sfm[["macros_temp"]][["eqn"]])) {
      cli::cli_inform(c(
        "i" = "User-defined macros and globals detected in model."
      ))
    } else {
      cli::cli_inform(c(
        "i" = "No user-defined macros or globals detected."
      ))
    }
  }

  # Already add eqn and units - update in data frame
  for (i in seq_len(nrow(sfm[["variables"]]))) {
    if (sfm[["variables"]][i, "type"] != "gf") {
      if ("eqn_insightmaker" %in% colnames(sfm[["variables"]])) {
        sfm[["variables"]][i, "eqn"] <- sfm[["variables"]][i, "eqn_insightmaker"]
      }
    }
    
    if ("units_insightmaker" %in% colnames(sfm[["variables"]])) {
      # Store original InsightMaker units for reference
      # The units column is already set from the initial creation
    }
  }

  sfm <- validate_xmile(sfm)

  return(sfm)
}


sim_specs_IM <- function(sfm, method, time_units, start, length, dt) {
  # Not every simulation specification may be specified in an Insight Maker model
  args <- compact_(as.list(environment()))
  # print(args)
  # print(names(args))

  # Ensure year and month match Insight Maker's unit definition - a year in Insight Maker is 365 days, not 365.25 days
  if ("time_units" %in% names(args)) {
    args[["time_units"]] <- stringr::str_replace_all(
      tolower(args[["time_units"]]),
      stringr::regex(c(
        "[Y|y]ear[s]?" = "common_yr",
        "[Q|q]uarter[s]?" = "common_quarter",
        "[M|m]onth[s]?" = "common_month"
      ), ignore_case = TRUE)
    )
  }

  if ("method" %in% names(args)) {
    args[["method"]] <- tolower(args[["method"]])
    args[["method"]] <- switch(args[["method"]],
      # "euler" = "euler",
      "rk1" = "euler",
      # "rk4" = "rk4",
      args[["method"]]
    )
  }

  if ("dt" %in% names(args)) {
    if ("method" %in% names(args) && as.numeric(args[["dt"]]) >= 1 && args[["method"]] == "rk4") {
      cli::cli_inform(c(
        "Adjusting timestep for solver.",
        "i" = "The timestep {.code dt = {args[[\"dt\"]]}} is not suitable for {.fn rk4} solver.",
        ">" = "Setting {.code dt = 0.1} for better accuracy."
      ))
      args[["dt"]] <- ".1"
    }

    args[["save_at"]] <- args[["dt"]]
  }

  if ("start" %in% names(args)) {
    args[["save_from"]] <- start <- args[["start"]]
  } else {
    start <- 0
  }

  if ("length" %in% names(args)) {
    args[["stop"]] <- as.numeric(start) + as.numeric(args[["length"]])
    args[["length"]] <- NULL
  }

  # sim_specs(sfm,
  #           method = method,
  #           time_units = time_units,
  #           start = start,
  #           stop = as.numeric(start) + as.numeric(length),
  #           dt = dt,
  #           save_at = dt,
  #           save_from = start
  # )

  sfm <- do.call(sim_specs, args)

  return(sfm)
}


#' Check whether Insight Maker model is a stock-and-flow model
#'
#' @param type Types of elements
#' @param name Names of elements
#'
#' @returns NULL; called for side effects
#' @noRd
is_sfm_IM <- function(type, name) {
  type <- tolower(type)
  if (!any(c("variable", "stock", "flow") %in% tolower(type))) {
    cli::cli_abort(c(
      "Model contains no stock-and-flow elements.",
      "x" = "The imported model contains no variables, stocks, or flows.",
      "i" = "{.pkg sdbuildR} only supports stock-and-flow models.",
      ">" = "Import a model with at least one variable, stock, or flow."
    ))
  }


  # Check for Agent-Based Model (ABM) elements
  idx <- which(type %in% c("state", "transition"))
  if (length(idx) > 0) {
    cli::cli_abort(c(
      "Unsupported model type detected.",
      "x" = "Agent-Based Modelling elements found: {paste0(name[idx], collapse = \", \")}.",
      "i" = "{.pkg sdbuildR} only supports stock-and-flow models.",
      ">" = "Remove agent-based elements or import a different model."
    ))
  }

  return(invisible())
}


new_names_IM <- function(old_names) {
  # Variables cannot be the same name as Insight Maker functions
  IM_func_names <- syntax_IM[["conv_df"]][["insightmaker"]]
  new_names <- clean_name(old_names,
    protected = c(
      IM_func_names, tolower(IM_func_names),
      toupper(IM_func_names)
    )
  )
  return(new_names)
}


get_source_target_IM <- function(children_attrs, node_types,
                                 type = c("InsightMaker", "json")) {
  # Construct dictionary for replacement of id

  if (type == "InsightMaker") {
    # Change links to access_ids
    keep_idx <- node_types %in% c("link", "flow")
    children_connectors <- children_attrs[keep_idx]
    connector_names <- node_types[keep_idx]

    # Bidirectional links and flows
    bidirectional <- get_map(children_connectors, "bidirectional")
    ids <- get_map(children_connectors, "id")
    sources <- get_map(children_connectors, "source")
    targets <- get_map(children_connectors, "target")

    # Add stocks as sources for flows as targets
    add_stock_sources <- c(sources[connector_names == "flow"], targets[connector_names == "flow"])
    add_stock_targets <- c(ids[connector_names == "flow"], ids[connector_names == "flow"])

    # Replace ghost ids with original
    if (any(node_types == "ghost")) {
      ghost_sources <- get_map(children_attrs[node_types == "ghost"], "source")
      ghost_ids <- get_map(children_attrs[node_types == "ghost"], "id")
      replace_dict <- stats::setNames(ghost_sources, ghost_ids)
      sources <- stringr::str_replace_all(sources, replace_dict)
      targets <- stringr::str_replace_all(targets, replace_dict)
    }

    # In case of a bidirectional link, switch around source and target and add
    add_bi_targets <- sources[bidirectional == "true"]
    add_bi_sources <- targets[bidirectional == "true"]

    # Add sources from converters
    keep_idx <- node_types %in% c("converter")
    converters <- children_attrs[keep_idx]
    converter_names <- get_map(converters, "name")
    converter_sources <- get_map(converters, "source")
    idx <- tolower(converter_sources) == "time"
    converter_sources[idx] <- ""

    targets <- c(targets, add_bi_targets, add_stock_targets, converter_names)
    sources <- c(sources, add_bi_sources, add_stock_sources, converter_sources)
  } else if (type == "json") {
    # In JSON, flows are unidirectional only
    keep_idx <- node_types %in% c("link", "flow")
    children_connectors <- children_attrs[keep_idx]
    connector_names <- node_types[keep_idx]

    sources <- get_map(children_connectors, "from")
    targets <- get_map(children_connectors, "to")
    names <- get_map(children_connectors, "name")

    # Add stocks as sources for flows as targets
    add_stock_sources <- c(sources[connector_names == "flow"], targets[connector_names == "flow"])
    add_stock_targets <- c(names[connector_names == "flow"], names[connector_names == "flow"])

    # Add sources from converters
    keep_idx <- node_types %in% c("converter")
    converters <- children_attrs[keep_idx]
    converter_names <- get_map(converters, "name")
    converter_sources <- get_map(converters, "input_element")

    targets <- c(targets, add_stock_targets, converter_names)
    sources <- c(sources, add_stock_sources, converter_sources)

    # Remove NAs
    idx <- is.na(sources) | is.na(targets)
    if (any(idx)) {
      sources <- sources[!idx]
      targets <- targets[!idx]
    }

    # Remove empty strings
    idx <- length(sources) == 0 | length(targets) == 0
    if (any(idx)) {
      sources <- sources[!idx]
      targets <- targets[!idx]
    }


    # Replace names with ids
    keep_idx <- node_types %in% c("converter", "stock", "flow", "variable")
    model_elements <- children_attrs[keep_idx]

    ids <- unname(get_map(model_elements, "id"))
    names <- unname(get_map(model_elements, "name"))
    sources <- ids[match(sources, names)]
    targets <- ids[match(targets, names)]
  }


  # Remove doubles
  temp <- data.frame(target = targets, source = sources)
  temp <- temp[!duplicated(temp), ]
  targets <- temp[["target"]]
  sources <- temp[["source"]]


  return(list(sources = sources, targets = targets))
}


change_names_IM <- function(model_elements, source_target_dict,
                            type = c("InsightMaker", "json")) {
  sources <- source_target_dict[["sources"]]
  targets <- source_target_dict[["targets"]]

  model_element_types <- get_map(model_elements, "type")
  ids <- get_map(model_elements, "id")
  old_names <- unname(get_map(model_elements, "name"))
  new_names <- new_names_IM(old_names)

  # Add to which ids it has access
  model_elements <- lapply(model_elements, function(x) {
    x[["access_ids"]] <- Filter(nzchar, compact_(sources[targets == x[["id"]]]))
    x[["access"]] <- new_names[match(x[["access_ids"]], ids)]
    return(x)
  })

  if (type == "InsightMaker") {
    use_id <- TRUE
  } else if (type == "json") {
    use_id <- FALSE
  }

  find_name <- function(old, access_ids, use_id) {
    if (use_id) {
      name <- new_names[match(old, ids)]
    } else {
      name <- new_names[old == old_names & ids %in% access_ids]
    }
    if (length(name) == 0 || is.na(name)) {
      name <- ""
    }
    return(name)
  }


  # Replace old names with new names
  model_elements <- lapply(model_elements, function(x) {
    # Which stocks are the flows connected to?
    if (x[["type"]] == "flow") {
      # x[["from"]] <- ifelse("from" %in% names(x), new_names[match(x[["from"]], ids)], "")
      # x[["to"]] <- ifelse("to" %in% names(x), new_names[match(x[["to"]], ids)], "")
      x[["from"]] <- find_name(x[["from"]], x[["access_ids"]], use_id)
      x[["to"]] <- find_name(x[["to"]], x[["access_ids"]], use_id)
    }


    if (x[["type"]] == "converter") {
      if (is_defined(x[["source"]])) {
        # x[["source"]] <- ifelse(tolower(x[["source"]]) == "time",
        #                         P[["time_name"]],
        #                         find_name(x[["source"]], x[["access_ids"]], use_id))

        if (tolower(x[["source"]]) != P[["time_name"]]) {
          x[["source"]] <- find_name(x[["source"]], x[["access_ids"]], use_id)
        }
      }
    }


    if (is_defined(x[["eqn_insightmaker"]])) {
      # Replace old names with new names
      original <- c(old_names[ids %in% x[["access_ids"]]])
      replacement <- c(new_names[ids %in% x[["access_ids"]]])

      if (x[["type"]] == "flow") {
        # Though no longer supported in Insight Maker version 39, in older versions:
        # Flows can use [Alpha] and [Omega] to refer to the stock they flow from and to, respectively. Change to new variable names:
        original <- c(original, "Alpha", "Omega")
        replacement <- c(replacement, x[["from"]], x[["to"]])
      }

      x[["eqn_insightmaker"]] <- replace_names_IM(x[["eqn_insightmaker"]],
        original = original,
        replacement = replacement,
        with_brackets = TRUE, ignore_case = TRUE
      )
    }

    x[["name_insightmaker"]] <- x[["name"]]
    x[["name"]] <- find_name(x[["id"]], ids, use_id = TRUE)

    return(x)
  })

  names(model_elements) <- model_element_types

  return(model_elements)
}


# change_names_json <- function(model_elements){
#
#   with_brackets = TRUE
#   ignore_case = TRUE # json is case insensitive, but does not allow duplicate names
#
#   old_names <- unname(get_map(model_elements, "name"))
#   new_names <- new_names_IM(old_names)
#
#   # Rename to eqn_insightmaker
#   model_element_types <- names(model_elements)
#
#   model_elements <- lapply(seq_along(model_elements), function(y) {
#
#     x <- model_elements[[y]]
#
#     eqn_name <- switch(model_element_types[y],
#                        "variable" = "value",
#                        "stock" = "initial_value",
#                        "flow" = "value",
#                        "none"
#                        )
#
#     if (eqn_name != "none") {
#       x[["eqn_insightmaker"]] <- x[[eqn_name]]
#
#       # Default is zero
#       if (!is_defined(x[["eqn_insightmaker"]])) {
#         x[["eqn_insightmaker"]] <- "0.0"
#       }
#
#       # Remove the element # don't to make IM_to_json() work
#       # x <- x[names(x) != eqn_name]
#       # x[eqn_name] <- NULL
#
#     } else if (model_element_types[y] %in% c("variable", "stock", "flow")) {
#       # The default value of a stock/flow/variable is 0 - add in case left unspecified
#       x[["eqn_insightmaker"]] <- "0.0"
#     }
#
#     # Rename description to doc
#     if ("description" %in% names(x)) {
#       x[["doc"]] <- clean_doc(x[["description"]])
#
#       x[["description"]] <- NULL
#     } else {
#       x[["description"]] <- ""
#     }
#
#     return(x)
#   })
#
#   temp <- function(name, surround_with_brackets = TRUE){
#
#     if (surround_with_brackets){
#       name <- paste0("[", name, "]")
#     }
#
#     new_name <- replace_names_IM(name,
#                      original = old_names,
#                      replacement = new_names,
#                      with_brackets = with_brackets,
#                      ignore_case = ignore_case
#     )
#
#     if (surround_with_brackets){
#       new_name <- gsub("^\\[", "", x = new_name) |> gsub("\\]$", "", x = _)
#     }
#
#     return(new_name)
#   }
#
#   # Replace old names with new names
#   model_elements <- lapply(model_elements, function(x) {
#
#     if (is_defined(x[["eqn_insightmaker"]])){
#
#       x[["eqn_insightmaker"]] <- gsub("\\n", "\n", x[["eqn_insightmaker"]], fixed = TRUE)
#       x[["eqn_insightmaker"]] <- temp(x[["eqn_insightmaker"]],
#                                                   surround_with_brackets = FALSE
#       )
#     }
#
#     x[["name_insightmaker"]] <- x[["name"]]
#     x[["name"]] <- temp(x[["name_insightmaker"]])
#
#     if (is_defined(x[["to"]])){
#       x[["to"]] <- temp(x[["to"]])
#     }
#
#     if (is_defined(x[["from"]])){
#       x[["from"]] <- temp(x[["from"]])
#     }
#
#     if (is_defined(x[["source"]])){
#       x[["source"]] <- temp(x[["source"]])
#     }
#
#     return(x)
#   })
#
#   names(model_elements) <- model_element_types
#
#   return(model_elements)
# }


clean_doc <- function(x) {
  # Some notes still have HTML? tags
  x <- gsub("<span[^>]*>", " ", x) # Remove opening <span> tags
  x <- gsub("</span>", " ", x) # Remove closing </span> tags
  x <- gsub("<a[^>]*>", " ", x) # Remove opening <a> tags
  x <- gsub("</a>", " ", x) # Remove closing </a> tags
  x <- gsub("<br>", "\n", x) # Remove closing line breaks
  x <- gsub("<div[^>]*>", " ", x) # Remove opening <div> tags
  x <- gsub("</div>", " ", x) # Remove closing </div> tags
  x <- gsub("&nbsp;", " ", x)
  return(x)
}


prep_globals_IM <- function(sfm, macros, units) {
  # Add globals to define at top of script
  out_global <- prep_eqn_IM(macros |>
    # Remove leading and last \" before replacing \" with "
    stringr::str_replace("^\\\"", "") |>
    stringr::str_replace("\\\"$", "") |> trimws())


  # Temporary entries in sfm
  sfm[["model_units_temp"]] <- units
  sfm[["macros_temp"]] <- list(
    eqn = out_global[["eqn"]],
    eqn_insightmaker = out_global[["eqn"]],
    doc = out_global[["doc"]]
  )

  return(sfm)
}


prep_conveyors_IM <- function(sfm) {
  # Conveyors Stocks have a fixed delay with a specified delay length. If a stock "A" is referred to as [A], it refers to the delayed value of A, whereas [[A]] refers to the accumulated value to the 'true' current value of A. If the stock is of StockMode Conveyor, any reference to [A] refers to the delayed value of A.
  # In short:
  # for a stock A which is a conveyor,
  # [A] refers to A_conveyor
  # [[A]] refers to A
  
  # Find conveyor stocks - check if conveyor column exists
  conveyor_stocks <- character(0)
  if ("conveyor" %in% colnames(sfm[["variables"]])) {
    conveyor_idx <- !is.na(sfm[["variables"]][["conveyor"]]) & 
                    sfm[["variables"]][["conveyor"]] != "" &
                    sfm[["variables"]][["type"]] == "stock"
    if (any(conveyor_idx)) {
      conveyor_stocks <- sfm[["variables"]][conveyor_idx, "name"]
    }
  }

  if (length(conveyor_stocks) > 0) {
    # Ensure correct referencing of conveyors
    dict <- paste0("[", conveyor_stocks, P[["conveyor_suffix"]], "]") |>
      stats::setNames(paste0("[[", conveyor_stocks, "]]"))

    # Update equations in data frame
    for (i in seq_len(nrow(sfm[["variables"]]))) {
      if ("eqn_insightmaker" %in% colnames(sfm[["variables"]])) {
        if (!is.na(sfm[["variables"]][i, "eqn_insightmaker"])) {
          sfm[["variables"]][i, "eqn_insightmaker"] <- stringr::str_replace_all(
            sfm[["variables"]][i, "eqn_insightmaker"],
            stringr::fixed(dict, ignore_case = TRUE)
          )

          # Remove any left-over double bracket notations - these should be []
          sfm[["variables"]][i, "eqn_insightmaker"] <- stringr::str_replace_all(
            sfm[["variables"]][i, "eqn_insightmaker"],
            stringr::fixed(c("[[" = "[", "]]" = "]"))
          )
        }
      }
    }
  }

  return(sfm)
}


prep_converters_IM <- function(sfm) {
  # Get graphical function names and sources
  gf_idx <- sfm[["variables"]][["type"]] == "gf"
  converters <- character(0)
  converters_sources <- character(0)
  
  if (any(gf_idx)) {
    converters <- sfm[["variables"]][gf_idx, "name"]
    converters_sources <- sfm[["variables"]][gf_idx, "source"]
  }

  if (length(converters) > 0) {
    # # Ensure correct referencing of converters
    # dict_t <- stats::setNames(
    #   paste0("(", P[["time_name"]], ")"),
    #   paste0("([", P[["time_name"]], "])")
    # )

    dict <- paste0(
      "[", converters, "]", "(",
      ifelse(converters_sources == P[["time_name"]], "", "["),
      converters_sources,
      ifelse(converters_sources == P[["time_name"]], "", "]"),
      ")"
    ) |>
      # # Some sources are time t, remove [t]
      # stringr::str_replace_all(stringr::fixed(dict_t)) |>
      stats::setNames(paste0("[", converters, "]"))

    # Some sources are other graphical functions, replace there as well
    dict_extra <- stats::setNames(paste0("(", unname(dict), ")"), paste0("(", names(dict), ")"))
    dict_extra <- stringr::fixed(dict_extra)

    dict <- stats::setNames(stringr::str_replace_all(unname(dict), dict_extra), names(dict))

    # Temporary placeholders to avoid overlapping replacements
    placeholders <- vapply(seq_along(dict), function(x) {
      paste0(sample(c(letters, LETTERS, 0:9), 12, replace = TRUE), collapse = "")
    }, character(1))

    dict_temp <- stats::setNames(placeholders, names(dict))
    dict_real <- stats::setNames(unname(dict), unname(dict_temp))

    dict_temp <- stringr::fixed(dict_temp)
    dict_real <- stringr::fixed(dict_real)

    # Replace graphical functions in all equations
    for (i in seq_len(nrow(sfm[["variables"]]))) {
      if ("eqn_insightmaker" %in% colnames(sfm[["variables"]])) {
        if (!is.na(sfm[["variables"]][i, "eqn_insightmaker"])) {
          sfm[["variables"]][i, "eqn_insightmaker"] <- stringr::str_replace_all(
            sfm[["variables"]][i, "eqn_insightmaker"], dict_temp
          ) |>
            stringr::str_replace_all(dict_real)
        }
      }
    }
  }

  return(sfm)
}


#' Prepare Insight Maker equation
#'
#' @param eqn Insight Maker equation to prepare
#'
#' @returns List with eqn and doc
#' @noRd
prep_eqn_IM <- function(eqn) {
  # HTML and escape character replacements
  eqn <- textutils::HTMLdecode(eqn) |>
    gsub("\"", "'", x = _, fixed = TRUE) |>
    gsub("\'", "'", x = _, fixed = TRUE) |>
    gsub("\\n", "\n", x = _, fixed = TRUE) |>
    # stringr::str_replace_all(stringr::fixed("\""), "'") |>
    # stringr::str_replace_all(stringr::fixed("\'"), "'") |>
    # stringr::str_replace_all(stringr::fixed("\\n"), "\n") |>
    trimws() |>
    # Ensure there is no scientific notation
    scientific_notation()

  # Replace_comments
  eqn <- replace_comments(eqn)

  # Extract and remove comments
  out <- remove_comments(eqn)
  return(out)
}


#' Replace Insight Maker ids
#'
#' Replace Insight Maker name with id, replace id with name
#'
#' @param string String to apply replacement to
#' @param original Vector with strings to replace
#' @param replacement Vector with strings as replacements
#' @param with_brackets Boolean; whether to include square brackets around the match and replacement
#' @param ignore_case Logical; whether to replace names in a case-insensitive manner. By default TRUE, as Insight Maker is generally not case-sensitive.
#'
#' @returns Updated string
#' @noRd
#'
replace_names_IM <- function(string, original, replacement,
                             with_brackets = TRUE,
                             ignore_case = TRUE) {
  if (is.null(string)) {
    return(string)
  } else {
    replace_dict <- stats::setNames(
      paste0(
        ifelse(with_brackets, "\\[", ""), replacement,
        ifelse(with_brackets, "\\]", "")
      ),
      # First make string suitable for regular expressions by adding escape characters, then add brackets around which cannot be preceded by a square bracket (in order to correctly translate conveyors)
      paste0(
        ifelse(with_brackets, "(?<!\\[)\\[", ""),
        stringr::str_escape(original),
        ifelse(with_brackets, "\\]", "")
      )
    )

    new_string <- stringr::str_replace_all(
      string,
      stringr::regex(replace_dict, ignore_case = ignore_case)
    )
    return(new_string)
  }
}


#' Prepare units for Julia's Unitful package
#'
#' @inheritParams build
#' @inheritParams clean_unit
#'
#' @returns Updated sfm
#' @noRd
#'
clean_units_IM <- function(sfm, regex_units) {
  # Get names of all model elements
  var_names <- get_model_var(sfm)

  # Remove year because Insight Maker's year is 365 days
  regex_units <- regex_units[!regex_units %in% c("yr", "month", "quarter")]
  regex_units <- c(regex_units, c(
    "^[Y|y]ear[s]?$" = "common_yr",
    "^[Q|q]uarter[s]?$" = "common_quarter",
    "^[M|m]onth[s]?$" = "common_month"
  ))


  # Define custom units
  if (!is.null(sfm[["model_units_temp"]])) {
    custom_units_df <- sfm[["model_units_temp"]]

    # Create new equation if base is defined; base can now be discarded
    custom_units_df[["eqn"]] <- ifelse(nzchar(custom_units_df[["base"]]),
      paste0(custom_units_df[["factor"]], " ", custom_units_df[["base"]]),
      custom_units_df[["factor"]]
    )

    # Clean units and keep mapping between old and new unit
    name_translation <- lapply(custom_units_df[["name"]], function(y) {
      clean_unit(y, regex_units,
        ignore_case = TRUE, include_translation = TRUE,
        unit_name = TRUE
      )
    })
    eqn_translation <- lapply(custom_units_df[["eqn"]], function(y) {
      clean_unit(y, regex_units, ignore_case = TRUE, include_translation = TRUE)
    })

    # Add translated units
    custom_units_df[["new_name"]] <- unlist(lapply(name_translation, `[[`, "x_new"))
    custom_units_df[["new_eqn"]] <- unlist(lapply(eqn_translation, `[[`, "x_new"))

    # Remove custom units of which all parts already exist
    idx_keep <- lapply(seq_along(name_translation), function(i) {
      x <- name_translation[i]
      # Check whether all parts already exist; only check for parts with letters in them
      not_all_parts_exist <- !all(x[["x_parts"]][grepl("[a-zA-Z]", x[["x_parts"]])] %in% c(custom_units_df[["new_name"]][-i], unname(regex_units)))
      # If the equation isn't zero, the existing unit is otherwise defined
      not_all_parts_exist | custom_units_df[["new_eqn"]][i] != "1"
    }) |> unlist()

    if (any(idx_keep)) {
      custom_units_df <- custom_units_df[idx_keep, ]

      # Create list of model units to add
      # Both name and eqn now need to be cleaned and split into parts; we cannot define units with a name such as "whales^2"; instead we should define "whale"
      add_model_units <- detect_undefined_units(sfm,
        new_eqns = "",
        new_units = c(
          custom_units_df[["new_name"]],
          custom_units_df[["new_eqn"]]
        ),
        regex_units = regex_units, R_or_Julia = "R"
      )

      # Create list with custom unit definitions
      custom_units_list <- lapply(seq_len(nrow(custom_units_df)), function(i) {
        custom_unit <- custom_units_df[i, ][["new_name"]]

        list(name = custom_unit, eqn = custom_units_df[i, ][["new_eqn"]], prefix = FALSE)
      }) |> stats::setNames(custom_units_df[["new_name"]])

      # Add to regex dictionary
      custom_dict <- c(
        unlist(lapply(name_translation, `[[`, "x_parts")),
        unlist(lapply(eqn_translation, `[[`, "x_parts"))
      )
      custom_dict <- custom_dict[unname(custom_dict) %in% names(add_model_units)]
      custom_dict <- custom_dict[!duplicated(custom_dict)] # Remove duplicate entries

      if (length(custom_dict)) {
        names(custom_dict) <- paste0("^", stringr::str_escape(names(custom_dict)), "$")
        regex_units <- c(custom_dict, regex_units)
      }

      # Create custom model units
      sfm[["model_units"]] <- add_model_units |>
        # Overwrite units which already have a definition in custom_units
        utils::modifyList(custom_units_list[names(custom_units_list) %in% names(add_model_units)])
    }
  }

  sfm[["model_units_temp"]] <- NULL

  # Define function to clean units contained in curly brackets
  clean_units_curly <- function(x, regex_units) {
    # First check if there are any curly brackets
    if (!grepl("\\{", x)) {
      return(x)
    }

    # Get indices of curly brackets
    paired_idxs <- get_range_all_pairs(x, var_names, type = "curly", names_with_brackets = TRUE)

    if (nrow(paired_idxs) == 0) {
      return(x)
    }

    # At least one letter needs to be in between the curly brackets and there cannot be commas
    paired_idxs <- paired_idxs[stringr::str_detect(paired_idxs[["match"]], "[a-zA-Z]") &
      !stringr::str_detect(paired_idxs[["match"]], ","), ]

    if (nrow(paired_idxs) == 0) {
      return(x)
    }

    # Apply clean unit here already, because Insight Maker doesn't care about case, though it will also be applied when converting equations
    # Find replacements by applying clean_unit()
    replacements <- lapply(
      seq.int(nrow(paired_idxs)),
      function(i) {
        # Remove curly brackets
        # Insight Maker treats units as case-insensitive
        stringr::str_sub(tolower(x), paired_idxs[i, "start"] + 1, paired_idxs[i, "end"] - 1) |>
          clean_unit(regex_units, ignore_case = TRUE)
      }
    )

    # Replace in reverse order
    for (i in rev(seq.int(nrow(paired_idxs)))) {
      stringr::str_sub(x, paired_idxs[i, "start"], paired_idxs[i, "end"]) <- paste0("u(\"", replacements[[i]], "\")")
    }

    return(x)
  }


  # Replace units in macros - only one macro in case of Insight Maker model
  sfm[["macros_temp"]][["eqn"]] <- clean_units_curly(sfm[["macros_temp"]][["eqn"]], regex_units)

  # Replace units in equations and unit definition in data frame
  for (i in seq_len(nrow(sfm[["variables"]]))) {
    if (!is.na(sfm[["variables"]][i, "eqn"])) {
      sfm[["variables"]][i, "eqn"] <- clean_units_curly(sfm[["variables"]][i, "eqn"], regex_units)
    }
    if (!is.na(sfm[["variables"]][i, "units"])) {
      sfm[["variables"]][i, "units"] <- clean_unit(tolower(sfm[["variables"]][i, "units"]), 
                                                      regex_units, ignore_case = TRUE)
    }
  }

  # Ensure all units are defined
  add_model_units <- detect_undefined_units(sfm,
    new_eqns = c(
      sfm[["variables"]][["eqn"]],
      sfm[["macros_temp"]][["eqn"]]
    ),
    new_units = sfm[["variables"]][["units"]],
    regex_units = regex_units, R_or_Julia = "R"
  )
  sfm[["model_units"]] <- utils::modifyList(add_model_units, sfm[["model_units"]])


  sfm <- validate_xmile(sfm)

  return(sfm)
}


#' Check non-negative stocks and flows
#'
#' @inheritParams build
#' @inheritParams insightmaker_to_sfm
#'
#' @returns Updated sfm
#' @noRd
#'
check_nonnegativity <- function(sfm, keep_nonnegative_flow,
                                keep_nonnegative_stock, keep_solver) {
  # Non-negative Stocks and Flows
  stock_idx <- sfm[["variables"]][["type"]] == "stock"
  if (!any(stock_idx)) {
    return(sfm)
  }

  # Check for non-negative stocks
  if ("non_negative" %in% colnames(sfm[["variables"]])) {
    nonneg_stock_idx <- stock_idx & sfm[["variables"]][["non_negative"]]
    nonneg_stock <- which(nonneg_stock_idx)
  } else {
    nonneg_stock <- integer(0)
  }

  if (keep_nonnegative_stock && length(nonneg_stock) > 0) {
    if (!keep_solver && sfm[["sim_specs"]][["method"]] == "rk4") {
      cli::cli_inform(c(
        "Adjusting solver for non-negative stocks.",
        "i" = "Non-negative stocks detected in the model.",
        ">" = "Switching ODE solver from {.fn rk4} to {.fn euler} for consistency with {.pkg InsightMaker}.",
        "i" = "Disable this by setting {.arg keep_solver = TRUE}."
      ))
      sfm[["sim_specs"]][["insightmaker_method"]] <- sfm[["sim_specs"]][["method"]]
      sfm[["sim_specs"]][["method"]] <- "euler"
    }
  }

  return(sfm)
}


#' Convert global Insight Maker script to macros
#'
#' @inheritParams build
#' @inheritParams clean_unit
#'
#' @returns Updated stock-and-flow model with macros
#' @noRd
convert_macros_IM_wrapper <- function(sfm, regex_units) {
  if (nzchar(sfm[["macros_temp"]][["eqn"]])) {
    # Convert each equation and create list of model elements to add
    var_names <- get_model_var(sfm)

    sfm <- replace_macro_names_IM(sfm)

    # Convert equations in macro
    out <- convert_equations_IM(
      type = P[["macro_name"]],
      name = P[["macro_name"]],
      eqn = sfm[["macros_temp"]][["eqn"]],
      var_names = var_names,
      regex_units = regex_units
    )

    if (P[["debug"]]) {
      cli::cli_inform(c(
        "i" = "Conversion output: {out}"
      ))
    }

    sfm[["macros_temp"]][["eqn"]] <- out[["eqn"]]

    # Extract names and separate equations
    sfm <- split_macros_IM(sfm)
  }

  # Remove placeholder
  sfm[["macros_temp"]] <- NULL

  return(sfm)
}


#' Replace Insight Maker names in macros with syntactically valid names
#'
#' @inheritParams build
#'
#' @returns Updated stock-and-flow model with replaced macro names all throughout the model
#' @noRd
replace_macro_names_IM <- function(sfm) {
  eqn <- sfm[["macros_temp"]][["eqn"]]

  # Get names of all model elements
  var_names <- get_model_var(sfm)

  # Variables cannot be the same name as Insight Maker functions
  IM_func_names <- syntax_IM[["conv_df"]][["insightmaker"]]

  # Find functions
  functions <- stringr::str_locate_all(eqn, stringr::regex("(\\n|^)[ ]*function\\b", ignore_case = TRUE))[[1]]

  # For each <-, find preceding \n and next <-
  newlines <- unique(c(1, stringr::str_locate_all(eqn, "\\n")[[1]][, "start"], nchar(eqn)))
  assignment_op <- stringr::str_locate_all(eqn, "<-")[[1]]

  # Exclude <- & \n in comments and strings
  seq_quot <- get_seq_exclude(eqn, var_names = NULL, type = "quot")

  functions <- functions[!(functions[, "start"] %in% seq_quot), , drop = FALSE]
  assignment_op <- assignment_op[!(assignment_op[, "start"] %in% seq_quot), , drop = FALSE]
  newlines <- newlines[!(newlines %in% seq_quot)]

  # Merge functions and assignments
  assignment <- rbind(assignment_op, functions)
  assignment <- assignment[order(assignment[, "start"]), , drop = FALSE]

  if (nrow(assignment) > 0 && length(newlines) > 0) {
    # Find preceding newline before assignment
    start_idxs <- vapply(assignment[, "start"], function(idx) {
      idxs_newline <- which(newlines <= idx)
      newlines[idxs_newline[length(idxs_newline)]] # select last newline before assignment
    }, numeric(1))

    # Split macros by assignment
    split_macros <- lapply(seq_len(nrow(assignment)), function(i) {
      # Extract equation indices
      start_eqn <- start_idxs[i]
      end_eqn <- ifelse(i == nrow(assignment), nchar(eqn), start_idxs[i + 1] - 1)

      # Extract name
      name_insightmaker <- trimws(stringr::str_sub(eqn, start_eqn, assignment[i, "start"] - 1))

      is_multiline_function <- stringr::str_detect(
        stringr::str_sub(eqn, start_eqn, start_eqn + nchar("function")),
        stringr::regex("function", ignore_case = TRUE)
      )

      names_arg <- names_arg_insightmaker <- c()
      if (is_multiline_function) {
        # Extract function name; In a multiline function, the function name comes after function
        sub_eqn <- stringr::str_sub(eqn, start_eqn, end_eqn)
        name_insightmaker <- trimws(sub("^function", "",
          sub("\\(.*", "", sub_eqn, ignore.case = TRUE),
          ignore.case = TRUE
        ))

        # Variables cannot be the same name as Insight Maker functions
        name <- clean_name(name_insightmaker,
          protected = c(
            var_names,
            IM_func_names, tolower(IM_func_names),
            toupper(IM_func_names)
          )
        )

        # Add opening bracket
        name_insightmaker <- paste0(name_insightmaker, "(")
        name <- paste0(name, "(")
      } else {
        # Take care of function assignment
        is_function <- stringr::str_detect(name_insightmaker, "\\(")

        if (is_function) {
          # Extract argument names
          arg <- trimws(stringr::str_extract(name_insightmaker, "\\(.*\\)$"))
          arg <- parse_args(stringr::str_replace_all(arg, c("^\\(" = "", "\\)$" = "")))

          # Find names and values of arguments
          # contains_name <- stringr::str_detect(arg, "=")
          arg_split <- stringr::str_split_fixed(arg, "=", n = 2)
          names_arg_insightmaker <- arg_split[, 1]
          names_arg <- clean_name(names_arg_insightmaker,
            protected = c(
              var_names,
              IM_func_names, tolower(IM_func_names),
              toupper(IM_func_names)
            )
          )

          # Extract function name, but add opening bracket
          name_insightmaker <- trimws(sub("\\(.*", "", name_insightmaker))

          name <- clean_name(name_insightmaker,
            protected = c(
              var_names,
              IM_func_names, tolower(IM_func_names),
              toupper(IM_func_names)
            )
          )

          # Add opening bracket
          name_insightmaker <- paste0(name_insightmaker, "(")
          name <- paste0(name, "(")
        } else {
          # Variables cannot be the same name as Insight Maker functions
          name <- clean_name(name_insightmaker,
            protected = c(
              var_names,
              IM_func_names, tolower(IM_func_names),
              toupper(IM_func_names)
            )
          )
        }
      }

      z <- list(
        start_eqn = start_eqn,
        end_eqn = end_eqn,
        name = name,
        name_insightmaker = name_insightmaker,
        names_arg = names_arg,
        names_arg_insightmaker = names_arg_insightmaker
      )
      return(z)
    })


    # Replace argument names only in those parts that are functions. This needs to be done after split_macros to preserve indices.
    # Reverse order indices
    for (i in rev(seq_len(nrow(assignment)))) {
      if (length(split_macros[[i]][["names_arg"]]) > 0) {
        # Construct replacement dictionary for replacing argument names in this equation
        dict <- stats::setNames(split_macros[[i]][["names_arg"]], paste0("\\b", stringr::str_escape(split_macros[[i]][["names_arg_insightmaker"]]), "\\b"))

        # Important! Don't simply replace names, as some names may be in (unit) strings.
        # Even arguments are not case-sensitive
        stringr::str_sub(eqn, split_macros[[i]][["start_eqn"]], split_macros[[i]][["end_eqn"]]) <- replace_safely(
          eqn = stringr::str_sub(eqn, split_macros[[i]][["start_eqn"]], split_macros[[i]][["end_eqn"]]),
          dict = dict,
          var_names = var_names,
          ignore_case = TRUE
        )
      }
    }

    # Construct replacement dictionary for replacing names in macros and other equations
    # Important: even if the name did not need to be changed, still apply dict because of differences in case
    old_names <- vapply(split_macros, `[[`, character(1), "name_insightmaker")
    new_names <- vapply(split_macros, `[[`, character(1), "name")
    dict <- stats::setNames(new_names, paste0("\\b", stringr::str_escape(old_names), "\\b"))

    # Insight Maker is not case-sensitive!
    # Important! Don't simply replace names, as some names may be in (unit) strings.
    sfm[["macros_temp"]][["eqn"]] <- replace_safely(
      eqn = sfm[["macros_temp"]][["eqn"]],
      dict = dict,
      var_names = var_names, ignore_case = TRUE
    )

    # Use same dictionary to replace macro names in other equations
    # Replace in equations in data frame
    for (i in seq_len(nrow(sfm[["variables"]]))) {
      if (!is.na(sfm[["variables"]][i, "eqn"])) {
        # Important! Don't simply replace names, as some names may be in (unit) strings.
        sfm[["variables"]][i, "eqn"] <- replace_safely(
          eqn = sfm[["variables"]][i, "eqn"],
          dict = dict,
          var_names = var_names, ignore_case = TRUE
        )
      }
    }
  }

  return(sfm)
}


#' Replace dictionary matches in equation safely
#'
#' @param eqn Equation
#' @param dict Dictionary
#' @param var_names Variable names
#' @param ignore_case If TRUE, ignore case. Defaults to TRUE.
#'
#' @returns Updated equation
#' @noRd
replace_safely <- function(eqn, dict, var_names, ignore_case = TRUE) {
  # Remove those matches that are in quotation marks or names
  idxs_exclude <- get_seq_exclude(eqn, var_names)

  idx_df <- lapply(seq_along(dict), function(i) {
    matches <- gregexpr(names(dict)[i], eqn, perl = TRUE, ignore.case = ignore_case)[[1]]

    if (matches[1] == -1) {
      return(data.frame(start = integer(), end = integer()))
    } else {
      data.frame(
        match = rep(names(dict)[i], length(matches)),
        replacement = rep(unname(dict)[i], length(matches)),
        start = as.integer(matches),
        end = as.integer(matches + attr(matches, "match.length") - 1)
      )
    }
  }) |>
    do.call(rbind, args = _) |>
    as.data.frame()


  if (nrow(idx_df) > 0) idx_df <- idx_df[!(idx_df[["start"]] %in% idxs_exclude | idx_df[["end"]] %in% idxs_exclude), ]
  if (nrow(idx_df) > 0) {
    # Replace in reverse order
    for (i in rev(seq.int(nrow(idx_df)))) {
      stringr::str_sub(eqn, idx_df[i, "start"], idx_df[i, "end"]) <- idx_df[i, "replacement"]
    }
  }

  return(eqn)
}


#' Split macro equation and names
#'
#' @inheritParams build
#'
#' @returns Updated stock-and-flow model with split macros
#' @noRd
split_macros_IM <- function(sfm) {
  eqn <- sfm[["macros_temp"]][["eqn"]]
  assignment <- stringr::str_locate_all(eqn, "=")[[1]]
  newlines <- unique(c(1, stringr::str_locate_all(eqn, "\\n")[[1]][, "start"], nchar(eqn)))

  # Exclude <- & \n in comments and strings
  seq_quot <- get_seq_exclude(eqn, var_names = NULL, type = "quot")
  assignment <- assignment[!(assignment[, "start"] %in% seq_quot), , drop = FALSE]
  newlines <- newlines[!(newlines %in% seq_quot)]

  if (nrow(assignment) > 0) {
    # Identify curly brackets to exclude assignment matches in for-loops, while-loops, ifelse etc.
    paired_idxs <- get_range_all_pairs(eqn, var_names = NULL, type = "curly", names_with_brackets = FALSE)

    if (nrow(paired_idxs) > 0) {
      idxs_curly <- unlist(mapply(seq, paired_idxs[, "start"], paired_idxs[, "end"], SIMPLIFY = FALSE))
      assignment <- assignment[!(assignment[, "start"] %in% idxs_curly), , drop = FALSE]
      newlines <- newlines[!(newlines %in% idxs_curly)]
    }

    # Find preceding newline before assignment
    start_idxs <- vapply(assignment[, "start"], function(idx) {
      idxs_newline <- which(newlines <= idx)
      newlines[idxs_newline[length(idxs_newline)]] # select last newline before assignment
    }, numeric(1))

    # Split macros by assignment
    split_macros <- lapply(seq_len(nrow(assignment)), function(i) {
      # Extract equation indices
      start_eqn <- start_idxs[i]
      end_eqn <- ifelse(i == nrow(assignment), nchar(eqn), start_idxs[i + 1] - 1)

      # Extract name
      name <- trimws(stringr::str_sub(eqn, start_eqn, assignment[i, "start"] - 1))
      sub_eqn <- trimws(stringr::str_sub(eqn, assignment[i, "end"] + 1, end_eqn))
      return(list(name = name, eqn = sub_eqn))
    })

    new_macros <- stats::setNames(split_macros, vapply(split_macros, `[[`, character(1), "name"))

    # Add original equation and documentation to first macro - it will be deleted afer
    new_macros[[1]][["eqn_insightmaker"]] <- sfm[["macros_temp"]][["eqn_insightmaker"]]
    new_macros[[1]][["doc"]] <- sfm[["macros_temp"]][["doc"]]

    sfm[[P[["macro_name"]]]] <- new_macros
  }

  return(sfm)
}


#' Convert Insight Maker equations to R code
#'
#' @inheritParams build
#' @inheritParams clean_unit
#'
#' @returns Updated stock-and-flow model with converted equations to R.
#' @noRd
#'
convert_equations_IM_wrapper <- function(sfm, regex_units) {
  # Convert each equation and create list of model elements to add
  var_names <- get_model_var(sfm)

  # Get variables to convert (stock, aux, flow)
  convert_idx <- sfm[["variables"]][["type"]] %in% c("stock", "aux", "flow")
  
  if (!any(convert_idx)) {
    return(sfm)
  }
  
  # Initialize transformation tracking if debug mode is on
  tracker <- if (P[["debug"]]) create_transformation_tracker() else NULL
  
  # Accumulate variables to add to the model
  accumulated_add_vars_aux <- list()
  accumulated_add_vars_gf <- list()
  
  # Convert equations for each variable using flat list structure
  for (i in which(convert_idx)) {
    var_name <- sfm[["variables"]][i, "name"]
    var_type <- sfm[["variables"]][i, "type"]
    eqn_before <- sfm[["variables"]][i, "eqn"]
    
    # Convert equation
    out <- convert_equations_IM(
      type = var_type,
      name = var_name,
      eqn = eqn_before,
      var_names = var_names,
      regex_units = regex_units
    )
    
    # Track transformation
    if (!is.null(tracker)) {
      eqn_after <- out[["eqn"]]
      n_aux_created <- length(out[["add_vars_aux"]])
      n_gf_created <- length(out[["add_vars_gf"]])
      
      if (eqn_before != eqn_after || n_aux_created > 0 || n_gf_created > 0) {
        log_transformation(tracker, var_name, "equation_conversion", list(
          variable_type = var_type,
          functions_used = paste(out[["translated_func"]], collapse = ", "),
          auxiliary_vars = n_aux_created,
          graphical_functions = n_gf_created,
          equation_changed = eqn_before != eqn_after
        ))
      }
    }
    
    # Update the variables data frame with converted equation
    sfm[["variables"]][i, "eqn"] <- out[["eqn"]]
    
    # Accumulate auxiliary and graphical function variables
    if (length(out[["add_vars_aux"]]) > 0) {
      accumulated_add_vars_aux <- append(accumulated_add_vars_aux, out[["add_vars_aux"]])
      if (!is.null(tracker)) {
        for (aux_var_name in names(out[["add_vars_aux"]])) {
          log_transformation(tracker, aux_var_name, "create_auxiliary", list(
            created_by = var_name,
            equation = out[["add_vars_aux"]][[aux_var_name]]
          ))
        }
      }
    }
    if (length(out[["add_vars_gf"]]) > 0) {
      accumulated_add_vars_gf <- append(accumulated_add_vars_gf, out[["add_vars_gf"]])
      if (!is.null(tracker)) {
        for (gf_var_name in names(out[["add_vars_gf"]])) {
          log_transformation(tracker, gf_var_name, "create_graphical_function", list(
            created_by = var_name,
            interpolation = out[["add_vars_gf"]][[gf_var_name]][["interpolation"]]
          ))
        }
      }
    }
  }

  # Add accumulated auxiliary and graphical function variables to the model
  sfm <- add_accumulated_variables(sfm, accumulated_add_vars_aux, accumulated_add_vars_gf)

  # Print transformation summary if debug mode is on
  if (!is.null(tracker) && P[["debug"]]) {
    cli::cli_h2("IM\u2192R Conversion Summary")
    for (line in summarize_transformations(tracker)) {
      cli::cli_text(line)
    }
  }

  sfm <- validate_xmile(sfm)

  return(sfm)
}


#' Remove brackets around R names
#'
#' Add units and add sources for graphical functions
#'
#' @inheritParams clean_units_IM
#' @inheritParams build
#'
#' @returns Updated sfm
#' @noRd
#'
remove_brackets_from_names <- function(sfm) {
  # Remove brackets
  var_names <- get_model_var(sfm)
  dict <- stringr::fixed(stats::setNames(var_names, paste0("[", var_names, "]")))

  for (i in seq_len(nrow(sfm[["variables"]]))) {
    if (!is.na(sfm[["variables"]][i, "eqn"])) {
      sfm[["variables"]][i, "eqn"] <- stringr::str_replace_all(
        sfm[["variables"]][i, "eqn"], dict
      )
    }
  }

  return(sfm)
}


#' Split auxiliaries into static parameters or dynamic variables
#'
#' @inheritParams build
#'
#' @returns Vector of variable names that are constants
#' @noRd
#'
split_aux_wrapper <- function(sfm) {
  # Get names
  var_names <- get_model_var(sfm)

  # Get auxiliary variables
  aux_idx <- sfm[["variables"]][["type"]] == "aux"
  if (!any(aux_idx)) {
    return(character(0))
  }
  
  aux_eqns <- sfm[["variables"]][aux_idx, "eqn"]
  names(aux_eqns) <- sfm[["variables"]][aux_idx, "name"]
  
  # Separate auxiliary variables into static parameters and dynamically updated auxiliaries
  dependencies <- find_dependencies_(sfm, eqns = aux_eqns, only_model_var = FALSE)

  # Constants are not dependent on time, have no dependencies in names, or are only dependent on constants
  temp <- dependencies
  temp <- temp[vapply(temp, function(x) {
    (!P[["time_name"]] %in% x) & (length(intersect(x, var_names)) == 0)
  }, logical(1))]
  constants <- names(temp)
  rm(temp)

  # Iteratively find constants
  done <- FALSE
  if (!done) {
    old_constants <- constants

    # Are there any remaining auxiliary variables to be split into constants or aux?
    aux_names <- sfm[["variables"]][aux_idx, "name"]
    remaining_aux <- setdiff(aux_names, constants)
    if (length(remaining_aux) == 0) {
      done <- TRUE
    } else {
      new_constants <- dependencies[remaining_aux]
      idx <- unlist(lapply(new_constants, function(x) {
        (!P[["time_name"]] %in% x) & all(intersect(x, var_names) %in% constants)
      }))
      new_constants <- new_constants[idx]
      constants <- c(constants, names(new_constants))

      # While-loop ends if there is no change
      if (setequal(old_constants, constants)) {
        done <- TRUE
      }
    }
  }

  # Update variable types in data frame
  for (const_name in constants) {
    idx <- sfm[["variables"]][["name"]] == const_name
    sfm[["variables"]][idx, "type"] <- "constant"
  }

  sfm <- validate_xmile(sfm)

  return(sfm)
}
