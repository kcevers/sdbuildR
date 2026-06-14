#' Transform Insight Maker eqn to R code
#'
#' @param eqn String with Insight Maker eqn, but translated R names
#' @param var_names data.frame with type, name, and label per variable
#' @param name R name of variable to which the eqn belongs
#' @param type Name of model element to which the eqn belongs
#'
#' @returns List with flat structure:
#'   - eqn: Converted equation string
#'   - translated_func: Functions used in equation translation
#'   - add_vars: data.frame with variables that need to be added to the model
#'   - doc: Documentation extracted from comments
#' @noRd
#' @importFrom rlang .data
#'
convert_equations_IM <- function(type,
                                 name,
                                 eqn,
                                 var_names) {
  if (P[["debug"]]) {
    cli::cli_inform(c(" " = ""))
    cli::cli_inform(c("i" = type))
    cli::cli_inform(c("i" = name))
    cli::cli_inform(c("i" = eqn))
  }

  if (is.null(eqn) || !nzchar(eqn)) {
    return(empty_IM_conversion())
  }

  eqn <- prepare_syntax_IM(eqn, var_names)
  eqn <- convert_statements_IM(eqn, var_names)
  eqn <- convert_operators_IM(eqn, var_names)

  conv_list <- convert_builtin_functions_IM(type, name, eqn, var_names)
  eqn <- postprocess_equation_IM(conv_list[["eqn"]], name)

  if (P[["debug"]]) {
    cli::cli_inform(c("i" = eqn))
  }

  list(
    eqn = eqn,
    translated_func = conv_list[["translated_func"]],
    add_vars = conv_list[["add_vars"]],
    doc = ""
  )
}


#' Empty Insight Maker equation conversion result
#'
#' @returns Standard empty conversion result.
#' @noRd
empty_IM_conversion <- function() {
  list(
    eqn = "",
    translated_func = c(),
    add_vars = data.frame(),
    doc = ""
  )
}


#' Prepare Insight Maker syntax before statement conversion
#'
#' @inheritParams convert_equations_IM
#' @returns Updated equation string.
#' @noRd
prepare_syntax_IM <- function(eqn, var_names) {
  curly_to_vector_brackets(eqn, var_names)
}


#' Convert Insight Maker statements to R statements
#'
#' @inheritParams convert_equations_IM
#' @returns Updated equation string.
#' @noRd
convert_statements_IM <- function(eqn, var_names) {
  convert_all_statements(eqn, var_names)
}


#' Convert Insight Maker operators to R operators
#'
#' @inheritParams convert_equations_IM
#' @returns Updated equation string.
#' @noRd
convert_operators_IM <- function(eqn, var_names) {
  eqn |>
    replace_op_IM(var_names) |>
    replace_colon(var_names)
}


#' Post-process converted Insight Maker equation text
#'
#' @inheritParams convert_equations_IM
#' @returns Updated equation string.
#' @noRd
postprocess_equation_IM <- function(eqn, name) {
  eqn <- stringr::str_replace_all(trimws(eqn), "\\\n[ ]*\\\n", "\n")

  eqn <- trimws(eqn)
  if (stringr::str_detect(eqn, stringr::fixed("\n")) && !(name %in% P[["func_name"]])) {
    eqn <- paste0("{\n", eqn, "\n}")
  }

  eqn
}


#' Collect candidate Insight Maker builtin-function matches
#'
#' @inheritParams convert_equations_IM
#' @param syntax_df Syntax table for supported functions.
#' @param IM_regex Compiled regex used for this matching pass.
#'
#' @returns List with match data frame, keep index, and rows per syntax.
#' @noRd
collect_IM_function_matches <- function(eqn, syntax_df, IM_regex, var_names) {
  idx_list <- stringr::str_locate_all(eqn, IM_regex)
  nrow_per_idx <- vapply(idx_list, nrow, integer(1))
  idx_keep <- nrow_per_idx > 0
  idx_list <- idx_list[idx_keep]

  if (length(idx_list) == 0) {
    return(list(
      idx_df = data.frame(),
      idx_keep = idx_keep,
      nrow_per_idx = nrow_per_idx
    ))
  }

  rep_syntax_df <- syntax_df[idx_keep, ]
  rep_syntax_df <- rep_syntax_df[rep(seq_len(nrow(rep_syntax_df)), nrow_per_idx[idx_keep]), ]

  idx_df <- cbind(rep_syntax_df, bind_rows_(idx_list))
  idx_df <- idx_df[order(idx_df[["insightmaker"]], idx_df[["start"]], -idx_df[["end"]]), ]
  idx_df <- idx_df[!duplicated(idx_df[, c("insightmaker", "start")]), ]
  rownames(idx_df) <- NULL

  idxs_exclude <- get_seq_exclude(eqn, var_names, names_with_brackets = TRUE)
  if (nrow(idx_df) > 0) {
    idx_df <- idx_df[!(idx_df[["start"]] %in% idxs_exclude | idx_df[["end"]] %in% idxs_exclude), ]
  }

  list(
    idx_df = idx_df,
    idx_keep = idx_keep,
    nrow_per_idx = nrow_per_idx
  )
}


#' Mark matched Insight Maker functions before replacement
#'
#' Adds a temporary `_replace` suffix to detected functions so builtins whose R
#' and InsightMaker names are identical do not loop forever.
#'
#' @inheritParams convert_equations_IM
#' @param idx_df Match data frame from collect_IM_function_matches().
#'
#' @returns Updated equation string.
#' @noRd
mark_IM_function_matches <- function(eqn, idx_df) {
  idx_df <- idx_df[order(idx_df[["start"]]), ]
  idx_df[["insightmaker_regex"]] <- stringr::str_replace_all(
    idx_df[["insightmaker_regex"]],
    stringr::fixed(c(
      "(?:^|(?<=\\W))" = "", "(?=(?:\\W|$))" = "",
      "\\b" = "", "\\(" = "(", "\\)" = ")"
    ))
  )

  apply_replacements_reversed(eqn, idx_df, idx_df[["insightmaker_regex"]])
}


#' Report unsupported Insight Maker functions remaining in an equation
#'
#' @inheritParams convert_equations_IM
#' @param syntax_df_unsupp Syntax table for unsupported functions.
#'
#' @returns Invisibly returns NULL.
#' @noRd
report_unsupported_IM_functions <- function(eqn, name, var_names, syntax_df_unsupp) {
  eqn_split <- strsplit(eqn, "")[[1]]

  idxs_exclude <- get_seq_exclude(eqn, var_names, names_with_brackets = TRUE)
  if (!is.null(idxs_exclude)) {
    eqn_no_names <- paste0(eqn_split[-idxs_exclude], collapse = "")
  } else {
    eqn_no_names <- paste0(eqn_split, collapse = "")
  }

  syntax4 <- syntax_df_unsupp[
    syntax_df_unsupp[["syntax"]] %in% c("syntax4", "syntax4b"), ,
    drop = FALSE
  ]
  idx_ABM <- stringr::str_detect(eqn_no_names, syntax4[["insightmaker_regex"]])

  if (any(idx_ABM)) {
    cli::cli_inform(c(
      "!" = "Agent-Based Modelling functions were detected in equation of {name}, and won't be translated:",
      ">" = paste0(paste0(syntax4[idx_ABM, "insightmaker"], ")"), collapse = ", ")
    ))
  }

  syntax5 <- syntax_df_unsupp[
    syntax_df_unsupp[["syntax"]] %in% c("syntax5", "syntax5b"), ,
    drop = FALSE
  ]
  idx5 <- stringr::str_detect(eqn_no_names, syntax5[["insightmaker_regex"]])

  if (any(idx5)) {
    cli::cli_inform(
      "Unsupported Insight Maker functions were detected in equation of ",
      name, ", and won't be translated: "
    )
    cli::cli_inform(paste0(syntax5[idx5, "insightmaker"], ")"))
  }

  invisible(NULL)
}


#' Build a replacement for a matched Insight Maker builtin
#'
#' @inheritParams convert_equations_IM
#' @param idx_func One matched function row from select_innermost_function().
#' @param arg Parsed arguments for the matched function.
#' @param translated_func Functions already translated in this equation.
#'
#' @returns List with replacement text, replacement bounds, and add_var.
#' @noRd
build_IM_builtin_replacement <- function(eqn, idx_func, arg, name, var_names, translated_func) {
  add_var <- data.frame()

  if (idx_func[["syntax"]] %in% c("syntax0", "syntax0b")) {
    return(list(
      replacement = idx_func[["R"]],
      start_idx = idx_func[["start"]],
      end_idx = idx_func[["end"]],
      add_var = add_var
    ))
  }

  if (idx_func[["syntax"]] == "syntax1") {
    if (as.logical(idx_func[["add_c()"]]) && length(arg) > 1) {
      arg <- paste0("c(", paste0(arg, collapse = ", "), ")")
    } else {
      arg <- paste0(arg, collapse = ", ")
    }

    return(list(
      replacement = sprintf(
        "%s(%s%s%s)",
        idx_func[["R"]],
        idx_func[["add_first_arg"]],
        ifelse(nzchar(idx_func[["add_first_arg"]]) & nzchar(arg), ", ", ""),
        arg
      ),
      start_idx = idx_func[["start"]],
      end_idx = idx_func[["end"]],
      add_var = add_var
    ))
  }

  if (idx_func[["syntax"]] == "syntax1b") {
    return(list(
      replacement = sprintf("%s(%s)", idx_func[["R"]], idx_func[["add_first_arg"]]),
      start_idx = idx_func[["start"]],
      end_idx = idx_func[["end"]],
      add_var = add_var
    ))
  }

  if (idx_func[["syntax"]] == "syntax2") {
    prefunc_arg <- extract_prefunc_args(eqn, var_names,
      start_func = idx_func[["start"]],
      names_with_brackets = TRUE
    )

    return(list(
      replacement = sprintf(
        "%s(%s%s%s%s%s)",
        idx_func[["R"]],
        idx_func[["add_first_arg"]],
        ifelse(nzchar(idx_func[["add_first_arg"]]), ", ", ""),
        prefunc_arg,
        ifelse(nzchar(arg[1]), ", ", ""),
        paste0(arg, collapse = ", ")
      ),
      start_idx = idx_func[["start"]] - stringr::str_length(prefunc_arg),
      end_idx = idx_func[["end"]],
      add_var = add_var
    ))
  }

  if (idx_func[["syntax"]] == "syntax3") {
    match_idx <- if (length(translated_func) == 0) {
      1
    } else {
      length(translated_func[translated_func == idx_func[["insightmaker"]]]) + 1
    }
    match_idx <- ifelse(match_idx == 1, "", match_idx)

    envir <- environment()
    call_args <- eval(parse(text = idx_func[["R"]])) |>
      formals() |>
      as.list() |>
      utils::modifyList(list(
        func = tolower(idx_func[["insightmaker"]]),
        arg = arg
      ))

    call_args <- lapply(seq_along(call_args), function(y) {
      if (is.name(call_args[[y]])) {
        envir[[names(call_args)[y]]]
      } else {
        call_args[[y]]
      }
    }) |>
      stats::setNames(names(call_args))

    out <- do.call(idx_func[["R"]], call_args)

    return(list(
      replacement = out[["replacement"]],
      start_idx = idx_func[["start"]],
      end_idx = idx_func[["end"]],
      add_var = out[["add_var"]]
    ))
  }

  cli::cli_abort(c(
    "Unsupported Insight Maker syntax class: {.val {idx_func[['syntax']]}}."
  ))
}


#' Replace comment characters from Insight Maker to R
#'
#' @inheritParams convert_equations_IM
#'
#' @returns Updated eqn
#' @noRd
#'
replace_comments <- function(eqn) {
  comment_char <- c("//", "/*", "*/")
  replacements <- c("#", "#", "\n")

  done <- FALSE

  while (!done) {
    idxs_comments_ <- stringr::str_locate_all(eqn, stringr::fixed(comment_char))
    idxs_comments <- as.data.frame(do.call(rbind, idxs_comments_))

    if (nrow(idxs_comments) == 0) {
      done <- TRUE
      next
    }

    idxs_comments[["char"]] <- rep(comment_char, vapply(idxs_comments_, nrow, numeric(1)))

    # Remove those that are in comments (#) or quotation marks
    idxs_exclude <- get_seq_exclude(eqn, type = "quot", names_with_brackets = TRUE)
    idxs_comments <- idxs_comments[!(idxs_comments[["start"]] %in% idxs_exclude | idxs_comments[["end"]] %in% idxs_exclude), ]

    if (nrow(idxs_comments) == 0) {
      done <- TRUE
    } else {
      idx_comments <- idxs_comments[1, ]
      replacement <- replacements[match(idx_comments[["char"]], comment_char)]
      stringr::str_sub(eqn, idx_comments[["start"]], idx_comments[["end"]]) <- replacement
    }
  }
  return(eqn)
}


#' Clean equation and extract comments
#'
#' @inheritParams convert_equations_IM
#'
#' @returns List with cleaned eqn and extracted comments
#' @noRd
#'
remove_comments <- function(eqn) {
  if (grepl("#", eqn)) {
    # Find indices of comments
    comment_df <- get_range_comments(eqn)

    # Get indices of comments
    seq_idxs_range <- unlist(mapply(seq, comment_df[, "start"],
      comment_df[, "end"],
      SIMPLIFY = FALSE
    ))

    split_formula <- strsplit(eqn, "")[[1]]

    # Get comments as documentation and remove comments from eqn
    doc <- trimws(paste0(split_formula[seq_idxs_range], collapse = ""))
    eqn <- trimws(paste0(split_formula[-seq_idxs_range], collapse = ""))

    if (!nzchar(eqn)) {
      eqn <- "0"
    }
  } else {
    doc <- ""
  }
  return(list(
    eqn = eqn,
    doc = doc
  ))
}


#' Translate Insight Maker colon operator to R
#'
#' @inheritParams convert_equations_IM
#' @returns Updated eqn
#' @noRd
#'
replace_colon <- function(eqn, var_names) {
  # Replace range including by argument
  eqn <- stringr::str_replace_all(
    eqn,
    "([0-9.-]+):([0-9.-]+):([0-9.-]+)",
    "seq\\(\\1, \\3, by = \\2\\)"
  )

  # Get indices of all colons
  idxs_colon <- stringr::str_locate_all(eqn, ":")[[1]][, "start"]

  # Remove those that are in quotation marks or names
  idxs_exclude <- get_seq_exclude(eqn, var_names, names_with_brackets = TRUE)
  idxs_colon <- setdiff(idxs_colon, idxs_exclude)

  # Don't keep those that are bordered by numbers
  idxs_range <- stringr::str_locate_all(eqn, "([0-9.-]+):([0-9.-]+)")[[1]]
  seq_idxs_range <- unlist(mapply(seq, idxs_range[, "start"], idxs_range[, "end"], SIMPLIFY = FALSE))

  # Only keep those between vector brackets
  paired_idxs <- get_range_all_pairs(eqn, var_names, type = "vector", names_with_brackets = TRUE)
  seq_paired_idxs <- unlist(mapply(seq, paired_idxs[["start"]], paired_idxs[["end"]], SIMPLIFY = FALSE))

  # Replace colons that are not used to define a range with "="
  replace_idxs <- idxs_colon[(idxs_colon %in% seq_paired_idxs) & !(idxs_colon %in% seq_idxs_range)]

  for (i in replace_idxs) {
    stringr::str_sub(eqn, i, i) <- "="
  }

  return(eqn)
}


#' Translate curly bracket syntax from Insight Maker to R
#'
#' @inheritParams convert_equations_IM
#' @returns Updated eqn
#' @noRd
#'
curly_to_vector_brackets <- function(eqn, var_names) {
  # Curly brackets can be:
  # - Indexers, e.g., "b{1}.length()"
  # if it is adjacent to [a-zA-Z0-9] or {}, it's indexing
  # - Vectors
  # - Nested lists

  done <- FALSE
  max_iter <- 1000
  i <- 1
  while (!done && i < max_iter) {
    # Get indices of all enclosures
    paired_idxs <- get_range_all_pairs(eqn, var_names,
      add_custom = "list()",
      names_with_brackets = TRUE
    )

    # Check nesting
    if (nrow(paired_idxs) > 0) {
      # Create data.frame with properties per pair
      paired_idxs_prop <- paired_idxs
      paired_idxs_prop["id"] <- seq_len(nrow(paired_idxs_prop))

      # Find which pairs this pair is nested within; this will be in order of highest to lowest level
      paired_idxs_prop[["nested_within"]] <- which(
        paired_idxs[["start"]] < paired_idxs_prop[["start"]] & paired_idxs[["end"]] > paired_idxs_prop[["end"]]
      ) |> paste0(collapse = ",")
      paired_idxs_prop[["nr_nesting_levels"]] <- length(strsplit(paired_idxs_prop[["nested_within"]], ",")[[1]])
      paired_idxs_prop[["nested_around"]] <- which(
        paired_idxs[["start"]] > paired_idxs_prop[["start"]] & paired_idxs[["end"]] < paired_idxs_prop[["end"]]
      ) |> paste0(collapse = ",")
      # First character to the left
      paired_idxs_prop[["left_adjacent_char"]] <- stringr::str_sub(
        eqn,
        paired_idxs_prop[["start"]] - 1,
        paired_idxs_prop[["start"]] - 1
      )
      # Preceding string
      paired_idxs_prop[["preceding_str"]] <- stringr::str_extract(
        stringr::str_sub(eqn, 1, paired_idxs_prop[["start"]] - 1),
        "\\b[\\w\\.\\\\]+$" # All word characters, periods . and slashes \\
      )
      paired_idxs_prop[["is_start_of_string"]] <- paired_idxs_prop[["start"]] == 1

      if (any(paired_idxs_prop[["type"]] == "curly")) {
        # Start with most nested string
        chosen_pair <- paired_idxs_prop[paired_idxs_prop[["type"]] == "curly", ]
        chosen_pair <- chosen_pair[order(chosen_pair[["start"]], decreasing = TRUE), ][1, ]

        # Find type of enclosure of the lowest order bracket it is nested within
        if (nzchar(chosen_pair[["nested_within"]])) {
          num <- strsplit(chosen_pair[["nested_within"]], ",")[[1]]
          num <- as.numeric(num[length(num)])
          nested_within <- paired_idxs_prop[paired_idxs_prop[["id"]] == num, "type"]
        } else {
          nested_within <- ""
        }

        # Find type of enclosure of the highest order bracket it is nested around
        if (nzchar(chosen_pair[["nested_around"]])) {
          num <- as.numeric(strsplit(
            chosen_pair[["nested_around"]], ","
          )[[1]][[1]])
          nested_around <- paired_idxs_prop[paired_idxs_prop[["id"]] == num, "type"]
        } else {
          nested_around <- ""
        }

        # It is a vector if it is not nested around anything
        x_is_vector <- (!nzchar(nested_around) |
          (nested_around %in% c("quot", "round"))) &
          (!nzchar(nested_within) |
            (
              nested_within %in% c("curly", "round", "vector", "square")
            ))
        # It is a list if...
        x_is_list <- nested_around %in% c("curly", "list", "vector")
        # It is an indexer if...
        x_is_indexer <- stringr::str_detect(
          chosen_pair[["left_adjacent_char"]],
          "[a-zA-Z0-9._\\}\\)]"
        ) &
          (!chosen_pair[["is_start_of_string"]]) &
          !(grepl("\\n", chosen_pair[["preceding_str"]], fixed = TRUE))

        start_idx <- chosen_pair[["start"]]
        end_idx <- chosen_pair[["end"]]
        string <- stringr::str_sub(eqn, start_idx + 1, end_idx - 1)

        if (x_is_indexer) {
          replacement <- paste0("[", string, "]")
        } else if (x_is_vector) {
          replacement <- paste0("c(", string, ")")
        } else if (x_is_list) {
          replacement <- paste0("list(", string, ")")
        } else {
          replacement <- stringr::str_sub(eqn, start_idx, end_idx) # No replacement
        }

        # Replace in string
        stringr::str_sub(eqn, start_idx, end_idx) <- replacement
        i <- i + 1
      } else {
        done <- TRUE
      }
    } else {
      done <- TRUE
    }
  }

  return(eqn)
}


#' Translate Insight Maker operators to R
#'
#' @inheritParams convert_equations_IM
#' @returns Updated eqn
#' @importFrom rlang .data
#' @noRd
#'
replace_op_IM <- function(eqn, var_names) {
  # Insight Maker uses "and", "AND", "or", "OR", "not", "NOT"
  logical_op <- c(
    # Insight Maker is case-insensitive
    "and" = "&",
    "or" = "|",
    "not" = "!",
    "true" = "TRUE",
    "false" = "FALSE",
    # mod in Insight Maker is actually the remainder operator!
    "mod" = "%REM%",
    # Euler
    "e" = "exp(1)"
  )
  # Operator words cannot be preceded or followed by a letter
  names(logical_op) <- paste0("(?:^|(?<=\\W))", names(logical_op), "(?=(?:\\W|$))")
  logical_op <- c(logical_op, c("(?<!=|<|>)=(?!=)" = "==", "<>" = "!="))

  # For "=", exclude matches inside function(...) argument lists: there "=" is a
  # default-assignment operator and must stay as is.
  exclude_function_arg_equals <- function(eqn, var_names) {
    paired_idxs <- get_range_all_pairs(eqn, var_names,
      type = "round",
      names_with_brackets = TRUE
    )
    end_function_words <- get_words(eqn)
    end_function_words <- end_function_words[end_function_words[["word"]] == "function", "end"]

    if (nrow(paired_idxs) > 0 && length(end_function_words) > 0) {
      function_brackets <- paired_idxs[paired_idxs[["start"]] %in% (end_function_words + 1), ]
      return(unlist(mapply(seq, function_brackets[["start"]], function_brackets[["end"]], SIMPLIFY = FALSE)))
    }
    integer(0)
  }

  eqn <- apply_operator_replacements(eqn, var_names, logical_op,
    names_with_brackets = TRUE,
    ignore_case = TRUE,
    extra_exclude = exclude_function_arg_equals
  )

  # Translate assignment operator <- to =. This can't be done above as then = would be translated to ==. Insight Maker does not use = as an assignment operator.
  df_logical_op <- as.data.frame(stringr::str_locate_all(eqn, "<-")[[1]])

  if (nrow(df_logical_op) > 0) {
    # Remove those that are in quotation marks or names
    idxs_exclude <- get_seq_exclude(eqn, var_names, names_with_brackets = TRUE)
    df_logical_op <- df_logical_op[!(df_logical_op[["start"]] %in% idxs_exclude | df_logical_op[["end"]] %in% idxs_exclude), ]

    if (nrow(df_logical_op) > 0) {
      # Replace in reverse order; no nested functions, so we can replace them in one go
      eqn <- apply_replacements_reversed(eqn, df_logical_op, "=")
    }
  }

  return(eqn)
}


#' Convert statement syntax from Insight Maker to R
#'
#' @param line String with line of code
#' @inheritParams convert_equations_IM
#'
#' @returns Updated line
#' @noRd
#'
convert_statement <- function(line, var_names) {
  equation <- line

  # Count amount of whitespace at beginning of string
  leading_whitespace <- stringr::str_extract(equation, "^[ ]*") |>
    nchar() |>
    stringr::str_dup(" ", times = _)
  equation <- trimws(equation)

  # Check for final comma - anything appended needs to come before the comma
  final_char <- stringr::str_extract(trimws(equation), ".$")
  final_char <- ifelse(!is.na(final_char), final_char, "")

  if (final_char == ",") {
    equation <- stringr::str_replace(equation, ",[ ]*$", "") # Remove comma from equation
    comma <- ","
  } else {
    comma <- ""
  }

  # Remove then (might not only be in if and else if, but also in a separate line)
  equation <- stringr::str_replace(
    equation,
    stringr::regex("(?:^|(?<=\\W))then(?=(?:\\W|$))", ignore_case = TRUE), ""
  )

  # To find statements (e.g., for, if), extract first and second word
  words <- get_words(equation)
  first_word_orig <- ifelse(nrow(words) > 0, words[1, "word"], "")
  second_word_orig <- ifelse(nrow(words) > 1, words[2, "word"], "")
  first_word <- tolower(first_word_orig)
  second_word <- tolower(second_word_orig)

  # Change equation
  statement <- ""
  closing_statement <- ""

  if (first_word == "function") {
    # A function declaration starts with function without brackets, e.g., Function Square(x).
    # In R, the variable name needs to come BEFORE function, so switch statement and first word of equation
    statement <- second_word_orig

    # When there is no second word, i.e., function(), don't change the equation
    if (nzchar(second_word_orig)) {
      equation <- stringr::str_replace(equation, sprintf("%s ", first_word_orig), "") |>
        stringr::str_replace(second_word_orig, " <- function") # Remove statement # Add equals sign
    }
  } else if ((first_word == "end" && second_word %in% c("loop", "if", "function"))) {
    statement <- "}"
    equation <- stringr::str_replace(equation, sprintf("^%s %s", first_word_orig, second_word_orig), "") # Remove statement
  } else if (first_word %in% c("for", "while", "if", "return", "throw") || (first_word == "else" && second_word == "if")) {
    statement <- first_word
    equation <- stringr::str_replace(equation, sprintf("^[ ]*%s", first_word_orig), "") # Remove statement from equation

    if (first_word %in% c("while", "if", "for") || (first_word == "else" && second_word == "if")) {
      closing_statement <- "{"
    }

    # Add "}" to else if
    if (first_word == "else" && second_word == "if") {
      statement <- sprintf("} %s %s", first_word, second_word)
      equation <- stringr::str_replace(equation, sprintf("^[ ]*%s", second_word_orig), "")
      # Remove statement from equation
    }

    # Add round brackets around equation
    equation <- trimws(equation)
    equation <- ifelse(stringr::str_starts(equation, "\\(") &
      stringr::str_ends(equation, "\\)"), equation,
    paste0("(", equation, ")")
    )
    equation <- ifelse(!(first_word %in% c("return", "throw")),
      paste0(" ", equation), equation
    )

    # Replace ranges in for-loop
    if (first_word == "for") {
      equation <- stringr::str_replace(
        equation, stringr::regex("from (.*?) to (.*?) by (.*?)\\)",
          ignore_case = TRUE
        ),
        "in seq(\\1, \\2, by = \\3))"
      ) |>
        stringr::str_replace(
          stringr::regex("from (.*?) to (.*?)\\)", ignore_case = TRUE),
          "in seq(\\1, \\2))"
        )
    }
  } else if (first_word == "else") {
    statement <- paste0("} ", first_word, " {")
    # Remove statement from equation
    equation <- stringr::str_replace(equation, sprintf("^%s", first_word_orig), "")

    # Try-statement
  } else if (first_word == "try") {
    statement <- "tryCatch({"
    equation <- stringr::str_replace(equation, sprintf("^%s", first_word_orig), "") # Remove statement from equation
  } else if (first_word == "catch") {
    statement <- "}, error = function("
    closing_statement <- "){"
    # Remove statement from equation
    equation <- stringr::str_replace(equation, sprintf("^%s", first_word_orig), "")
  } else if (first_word == "end" && second_word == "try") {
    statement <- "})"
    # Remove statement from equation
    equation <- stringr::str_replace(
      equation,
      sprintf(
        "^%s %s", first_word_orig,
        second_word_orig
      ), ""
    )
  }


  # Left-over: Make sure all functions are in the right lower case; don't do this with simple replacement as "function" might be part of a variable name
  words <- get_words(equation)
  words_function <- words
  words_function <- words_function[tolower(words_function[["word"]]) == "function", ]

  if (nrow(words_function) > 0) {
    for (i in seq_len(nrow(words_function))) {
      stringr::str_sub(equation, words_function[i, "start"], words_function[i, "end"]) <- "function"
    }
  }

  # Make sure all functions have a closing statement "{"
  paired_idxs <- get_range_all_pairs(equation, var_names,
    type = "round",
    names_with_brackets = TRUE
  ) # Extract all round brackets
  if (nrow(paired_idxs) > 0 && nrow(words) > 0) {
    # Pick bracket that ends the string
    start_bracket <- paired_idxs
    start_bracket <- start_bracket[start_bracket[["end"]] == stringr::str_length(equation), "start"]
    # Get last word before vector brackets
    last_word <- words
    last_word <- last_word[(last_word[["end"]] + 1) == ifelse(length(start_bracket), start_bracket, 0), "word"]
    last_word <- ifelse(length(last_word) > 0, last_word, "")
    if (last_word == "function") {
      closing_statement <- "{"
    }
  }

  join_str <- c(
    leading_whitespace = leading_whitespace,
    statement = statement,
    equation = equation,
    closing_statement = closing_statement,
    comma = comma
  ) |> stringr::str_c(collapse = "")

  if (P[["debug"]]) {
    cli::cli_inform(c("i" = "Converting statements:"))
    cli::cli_inform(c("i" = line))
    cli::cli_inform(c("i" = join_str))
    cli::cli_inform(c(" " = ""))
  }
  return(join_str)
}


#' Convert all statement syntax from Insight Maker to R
#'
#' Wrapper around convert_statement()
#'
#' @inheritParams convert_equations_IM
#'
#' @returns Updated eqn
#' @noRd
#'
convert_all_statements <- function(eqn, var_names) {
  # Convert inline functions
  done <- FALSE

  while (!done) {
    # In-line function assignment without using the word function
    idxs_inline <- stringr::str_locate_all(
      eqn,
      stringr::regex(
        "^(.*?)\\)[ ]*[^%]<-",
        dotall = FALSE,
        multiline = TRUE
      )
    )[[1]]
    idxs_inline

    if (nrow(idxs_inline) == 0) {
      done <- TRUE
    } else {
      idx_inline <- idxs_inline[1, ]
      # Change to one-liner function
      stringr::str_sub(eqn, idx_inline["start"], idx_inline["end"]) <-
        stringr::str_sub(eqn, idx_inline["start"], idx_inline["end"]) |>
        stringr::str_replace("[ ]*<-[ ]*", " ") |>
        stringr::str_replace("\\(", " <- function(") # Replace first opening bracket; important to change it to <- and not =, as otherwise = will be replaced by ==
    }
  }


  # Insight Maker doesn't require users to specify an Else-statement in If ... Else If ... End If -> if no condition evaluates to TRUE, the output is zero. Add "Else\n0\n" in these lines.
  # Find all if end if
  formula_split <- stringr::str_split(eqn, "\n")[[1]]
  idx_end_if <- which(stringr::str_detect(
    formula_split,
    stringr::regex("(?:^|(?<=\\W))end if(?=(?:\\W|$))", ignore_case = TRUE)
  ))

  # For each end if, check whether first preceding line with "Then" or "Else" or "If"
  for (i in idx_end_if) {
    last_idx <- vapply(
      c("else", "else if", "if"),
      function(x) {
        idx <- which(stringr::str_detect(
          formula_split[seq_len(i - 1)],
          stringr::regex(sprintf("(?:^|(?<=\\W))%s(?=(?:\\W|$))", x),
            ignore_case = TRUE
          )
        ))
        if (length(idx) > 0) {
          return(max(idx))
        } else {
          return(0)
        }
      }, numeric(1)
    )
    last_idx

    # Add else-statement
    if (last_idx[["else if"]] == max(last_idx) || last_idx[["if"]] == max(last_idx)) {
      # If the last found statement is else if or if, add "end if"
      formula_split[i] <- paste("\nelse\n0\n", formula_split[i])
    }
  }
  eqn <- stringr::str_c(formula_split, collapse = "\n")

  formula_new <- unlist(lapply(
    stringr::str_split(eqn, "\n")[[1]],
    convert_statement, var_names
  )) |>
    stringr::str_c(collapse = "\n")
  return(formula_new)
}


#' Convert Insight Maker built-in functions to R
#'
#' @returns List with transformed eqn and list with additional R code needed to make the eqn function
#' @inheritParams convert_equations_IM
#' @importFrom rlang .data
#' @noRd
#'
convert_builtin_functions_IM <- function(type, name, eqn, var_names) {
  # If there are no letters in the eqn, don't run function
  translated_func <- c()
  add_vars <- data.frame() # Will accumulate all add_vars

  if (grepl("[[:alpha:]]", eqn)) {
    # data.frame with regular expressions for each built-in Insight Maker function
    syntax_df <- syntax_IM[["syntax_df"]]
    syntax_df_unsupp <- syntax_IM[["syntax_df_unsupp"]] # Unsupported functions

    done <- FALSE
    i <- 1
    ignore_case_arg <- TRUE
    IM_regex <- stringr::regex(syntax_df[["insightmaker_regex_first_iter"]],
      ignore_case = ignore_case_arg
    )

    while (!done) {
      matches <- collect_IM_function_matches(eqn, syntax_df, IM_regex, var_names)
      idx_df <- matches[["idx_df"]]
      idx_keep <- matches[["idx_keep"]]

      if (nrow(idx_df) == 0 && i > 1) {
        done <- TRUE
        next
      }

      if (nrow(idx_df) == 0) {
        done <- TRUE
        next
      }

      # For the first iteration, add _replace to all detected functions, so we don't end in an infinite loop (some insightmaker and R functions have the same name)
      if (i == 1 && nrow(idx_df) > 0) {
        eqn <- mark_IM_function_matches(eqn, idx_df)
      }

      if (i == 1) {
        ignore_case_arg <- FALSE

        # Switch from insightmaker_regex_first_iter to insightmaker_regex
        # Also only keep those functions that were detected on the first iteration.
        # No new functions to be translated will be added.
        syntax_df <- syntax_df[idx_keep, , drop = FALSE]
        IM_regex <- stringr::regex(syntax_df[["insightmaker_regex"]],
          ignore_case = ignore_case_arg
        )
        i <- i + 1
        # Stop first iteration
        next
      }

      if (nrow(idx_df) == 0) {
        done <- TRUE
      } else {
        # Pair functions to brackets and pick the most nested one
        # (syntax0b/syntax1b need no brackets)
        idx_func <- select_innermost_function(eqn, idx_df, var_names,
          bracketless_syntaxes = c("syntax0b", "syntax1b"),
          pair_args = list(names_with_brackets = TRUE)
        )

        # Remove _replace in replacement function
        idx_func[["insightmaker"]] <- stringr::str_replace(idx_func[["insightmaker"]], "_replace$", "")

        # Extract argument between brackets (excluding brackets)
        bracket_arg <- stringr::str_sub(eqn, idx_func[["start_bracket"]] + 1, idx_func[["end"]] - 1)
        arg <- parse_args(bracket_arg)

        replacement_info <- build_IM_builtin_replacement(
          eqn = eqn,
          idx_func = idx_func,
          arg = arg,
          name = name,
          var_names = var_names,
          translated_func = translated_func
        )
        replacement <- replacement_info[["replacement"]]
        start_idx <- replacement_info[["start_idx"]]
        end_idx <- replacement_info[["end_idx"]]
        add_var <- replacement_info[["add_var"]]

        if (nrow(add_var)) {
          add_vars <- rbind(add_vars, add_var)

          # Add newly created variables to names_df so that they are safe from replacement, e.g., if a variable contains the word "Time"
          var_names <- c(var_names, add_var[["name"]])
        }

        if (P[["debug"]]) {
          cli::cli_inform(c("i" = stringr::str_sub(eqn, start_idx, end_idx)))
          cli::cli_inform(c("i" = replacement))
          cli::cli_inform(c(" " = ""))
        }

        # Replace eqn
        stringr::str_sub(eqn, start_idx, end_idx) <- replacement

        translated_func <- c(translated_func, idx_func[["insightmaker"]])
      }
    }

    report_unsupported_IM_functions(eqn, name, var_names, syntax_df_unsupp)
  }

  list(
    eqn = eqn,
    translated_func = translated_func,
    add_vars = add_vars,
    doc = ""
  )
}


#' Extract arguments before function (object-oriented syntax)
#'
#' @inheritParams convert_equations_IM
#' @inheritParams get_range_names
#' @param start_func Index of start of function
#'
#' @returns String with arguments before function
#' @noRd
#'
extract_prefunc_args <- function(eqn, var_names, start_func, names_with_brackets) {
  # Get all enclosing elements before start of function
  prefunc_brackets <- get_range_all_pairs(eqn, var_names,
    # add_custom = "paste0()",
    names_with_brackets = names_with_brackets
  )

  prefunc_brackets <- prefunc_brackets[
    prefunc_brackets[["type"]] != "square",
    prefunc_brackets[["end"]] == (start_func - 1),
  ]

  if (nrow(prefunc_brackets) > 0) {
    # Second argument is whatever is between brackets
    prefunc_arg <- stringr::str_sub(
      eqn,
      prefunc_brackets[["start"]],
      # Keep c()
      prefunc_brackets[["end"]]
    )
  } else {
    # If there are no brackets around the argument preceding . (e.g., .length()), extract string before
    idx_prefunc_arg <- stringr::str_locate(
      stringr::str_sub(eqn, 1, start_func - 1),
      "[\\w\\.\\[\\]]+$" # Don't match square brackets as we don't only want to extract indexers
    )

    prefunc_arg <- stringr::str_sub(
      stringr::str_sub(eqn, 1, start_func - 1),
      idx_prefunc_arg[1, "start"], idx_prefunc_arg[1, "end"]
    )
  }

  return(prefunc_arg)
}


#' Convert Insight Maker's Lookup() to R
#'
#' Lookup() is a linear interpolation function, equivalent to R's approx().
#'
#' @inheritParams convert_equations_IM
#'
#' @returns Transformed eqn
#' @noRd
#'
conv_lookup <- function(func, arg, name) {
  func_name_str <- sprintf("%s_lookup", name)
  arg[1] <- stringr::str_replace_all(arg[1], c("^\\[" = "", "\\]$" = ""))
  add_var <- do.call(
    get_variable_row,
    list(
      name = func_name_str, type = "lookup",
      xpts = arg[2], ypts = arg[3],
      source = arg[1],
      interpolation = "linear",
      extrapolation = "nearest"
    )
  )

  replacement <- sprintf("[%s]([%s])", func_name_str, arg[1])

  return(list(
    replacement = replacement,
    add_var = add_var
  ))
}


#' Check whether an eqn consists of only a primitive between brackets
#'
#' @inheritParams convert_equations_IM
#' @returns Logical value
#' @noRd
#'
check_only_primitive <- function(eqn) {
  # A eqn only contains a primitive when there is one pair of square brackets and they are located at the beginning and end
  opening <- stringr::str_locate_all(eqn, "\\[")[[1]][, 1]
  closing <- stringr::str_locate_all(eqn, "\\]")[[1]][, 1]

  return(
    length(opening) == 1 &
      length(closing) == 1 &
      opening[1] == 1 & closing[1] == stringr::str_length(eqn)
  )
}


#' Build the replacement and auxiliary variable for a time-based graphical function
#'
#' Shared tail of conv_step(), conv_pulse(), and conv_ramp(). Each names a
#' new auxiliary variable after the function type and occurrence index, replaces the
#' call with a lookup into that auxiliary at the current time, and registers the
#' auxiliary's definition.
#'
#' @param name Model element the function belongs to.
#' @param func Function type ("step", "pulse", "ramp").
#' @param match_idx Occurrence index of this function within the equation.
#' @param func_def_str Equation defining the auxiliary variable.
#'
#' @returns List with `replacement` and `add_var`.
#' @noRd
#'
make_aux_graphical_function <- function(name, func, match_idx, func_def_str) {
  func_name_str <- sprintf("%s_%s%s", name, func, as.character(match_idx))
  list(
    replacement = sprintf("[%s](%s)", func_name_str, P[["time_name"]]),
    add_var = get_variable_row(name = func_name_str, type = "aux", eqn = func_def_str)
  )
}


#' Convert Insight Maker's Step() function to R
#'
#' @param h_step Height of step, defaults to 1
#' @param match_idx Index of the number of times the same function has been called in the same eqn
#'
#' @returns List with transformed eqn and list with additional R code needed to make the eqn function
#' @noRd
#' @inheritParams convert_equations_IM
#'
conv_step <- function(func, arg, match_idx, name, # Default settings of Insight Maker
                      h_step = "1") {
  # Step(Start, Height=1), e.g., Step({2 Years}, 100)

  # Clean start time by converting to simulation time units
  start_t_step <- arg[1]

  # Define interpolation function
  h_step <- ifelse(is.na(arg[2]), h_step, arg[2])

  # Function definition to put at beginning of script
  func_def_str <- sprintf(
    "step(%s, start = %s, height = %s)",
    P[["times_name"]],
    start_t_step,
    h_step
  )

  make_aux_graphical_function(name, func, match_idx, func_def_str)
}


#' Convert Insight Maker's Pulse() function to R
#'
#' @param h_pulse Height of pulse, defaults to 1
#' @param w_pulse Width of pulse in duration ( time), defaults to 0 to indicate an instantaneous pulse
#' @param repeat_interval Interval at which to repeat pulse, defaults to "NULL" to indicate no repetition
#'
#' @returns List with transformed eqn and list with additional R code needed to make the eqn function
#' @noRd
#' @inheritParams convert_equations_IM
#' @inheritParams conv_step
#'
conv_pulse <- function(func,
                       arg,
                       match_idx,
                       name,
                       # Default settings of Insight Maker
                       h_pulse = "1",
                       w_pulse = "0",
                       repeat_interval = "NULL") {
  # Pulse(Time, Height, Width=0, Repeat=-1), e.g., Pulse({5 Years}, 10, 1, {10 Years})

  # Start time
  start_t_pulse <- arg[1]

  # Define interpolation function
  h_pulse <- ifelse(is.na(arg[2]), h_pulse, arg[2])
  w_pulse <- ifelse(is.na(arg[3]), w_pulse, arg[3])

  if (w_pulse == "0") {
    w_pulse <- P[["timestep_name"]]
  }
  repeat_interval <- ifelse(is.na(arg[4]), repeat_interval, arg[4])

  # Function definition to put at beginning of script
  func_def_str <- sprintf(
    "pulse(%s, start = %s, height = %s, width = %s, repeat_interval = %s)",
    P[["times_name"]],
    start_t_pulse,
    h_pulse,
    w_pulse,
    repeat_interval
  )

  make_aux_graphical_function(name, func, match_idx, func_def_str)
}


#' Convert Insight Maker's Ramp() function to R
#'
#' @param h_ramp End height of ramp, defaults to 1
#'
#' @returns List with transformed eqn and list with additional R code needed to make the eqn function
#' @noRd
#' @inheritParams convert_equations_IM
#' @inheritParams conv_step
#'
conv_ramp <- function(func, arg, match_idx, name, # Default settings of Insight Maker
                      h_ramp = "1") {
  # Ramp(Start, Finish, Height=1), e.g., Ramp({3 Years}, {8 Years}, -50)

  # Start and end time
  start_t_ramp <- arg[1]
  end_t_ramp <- arg[2]

  # Define interpolation function
  h_ramp <- ifelse(is.na(arg[3]), h_ramp, arg[3])

  # Function definition to put at beginning of script
  func_def_str <- sprintf(
    "ramp(%s, start = %s, finish = %s, height = %s)",
    P[["times_name"]],
    start_t_ramp,
    end_t_ramp,
    h_ramp
  )

  make_aux_graphical_function(name, func, match_idx, func_def_str)
}
