#' Script templates for code generation
#'
#' Templates use `%(key)s` placeholders that get substituted via fmt().
#'
#' @noRd
script_template <- function() {
  list(
    # -- compile_times ---------------------------------------------------------
    times_r = "
# Define time sequence
%(timestep_name)s = %(dt)s
%(times_name)s <- seq(from=%(start)s, to=%(stop)s, by=%(timestep_name)s)
%(time_name)s = %(times_name)s[1]

# Simulation time unit (smallest time scale in your model)
%(time_units_name)s = '%(time_units)s'
",
    times_julia = "\n\n# Simulation time unit (smallest time scale in your model)\n%(time_units_name)s = u\"%(time_units)s\"\n# Define time sequence\n%(times_name)s = (%(start)s, %(stop)s)%(times_unit_mult)s\n# Initialize time (only necessary if constants use t)\n%(time_name)s = %(times_name)s[1]\n# Time step\n%(timestep_name)s = %(dt)s%(dt_unit_mult)s\n# Define saving time sequence\n%(saveat_name)s = %(saveat_expr)s\n%(tstops_name)s = %(times_name)s[1]:%(timestep_name)s:%(times_name)s[2]\n",

    # -- compile_nonneg_stocks -------------------------------------------------

    nonneg_stocks_r = "
# Ensure non-negativity of (selected) stocks
%(nonneg_stock_name)s = which(names(%(initial_value_name)s) %in% c(%(nonneg_stock_names_quoted)s))

# Define root function to be triggered when non-negative stocks go below 0
%(rootfun_name)s <- function (%(time_name)s, %(state_name)s, %(parameter_name)s) {
  return(ifelse(any(%(state_name)s[%(nonneg_stock_name)s] < 0), 0, 1))
}

# Set non-negative stocks to zero when root function is triggered
%(eventfun_name)s <- function(%(time_name)s, %(state_name)s, %(parameter_name)s) {
  %(state_name)s[%(state_name)s[%(nonneg_stock_name)s] < 0] = 0
  return(%(state_name)s)
}
",
    nonneg_root_arg_r = ",\n\t\t\t\tevents = list(func = %(eventfun_name)s, root = TRUE), rootfun = %(rootfun_name)s",
    nonneg_check_root_r = "
# Times at which non-negative stocks fell below 0
attributes(%(sim_df_name)s)$troot

# Values of non-negative stocks when root function was triggered
attributes(%(sim_df_name)s)$valroot
",

    # -- compile_prep_script ---------------------------------------------------

    prep_seed_r = "# Ensure reproducibility across runs in case of random elements\nset.seed(%(seed)s)",
    prep_seed_julia = "# Ensure reproducibility across runs in case of random elements\nRandom.seed!(%(seed)s)\n",

    # -- compile_funcs ----------------------------------------------------------

    funcs_r = "\n\n# User-specified funcs\n%(func_body)s\n",
    funcs_julia = "\n\n# User-specified funcs\n%(func_body)s\n\n",

    # -- compile_units ----------------------------------------------------------

    units_julia = "\n# Define custom units; register after each unit as some units may be defined by other units\nold_logger = global_logger(NullLogger())\nmodule %(MyCustomUnits)s\n\tusing Unitful\n\tusing %(jl_pkg_name)s.%(sdbuildR_units)s\n\t%(unit_str)s\n\tUnitful.register(%(MyCustomUnits)s)\nend\n\nUnitful.register(%(MyCustomUnits)s)\nglobal_logger(old_logger)\n",

    # -- compile_static ----------------------------------------------------

    static_r = "\n\n# Define parameters, initial conditions, and functions in correct order\n%(static_str)s%(constants_def)s%(init_def)s",
    static_julia = "\n\n# Define parameters, initial conditions, and functions in correct order\n%(model_setup_name)s = let\n%(ensemble_iter_code)s%(intermediary_names_str)s%(static_str)s%(pars_def)s%(init_def)s%(init_names_str)s%(init_idx)s%(intermediary_names_correct)s\n\t(%(parameter_name)s = %(parameter_name)s, %(initial_value_name)s = %(initial_value_name)s, %(initial_value_names)s = %(initial_value_names)s, %(intermediary_names)s = %(intermediary_names)s%(delay_idx_return)s)\nend\n",

    # -- compile_ode -----------------------------------------------------------

    ode_r = "\n\n# Define ODE\n%(ode_func_name)s = function(%(time_name)s, %(state_name)s, %(parameter_name)s){\n\n  %(S_str)s\n\n# Compute change in stocks at current time %(time_name)s\n  with(c(%(state_name)s, %(parameter_name)s), {\n\n    # Update auxiliaries and flows\n    %(dynamic_eqn_str)s\n\n    # Collect inflows and outflows for each stock\n    %(stock_change_str)s\n\n    # Combine change in stocks\n    %(state_change_str)s\n\n    return(list(%(change_state_name)s%(save_var_str)s))\n  })\n}",
    ode_julia = "\n\n# Define ODE\nfunction %(ode_func_name)s!(%(change_state_name)s, %(state_name)s, %(parameter_name)s, %(time_name)s)\n\n\t# Unpack state variables\n\t%(unpack_state_str)s%(add_stock_units)s%(unpack_pars_str)s\n\n\t# Update auxiliaries\n\t%(dynamic_eqn_str)s\n\n\t# Collect inflows and outflows for each stock\n\t%(stock_change_str)s\n\tnothing\nend\n",
    callback_julia = "\n\n# Define callback function\nfunction %(callback_func_name)s(%(state_name)s, %(time_name)s, integrator)\n\n\t# Unpack state variables\n\t%(unpack_state_str)s%(add_stock_units)s%(unpack_pars_integrator_str)s\n\n\t# Update auxiliaries\n\t%(dynamic_eqn_str)s\n\n\t# Return intermediary values and remove functions\n\treturn filter(x -> !is_function_or_interp(x), (%(intermediary_values)s))\n\n\nend\n\n# Callback setup\n%(callback_setup)s",
    callback_empty_julia = "\n\n# Define empty callback function\n%(intermediaries)s = nothing\n%(callback_name)s = nothing\n\n",

    # -- compile_run_ode -------------------------------------------------------

    run_ode_r = "\n\n# Run ODE\n%(sim_df_name)s = as.data.frame(deSolve::ode(\n  func=%(ode_func_name)s,\n  y=%(initial_value_name)s,\n  times=%(times_name)s,\n  parms=%(parameter_name)s,\n  method = '%(method)s'%(root_arg)s\n)) %(check_root)s\n",
    post_ode_r = "%(saveat_script)s# Wide to long\n    %(sim_df_name)s <- stats::reshape(\n       data = as.data.frame(%(sim_df_name)s),\n       direction = \"long\",\n       idvar = \"time\",\n       varying = colnames(%(sim_df_name)s)[colnames(%(sim_df_name)s) != \"time\"],\n       v.names = \"value\",\n       timevar = \"variable\",\n       # Ensure variable names are used\n       times = colnames(%(sim_df_name)s)[colnames(%(sim_df_name)s) != \"time\"]\n     )\nrownames(%(sim_df_name)s) <- NULL",
    run_ode_julia = "\n\n# Run ODE\n%(prob_name)s = ODEProblem(%(ode_func_name)s!, %(model_setup_name)s.%(initial_value_name)s, %(times_name)s, %(model_setup_name)s.%(parameter_name)s)\n%(solution_name)s = solve(%(prob_name)s, %(method)s, dt = %(timestep_name)s, saveat = %(saveat_name)s, tstops = %(tstops_name)s, adaptive = false%(callback_arg)s)\n",
    post_ode_julia = "%(sim_df_name)s, %(parameter_name)s, %(parameter_names)s, %(initial_value_name)s, %(initial_value_names)s = clean_df(%(prob_name)s, %(solution_name)s, %(model_setup_name)s.%(initial_value_names)s, %(intermediaries_or_nothing)s, %(model_setup_name)s.%(intermediary_names)s)\n\nCSV.write(\"%(filepath_sim)s\", %(sim_df_name)s)\n\n# Delete variables\n%(solution_name)s = Nothing\n%(sim_df_name)s = Nothing\nNothing",

    # -- compile_run_ode: R saveat interpolation --------------------------------

    saveat_interval_r = "\n# Save at interval\nnew_times = seq(%(start)s, %(stop)s, by = %(save_at_val)s)\n%(sim_df_name)s = %(saveat_func)s(%(sim_df_name)s, 'time', new_times)\n",
    saveat_n_r        = "\n# Save n evenly-spaced points\nnew_times = seq(%(start)s, %(stop)s, length.out = %(save_n_val)s)\n%(sim_df_name)s = %(saveat_func)s(%(sim_df_name)s, 'time', new_times)\n",
    saveat_n1_r       = "\n# Save only stop\nnew_times = c(%(stop)s)\n%(sim_df_name)s = %(saveat_func)s(%(sim_df_name)s, 'time', new_times)\n",
    saveat_explicit_r = "\n# Explicit save times\nnew_times = c(%(save_at_str)s)\n%(sim_df_name)s = %(saveat_func)s(%(sim_df_name)s, 'time', new_times)\n",

    # -- compile_static: Julia ensemble definition --------------------------

    ensemble_def_range_julia = "\n\n# Generate ensemble design\n%(ensemble_n)s = %(n_value)s\n%(ensemble_range)s = (\n%(range_items)s,\n)\n%(ensemble_pars)s, %(ensemble_total_n)s = generate_param_combinations(\n%(ensemble_range)s; crossed=%(crossed)s, n_replicates = %(ensemble_n)s)\n%(ensemble_iter)s = 1\n",
    ensemble_def_norange_julia = "%(ensemble_n)s = %(n_value)s\n%(ensemble_total_n)s = %(n_value)s\n%(ensemble_iter)s = 1\n",
    ensemble_iter_julia = "\n\t# Assign ensemble parameters\n\t%(range_names)s, = %(ensemble_pars)s[div(%(ensemble_iter)s-1, %(ensemble_n)s) + 1]\n\n",

    # -- compile_run_ode: Julia ensemble problem --------------------------------

    ensemble_prob_julia = "\n\n# Create ODE problem\n%(prob_name)s = ODEProblem(%(ode_func_name)s!, %(model_setup_name)s.%(initial_value_name)s, %(times_name)s, %(model_setup_name)s.%(parameter_name)s)\n\n%(intermediaries_setup)s# Define ensemble problem\nfunction %(ensemble_func_name)s(prob, %(ensemble_iter)s, repeat)\n%(static_str)s%(intermediaries_callback)s\n\tremake(prob, u0 = %(model_setup_name)s.%(initial_value_name)s, p = %(model_setup_name)s.%(parameter_name)s%(intermediaries_remake)s)\nend\n\nfunction %(ensemble_output_func)s(sol, i)\n\t# Save both solution and parameters\n\treturn (t = sol.t, u = sol.u, p = sol.prob.p, u0 = sol.prob.u0), false\nend\n\n%(ensemble_prob_name)s = EnsembleProblem(%(prob_name)s, prob_func = %(ensemble_func_name)s, output_func = %(ensemble_output_func)s)\n%(solution_name)s = solve(%(ensemble_prob_name)s, %(method)s%(threaded_str)s, dt = %(timestep_name)s, saveat = %(saveat_name)s, tstops = %(tstops_name)s, adaptive = false, trajectories = %(ensemble_total_n)s);\n",

    # -- compile_run_ode: Julia ensemble save dataframe -------------------------

    ensemble_save_julia = "\n# Save timeseries dataframe\n%(sim_df_name)s, %(parameter_name)s, %(initial_value_name)s%(ensemble_to_df_func)s%(solution_name)s, %(model_setup_name)s.%(initial_value_names)s, %(intermediaries)s, %(model_setup_name)s.%(intermediary_names)s, %(ensemble_n)s)\n",

    # -- compile_run_ode: Julia ensemble save CSVs ------------------------------

    ensemble_csv_julia = "CSV.write(\"%(filepath_df)s\", %(sim_df_name)s)\nCSV.write(\"%(filepath_constants)s\", %(parameter_name)s)\nCSV.write(\"%(filepath_init)s\", %(initial_value_name)s)\n",

    # -- compile_run_ode: Julia ensemble summary stats --------------------------

    ensemble_summary_julia = "\n# Compute summary statistics\n%(summary_df_name)s%(ensemble_summ_func)s%(sim_df_name)s, [%(quantiles)s])\n\n%(parameter_name)s[!, :time] .= 0.0\n%(summary_df_constants_name)s%(ensemble_summ_func)s%(parameter_name)s, [%(quantiles)s])\nselect!(%(summary_df_constants_name)s, Not(:time))\n\n%(initial_value_name)s[!, :time] .= 0.0\n%(summary_df_init_name)s%(ensemble_summ_func)s%(initial_value_name)s, [%(quantiles)s])\nselect!(%(summary_df_init_name)s, Not(:time))\n\n\n# Save to CSV\nCSV.write(\"%(filepath_summary_df)s\", %(summary_df_name)s)\n\nCSV.write(\"%(filepath_summary_constants)s\", %(summary_df_constants_name)s)\n\nCSV.write(\"%(filepath_summary_init)s\", %(summary_df_init_name)s)\n\n# Delete variables\n%(sim_df_name)s = Nothing\n%(parameter_name)s = Nothing\n%(initial_value_name)s = Nothing\n%(summary_df_name)s = Nothing\n%(summary_df_constants_name)s = Nothing\n%(summary_df_init_name)s = Nothing\n%(solution_name)s = Nothing\n%(intermediaries)s = Nothing\n"
  )
}

extract_placeholders <- function(x) {
  unique(sub(
    "%\\(([^)]+)\\)s", "\\1",
    regmatches(x, gregexpr("%\\(([^)]+)\\)s", x))[[1]]
  ))
}

prep_script_template <- function() {
  # Get all templates
  templates <- script_template()
  nms <- names(templates)

  # Extract placeholders for each template
  placeholders <- lapply(templates, extract_placeholders)

  # Already replace placeholders contained in P
  templates <- lapply(nms, function(name) {
    tpl <- templates[[name]]
    replacements <- P[names(P) %in% placeholders[[name]]]
    if (length(replacements) == 0) {
      return(tpl)
    }
    names(replacements) <- paste0("%(", names(replacements), ")s")
    fmt(tpl, replacements)
  })
  names(templates) <- nms

  # Extract placeholders again for each template
  placeholders <- lapply(templates, extract_placeholders)

  # Merge into one list with templates and placeholders
  scripts <- list()
  for (name in names(templates)) {
    scripts[[name]] <- list(
      template = templates[[name]],
      placeholders = placeholders[[name]]
    )
  }
  names(scripts) <- nms

  scripts
}


# ---------------------------------------------------------------------------
# Public interface
# ---------------------------------------------------------------------------

#' Get a filled-in script template
#'
#' @param type Template type, e.g. `"times"`, `"prep"`, `"nonneg_stocks"`.
#' @param language `"R"` or `"Julia"`.
#' @param ... Named values to substitute into the template
#'
#' @returns Character string with placeholders replaced
#' @noRd
fmt_script <- function(type, language, ...) {
  # Get template
  key <- paste0(type, "_", tolower(language))
  scripts <- prep_script_template()
  tpl <- scripts[[key]]
  if (is.null(tpl)) {
    stop(sprintf("No template found for type='%s', language='%s' (key='%s')", type, language, key))
  }

  replacements <- do.call(c, list(...))
  if (length(replacements) == 0) {
    return(tpl[["template"]])
  }

  # Warn if any necessary placeholders are missing
  missing_placeholders <- setdiff(tpl[["placeholders"]], names(replacements))
  if (length(missing_placeholders) > 0) {
    stop(sprintf(
      "Missing placeholders for template type='%s', language='%s': %s",
      type, language,
      paste(missing_placeholders, collapse = ", ")
    ))
  }
  replacements <- replacements[tpl[["placeholders"]]] # only keep needed placeholders

  # Set placeholders with NULL or of length 0 to empty string
  idx <- vapply(replacements, function(x) is.null(x) || length(x) == 0, logical(1))
  replacements[idx] <- ""

  names(replacements) <- paste0("%(", names(replacements), ")s")

  fmt(tpl[["template"]], replacements)
}
