#' Create stock-and-flow model from model library
#'
#' Create a stock-and-flow model from a template in the model library. The function will return a stock-and-flow model ready to be simulated and plotted, or modified in any way you like.
#'
#' @param template Name of model.
#'
#' @noRd
#' @returns Stock-and-flow model of class [`sdbuildR`][sdbuildR]
#'
templates <- function(template) {
  model_names <- c(
    "logistic_model", "SIR", "predator_prey",
    "cusp",
    "Crielaard2022",
    "coffee_cup", "bank_account",
    "Lorenz", "Rossler", "vanderPol", "Duffing", "Chua",
    "JDR"
    # "spruce_budworm"
  )

  if (missing(template)) {
    cli::cli_inform(c(
      "Choose from the following templates:",
      " " = paste0(model_names, collapse = "\n")
    ))
    return(invisible())
  }

  if (!is.character(template)) {
    cli::cli_abort(c(
      "Invalid {.arg template} argument.",
      "x" = "The {.arg template} argument must be {.cls character}."
    ))
  }

  if (length(template) != 1) {
    cli::cli_abort(c(
      "Invalid {.arg template} length.",
      "x" = "The {.arg template} argument must be a single {.cls character} string."
    ))
  }

  if (!template %in% model_names) {
    cli::cli_abort(c(
      "Template not found.",
      "i" = "Available templates are:",
      " " = paste0(model_names, collapse = "\n")
    ))
  }


  if (template == "logistic_model") {
    object <- sdbuildR() |>
      meta(name = "Logistic model") |>
      sim_specs(stop = 200) |>
      update("X", "stock", eqn = ".01", label = "Population size") |>
      # update("flow", "flow", eqn = "r * X * (1 - X / K)", to = "X", label = "Net change") |>
      update("inflow", "flow", eqn = "r * X", to = "X", label = "Births") |>
      update("outflow", "flow", eqn = "r * X^2 / K", from = "X", label = "Deaths") |>
      update("r", "constant", eqn = "0.1", label = "Growth rate") |>
      update("K", "constant", eqn = "1", label = "Carrying capacity")
  } else if (template == "SIR") {
    # Chapter 5 Duggan: SIR Aggregate.R & Chapter 7. Screening.R
    # https://github.com/JimDuggan/SDMR/blob/master/models/05%20Chapter/R/01%20SIR%20Aggregate.R
    # https://github.com/JimDuggan/SDMR/blob/master/models/07%20Chapter/R/02%20Screening.R

    object <- sdbuildR() |>
      meta(name = "Susceptible-Infected-Recovered (SIR)") |>
      sim_specs(start = 0, stop = 20, time_units = "weeks") |>
      update("Susceptible", "stock", eqn = "99999") |>
      update("Infected", "stock", eqn = 1) |>
      update("Recovered", "stock", eqn = 0) |>
      update("Beta", "constant", eqn = "Effective_Contact_Rate / Total_Population") |>
      update("Lambda", "aux", eqn = "Beta * Infected") |>
      update("Infection_Rate", "flow", eqn = "Susceptible * Lambda", from = "Susceptible", to = "Infected") |>
      update("Recovery_Rate", "flow", eqn = "Infected / Delay", from = "Infected", to = "Recovered") |>
      update("Total_Population", "constant", eqn = "100000") |>
      update("Effective_Contact_Rate", "constant", eqn = "2") |>
      update("Delay", "constant", eqn = "2")
  } else if (template == "predator_prey") {
    object <- sdbuildR() |>
      meta(name = "Predator-Prey Dynamics (Lotka-Volterra)") |>
      sim_specs(method = "euler", stop = 500) |>
      update("predator", "stock", eqn = 10, label = "Predator") |>
      update("prey", "stock", eqn = 50, label = "Prey") |>
      update("predator_births", "flow",
        eqn = "delta*prey*predator",
        label = "Births", to = "predator"
      ) |>
      update("predator_deaths", "flow",
        eqn = "gamma*predator",
        label = "Deaths", from = "predator"
      ) |>
      update("prey_births", "flow",
        eqn = "alpha*prey",
        label = "Births", to = "prey"
      ) |>
      update("prey_deaths", "flow",
        eqn = "beta*prey*predator",
        label = "Deaths", from = "prey"
      ) |>
      update(c("delta", "gamma", "alpha", "beta"), "constant",
        eqn = c(.025, .5, .5, .05),
        label = c("Delta", "Gamma", "Alpha", "Beta"),
        doc = c(
          "Birth rate of predators", "Death rate of predators",
          "Birth rate of prey", "Death rate of prey by predators"
        )
      )
  } else if (template == "cusp") {
    object <- sdbuildR() |>
      meta(name = "Cusp Catastrophe") |>
      sim_specs(method = "euler", stop = 500) |>
      update("x", "stock", eqn = .1) |>
      update("dxdt", "flow",
        eqn = "a + b*x - x^3 + rnorm(1, dt)",
        to = "x"
      ) |>
      update("a", "constant", eqn = 2, label = "Normal variable") |>
      update("b", "constant", eqn = 2, label = "Splitting variable")
  } else if (template == "Crielaard2022") {
    object <- sdbuildR() |>
      meta(
        name = "Eating Behaviour (Crielaard et al., 2022)",
        doi = "10.1037/met0000484"
      ) |>
      sim_specs(time_units = "days", stop = 100) |>
      update("Food_intake", "stock",
        eqn = "runif(1)",
        label = "Food intake"
      ) |>
      update("Hunger", "stock", eqn = "runif(1)") |>
      update("Compensatory_behaviour", "stock",
        eqn = "runif(1)",
        label = "Compensatory behaviour"
      ) |>
      update("Losing_energy_by_compensatory_behavior", "flow",
        eqn = "(0.3 * Compensatory_behaviour) * ((1 - Hunger) / 1)",
        label = "Losing energy by compensatory behavior", to = "Hunger"
      ) |>
      update("Feeling_hunger", "flow",
        eqn = "((0.8 * Hunger^a0) * Food_intake) * ((1 - Food_intake) / 1)",
        label = "Feeling hunger", to = "Food_intake"
      ) |>
      update("Satiety", "flow",
        eqn = "(1.3 * (Food_intake)) * ((1 - Food_intake) / 1)",
        from = "Food_intake"
      ) |>
      update("Food_intake_reduces_hunger", "flow",
        eqn = "((1.4 * Food_intake^a0) * Hunger) * ((1 - Hunger) / 1)", label = "Food intake reduces hunger",
        from = "Hunger"
      ) |>
      update("Compensating_for_having_eaten", "flow",
        eqn = "(Sig(a2 * Food_intake)) * ((1 - Compensatory_behaviour) / 1)",
        label = "Compensating for having eaten",
        to = "Compensatory_behaviour"
      ) |>
      update("Satisfaction_with_hungry_feeling", "flow",
        eqn = "(1.3 * Hunger * Compensatory_behaviour) * ((1 - Compensatory_behaviour) / 1)",
        label = "Satisfaction with hungry feeling", from = "Compensatory_behaviour"
      ) |>
      update("Effect_of_eating_triggers", "flow",
        eqn = "(a1 * Food_intake) * ((1 - Food_intake) / 1)",
        to = "Food_intake", label = "Effect of eating triggers"
      ) |>
      update("Effect_of_compensatory_behavior", "flow",
        eqn = "(2 * Compensatory_behaviour * Food_intake) * ((1 - Food_intake) / 1)",
        from = "Food_intake",
        label = "Effect of compensatory behavior"
      ) |>
      update(c("a0", "a1", "a2"), "constant", eqn = c(1.31, 1.5, 0.38)) |>
      custom_func(name = "Sig", eqn = "function(x) 1 / (1 + exp(1)^(-x))")
  } else if (template == "coffee_cup") {
    object <- sdbuildR() |>
      meta(name = "Coffee cup", caption = "Coffee cup cooling or heating from Meadows' Thinking in Systems (Chapter 1)") |>
      sim_specs(stop = 100, dt = 1, time_units = "minute", language = "Julia") |>
      update("coffee_temperature", "stock", eqn = "100", units = "Celsius", label = "Coffee temperature") |>
      update("cooling", "flow", eqn = "discrepancy * .1 / u('min')", units = "Celsius/min", to = "coffee_temperature", label = "Cooling or heating") |>
      update("discrepancy", "aux", eqn = "room_temperature - coffee_temperature", units = "Celsius", label = "Discrepancy") |>
      update("room_temperature", "constant", eqn = "18", units = "Celsius", label = "Room temperature")
  } else if (template == "bank_account") {
    object <- sdbuildR() |>
      meta(
        name = "Bank account with interest",
        caption = "Bank account with compounding interest from Meadows' Thinking in Systems (Chapter 1)"
      ) |>
      sim_specs(start = 0, stop = 12, dt = 1, time_units = "year", language = "Julia") |>
      update("money_in_bank_account", "stock",
        eqn = "100",
        label = "Money in bank account", units = "dollar"
      ) |>
      update("interest_added", "flow",
        eqn = "money_in_bank_account * interest_rate / u('1year')",
        label = "Adding interest",
        units = "dollar/year", to = "money_in_bank_account"
      ) |>
      update("interest_rate", "constant",
        eqn = ".02", label = "Interest rate",
        units = "1"
      )
  } else if (template == "Lorenz") {
    object <- sdbuildR() |>
      meta(
        name = "Lorenz Attractor",
        caption = "Lorenz Attractor system for chaotic dynamics"
      ) |>
      sim_specs(stop = 50, time_units = "hours") |>
      # Stocks
      update("x", "stock", eqn = "1") |>
      update("y", "stock", eqn = "1") |>
      update("z", "stock", eqn = "1") |>
      # Flows (differential equations)
      update("dx_dt", "flow", eqn = "sigma * (y - x)", to = "x", label = "Rate of change of X") |>
      update("dy_dt", "flow", eqn = "x * (rho - z) - y", to = "y", label = "Rate of change of Y") |>
      update("dz_dt", "flow", eqn = "x * y - beta * z", to = "z", label = "Rate of change of Z") |>
      # Parameters
      update("sigma", "constant", eqn = "10") |>
      update("rho", "constant", eqn = "28") |>
      update("beta", "constant", eqn = "8/3")
  } else if (template == "Rossler") {
    object <- sdbuildR() |>
      meta(
        name = "Rossler Attractor",
        caption = "Chaotic Rossler system in 3D"
      ) |>
      sim_specs(stop = 100, time_units = "hours") |>
      # Stocks
      update("x", "stock", eqn = "1") |>
      update("y", "stock", eqn = "1") |>
      update("z", "stock", eqn = "1") |>
      # Flows
      update("dx_dt", "flow", eqn = "-y - z", to = "x", label = "Rate of change of X") |>
      update("dy_dt", "flow", eqn = "x + a * y", to = "y", label = "Rate of change of Y") |>
      update("dz_dt", "flow", eqn = "b + z * (x - c)", to = "z", label = "Rate of change of Z") |>
      # Parameters
      update("a", "constant", eqn = "0.2") |>
      update("b", "constant", eqn = "0.2") |>
      update("c", "constant", eqn = "5.7")
  } else if (template == "vanderPol") {
    object <- sdbuildR() |>
      meta(
        name = "Van der Pol Oscillator",
        caption = "Nonlinear oscillator with limit cycle behavior"
      ) |>
      sim_specs(stop = 50, time_units = "hours") |>
      # Stocks
      update("x", "stock", eqn = "0.1", label = "Position") |>
      update("y", "stock", eqn = "0", label = "Velocity") |>
      # Flows
      update("dx_dt", "flow", eqn = "y", to = "x", label = "Rate of change of position") |>
      update("dy_dt", "flow", eqn = "mu * (1 - x^2) * y - x", to = "y", label = "Rate of change of velocity") |>
      # Parameters
      update("mu", "constant", eqn = "1", label = "Damping parameter")
  } else if (template == "Duffing") {
    object <- sdbuildR() |>
      meta(
        name = "Duffing Oscillator",
        caption = "Nonlinear oscillator with forcing"
      ) |>
      sim_specs(stop = 100, time_units = "hours") |>
      # Stocks
      update("x", "stock", eqn = "0.1", label = "Position") |>
      update("y", "stock", eqn = "0", label = "Velocity") |>
      # Flows
      update("dx_dt", "flow",
        eqn = "y", to = "x",
        label = "Rate of change of position"
      ) |>
      update("dy_dt", "flow",
        eqn = "-delta * y - alpha * x - beta * x^3 + gamma * cos(omega * t)",
        to = "y", label = "Rate of change of velocity"
      ) |>
      # Parameters
      update("delta", "constant", eqn = "0.3", label = "Damping coefficient") |>
      update("alpha", "constant", eqn = "-1", label = "Linear stiffness") |>
      update("beta", "constant", eqn = "1", label = "Nonlinear stiffness") |>
      update("gamma", "constant", eqn = "0.5", label = "Forcing amplitude") |>
      update("omega", "constant", eqn = "1.2", label = "Forcing frequency")
  } else if (template == "Chua") {
    object <- sdbuildR() |>
      meta(name = "Chua's Circuit", caption = "Chaotic electronic circuit model") |>
      sim_specs(stop = 50, time_units = "hours") |>
      # Stocks
      update("x", "stock", eqn = "0.1", label = "Voltage 1") |>
      update("y", "stock", eqn = "0", label = "Voltage 2") |>
      update("z", "stock", eqn = "0", label = "Current") |>
      # Flows
      update("dx_dt", "flow", eqn = "alpha * (y - x - fx)", to = "x", label = "Rate of change of x") |>
      update("dy_dt", "flow", eqn = "x - y + z", to = "y", label = "Rate of change of y") |>
      update("dz_dt", "flow", eqn = "-beta * y", to = "z", label = "Rate of change of z") |>
      update("fx", "aux",
        eqn = "m1 * x + 0.5 * (m0 - m1) * (abs(x + 1) - abs(x - 1))",
        label = "Nonlinear resistor"
      ) |>
      # Parameters
      update("alpha", "constant", eqn = "15.6", label = "Parameter alpha") |>
      update("beta", "constant", eqn = "28", label = "Parameter beta") |>
      update("m0", "constant", eqn = "-1.143", label = "Nonlinear slope m0") |>
      update("m1", "constant", eqn = "-0.714", label = "Nonlinear slope m1")
  } else if (template == "JDR") {
    object <- sdbuildR() |>
      sim_specs(method = "euler", start = "0.0", stop = "182.5", dt = "0.01", save_at = "0.1", seed = "123", time_units = "d", language = "R") |>
      meta(
        name = "Job Resources and Demands Theory",
        caption = "JD-R Theory as formalized in Evers et al. (submitted)"
      ) |>
      update(name = "E", type = "stock", eqn = "0.5", label = "Engagement") |>
      update(name = "R", type = "stock", eqn = "0.7", label = "Job Resources") |>
      update(name = "D", type = "stock", eqn = "0.2", label = "Job Demands") |>
      update(name = "X", type = "stock", eqn = "0.5", label = "Energy") |>
      update(name = "r_E_R", type = "constant", eqn = "0.2", label = "Motivation Rate") |>
      update(name = "K_E", type = "constant", eqn = "1", label = "Engagement Capacity") |>
      update(name = "r_A", type = "constant", eqn = "0.2", label = "Proactive Behaviour Rate") |>
      update(name = "K_R", type = "constant", eqn = "1", label = "Resource Capacity") |>
      update(name = "r_R", type = "constant", eqn = "0.05", label = "Resource Decay Rate") |>
      update(name = "r_X_D", type = "constant", eqn = "0.4", label = "Fatigue from Demand Rate") |>
      update(name = "K_X", type = "constant", eqn = "1", label = "Energy Capacity") |>
      update(name = "r_X_X", type = "constant", eqn = "0.15", label = "Restoration Rate") |>
      update(name = "r_E_X", type = "constant", eqn = "0.1", label = "Energy-Based Disengagement Rate") |>
      update(name = "r_U", type = "constant", eqn = "0.15", label = "Self-undermining Rate") |>
      update(name = "K_D", type = "constant", eqn = "1", label = "Demand Capacity") |>
      update(name = "r_D", type = "constant", eqn = "0.2", label = "Demand Regulation Rate") |>
      update(name = "P", type = "aux", eqn = "E + X", label = "Job Performance") |>
      update(name = "E_R", type = "flow", eqn = "r_E_R * X * R * (1 + D) * (1 - E/K_E)", to = "E", label = "Motivation", doc = "Boost of demands") |>
      update(name = "A_to_R", type = "flow", eqn = "r_A * E * (1 - R/K_R)", to = "R", label = "Proactive Behaviour") |>
      # update(name = "R_X", type = "flow", eqn = "r_R_X * R", from = "R", label = "Decay") |>
      update(name = "from_R", type = "flow", eqn = "r_R * R * (1 - X/K_X)", from = "R", label = "Decay from effort") |>
      update(name = "X_D", type = "flow", eqn = "r_X_D * X * D / (1 + R)", from = "X", label = "Effort", doc = "Buffer of resources") |>
      update(name = "X_X", type = "flow", eqn = "r_X_X * X * (1 - X/K_X)", to = "X", label = "Restoration") |>
      update(name = "E_X", type = "flow", eqn = "r_E_X * E * (1 - X/K_X)", from = "E", label = "Energy-Based Disengagement") |>
      update(name = "A_from_D", type = "flow", eqn = "r_A * E * D", from = "D", label = "Proactive Behaviour") |>
      update(name = "U", type = "flow", eqn = "r_U * (1 - X/K_X)", to = "D", label = "Self-undermining") |>
      update(name = "D_D", type = "flow", eqn = "r_D * (1 - D/K_D)", to = "D", label = "Demand Regulation")
  }


  # Compile the model to populate the assemble cache


  object
}
