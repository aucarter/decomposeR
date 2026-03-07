# ============================================================================
# examples/sir_model.R
#
# Example: Implementing the decomposer interface for a simple discrete-time
# SIR model. This demonstrates how to use the decomposer package with an
# arbitrary model.
#
# Model: SIR with two decomposable factors:
#   - beta (transmission rate) — can be reduced by interventions
#   - treatment (recovery rate enhancement) — represents treatment availability
#
# The model runs in discrete yearly time steps.
# ============================================================================

library(data.table)
library(decomposer)

# ---------------------------------------------------------------------------
#  1. Define the model
# ---------------------------------------------------------------------------

#' Create an SIR model object
#'
#' @param population  Total population size
#' @param years       Integer vector of simulation years
#' @return An S3 object of class "sir_model"
create_sir_model <- function(population = 100000, years = 1990:2020) {
  model <- list(
    population = population,
    years      = years,
    S0         = population - 10,  # Initial susceptible
    I0         = 10,               # Initial infected
    R0         = 0                 # Initial recovered
  )
  class(model) <- "sir_model"
  return(model)
}

#' Create SIR model parameters
#'
#' @param years          Integer vector of simulation years
#' @param base_beta      Baseline transmission rate (contact rate × probability)
#' @param gamma          Base recovery rate
#' @param beta_reduction Named numeric vector: year -> reduction in beta from
#'                       interventions (0 = no reduction, 1 = full elimination)
#' @param treatment_effect Named numeric vector: year -> additive recovery rate
#'                         increase from treatment availability
create_sir_params <- function(years = 1990:2020,
                              base_beta = 0.3,
                              gamma = 0.1,
                              beta_reduction = NULL,
                              treatment_effect = NULL) {
  n <- length(years)
  yr_names <- as.character(years)

  # Default: gradually increasing intervention coverage
  if (is.null(beta_reduction)) {
    beta_reduction <- setNames(
      pmin(seq(0, 0.5, length.out = n), 0.5),
      yr_names
    )
  }
  if (is.null(treatment_effect)) {
    treatment_effect <- setNames(
      pmin(seq(0, 0.15, length.out = n), 0.15),
      yr_names
    )
  }

  list(
    years            = years,
    base_beta        = base_beta,
    gamma            = gamma,
    beta_reduction   = beta_reduction,
    treatment_effect = treatment_effect
  )
}


# ---------------------------------------------------------------------------
#  2. Implement the decomposer S3 interface
# ---------------------------------------------------------------------------

#' Run the SIR simulation
#' @export
run_simulation.sir_model <- function(model, params, ...) {
  years <- model$years
  n     <- length(years)
  N     <- model$population

  S <- numeric(n); I <- numeric(n); R <- numeric(n)
  new_infections <- numeric(n); deaths <- numeric(n)

  S[1] <- model$S0; I[1] <- model$I0; R[1] <- model$R0

  for (t in 2:n) {
    yr <- as.character(years[t])
    beta  <- params$base_beta * (1 - params$beta_reduction[yr])
    gamma <- params$gamma + params$treatment_effect[yr]

    new_inf <- beta * S[t-1] * I[t-1] / N
    new_rec <- gamma * I[t-1]
    disease_deaths <- 0.01 * I[t-1]  # 1% disease mortality

    S[t] <- S[t-1] - new_inf
    I[t] <- I[t-1] + new_inf - new_rec - disease_deaths
    R[t] <- R[t-1] + new_rec

    new_infections[t] <- new_inf
    deaths[t] <- disease_deaths
  }

  data.table(
    year       = years,
    S          = S,
    I          = I,
    R          = R,
    infections = new_infections,
    deaths     = deaths
  )
}

#' Summarize SIR output
#' @export
summarize_results.sir_model <- function(model, sim_output, ...) {
  # sim_output is already a data.table with year, infections, deaths
  sim_output[, .(year, infections, deaths)]
}

#' Get baseline value for a factor
#' @export
get_baseline_value.sir_model <- function(model, params, factor_name, cf_type,
                                         baseline_year, ...) {
  yr <- as.character(baseline_year)
  if (cf_type == CF_TOTAL_IMPACT) {
    return(list(type = "zero"))
  }
  # CF_CHANGE_SINCE: hold at baseline year value
  if (factor_name == "beta_reduction") {
    return(list(type = "change_since", value = params$beta_reduction[yr]))
  } else if (factor_name == "treatment_effect") {
    return(list(type = "change_since", value = params$treatment_effect[yr]))
  } else {
    stop(sprintf("Unknown factor: %s", factor_name))
  }
}

#' Build counterfactual parameters
#' @export
build_counterfactual.sir_model <- function(model, params, factor_name,
                                           decomp_year, cf_type,
                                           baseline_values, ...) {
  bv    <- baseline_values[[factor_name]]
  years <- params$years
  idx   <- which(years >= decomp_year)

  if (cf_type == CF_TOTAL_IMPACT) {
    # Zero out the factor
    params[[factor_name]][idx] <- 0
  } else {
    # Hold at baseline value
    params[[factor_name]][idx] <- bv$value
  }
  return(params)
}


# ---------------------------------------------------------------------------
#  3. Run the decomposition
# ---------------------------------------------------------------------------

if (FALSE) {  # Set to TRUE to run

  model  <- create_sir_model(years = 1990:2020)
  params <- create_sir_params(years = 1990:2020)

  # Validate the interface
  validate_model_interface(
    model, params,
    factors      = c("beta_reduction", "treatment_effect"),
    summary_vars = c("deaths", "infections"),
    baseline_year = 2000
  )

  # Run decomposition: impact of changes since 2000
  results_since <- decompose(
    model        = model,
    params       = params,
    factors      = c("beta_reduction", "treatment_effect"),
    summary_vars = c("deaths", "infections"),
    decomp_years = 2000:2020,
    method       = "das_gupta",
    cf_type      = "change_since",
    baseline_year = 2000
  )

  cat("=== Change Since 2000 ===\n")
  print(results_since$decomp)

  # Run decomposition: total impact of each factor
  results_total <- decompose(
    model        = model,
    params       = params,
    factors      = c("beta_reduction", "treatment_effect"),
    summary_vars = c("deaths", "infections"),
    decomp_years = 2000:2020,
    method       = "das_gupta",
    cf_type      = "total_impact"
  )

  cat("\n=== Total Impact ===\n")
  print(results_total$decomp)
}
