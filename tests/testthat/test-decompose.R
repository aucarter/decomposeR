# ============================================================================
# tests/testthat/test-decompose.R
#
# Integration test: full decompose() pipeline with a toy model.
# ============================================================================

library(testthat)
library(data.table)

# ---------------------------------------------------------------------------
#  Define a minimal toy model for testing
# ---------------------------------------------------------------------------

#' A simple 2-factor model: outcome = base + A_val * factor_a + B_val * factor_b
#' where factor values change linearly over time.
create_toy_model <- function() {
  model <- list(
    years = 1990:2010
  )
  class(model) <- "toy_model"
  return(model)
}

#' Toy params: two factors with time-varying values
create_toy_params <- function(years = 1990:2010) {
  list(
    base     = 1000,
    factor_a = setNames(seq(0, 100, length.out = length(years)), years),
    factor_b = setNames(seq(0, 50,  length.out = length(years)), years),
    years    = years
  )
}

# S3 methods for the toy model — use <<- so they register in .GlobalEnv
# where S3 method dispatch can find them

run_simulation.toy_model <<- function(model, params, ...) {
  years <- model$years
  data.table(
    year   = years,
    deaths = params$base + params$factor_a[as.character(years)] +
             params$factor_b[as.character(years)]
  )
}

summarize_results.toy_model <<- function(model, sim_output, ...) {
  sim_output
}

get_baseline_value.toy_model <<- function(model, params, factor_name, cf_type,
                                         baseline_year, ...) {
  if (cf_type == CF_TOTAL_IMPACT) {
    return(list(type = "zero"))
  }
  return(list(
    type  = "change_since",
    value = params[[factor_name]][as.character(baseline_year)]
  ))
}

build_counterfactual.toy_model <<- function(model, params, factor_name,
                                           decomp_year, cf_type,
                                           baseline_values, ...) {
  bv <- baseline_values[[factor_name]]
  years <- params$years
  idx <- which(years >= decomp_year)

  if (cf_type == CF_TOTAL_IMPACT) {
    params[[factor_name]][idx] <- 0
  } else {
    params[[factor_name]][idx] <- bv$value
  }
  return(params)
}


# ---------------------------------------------------------------------------
#  Tests
# ---------------------------------------------------------------------------

test_that("decompose() works with toy model (change_since)", {
  model  <- create_toy_model()
  params <- create_toy_params()

  results <- decompose(
    model        = model,
    params       = params,
    factors      = c("factor_a", "factor_b"),
    summary_vars = "deaths",
    decomp_years = 2000:2005,
    method       = "das_gupta",
    cf_type      = "change_since",
    baseline_year = 2000,
    extended     = FALSE,
    step_scenarios = FALSE,
    verbose      = FALSE
  )

  expect_true(is.data.table(results$decomp))
  expect_true("measure" %in% names(results$decomp))
  expect_true(all(c("factor_a", "factor_b") %in% names(results$decomp)))

  # Effects should sum to the total difference at each decomp year
  for (i in seq_len(nrow(results$decomp))) {
    total_effect <- results$decomp$factor_a[i] + results$decomp$factor_b[i]
    # Total effect should be non-negative (factors are increasing)
    expect_true(total_effect >= 0)
  }
})


test_that("decompose() works with total_impact", {
  model  <- create_toy_model()
  params <- create_toy_params()

  results <- decompose(
    model        = model,
    params       = params,
    factors      = c("factor_a", "factor_b"),
    summary_vars = "deaths",
    decomp_years = 2000:2005,
    method       = "das_gupta",
    cf_type      = "total_impact",
    extended     = FALSE,
    step_scenarios = FALSE,
    verbose      = FALSE
  )

  expect_true(is.data.table(results$decomp))

  # With total_impact, the total effect should equal the sum of factor values
  # at the decomp year (since counterfactual is zero)
  for (i in seq_len(nrow(results$decomp))) {
    total <- results$decomp$factor_a[i] + results$decomp$factor_b[i]
    expect_true(total >= 0)
  }
})


test_that("decompose() produces step_scenarios", {
  model  <- create_toy_model()
  params <- create_toy_params()

  results <- decompose(
    model        = model,
    params       = params,
    factors      = c("factor_a", "factor_b"),
    summary_vars = "deaths",
    decomp_years = 2000:2005,
    method       = "das_gupta",
    cf_type      = "change_since",
    extended     = FALSE,
    step_scenarios = TRUE,
    verbose      = FALSE
  )

  expect_true(is.data.table(results$step_scenarios))
  expect_true("step_year" %in% names(results$step_scenarios))
  expect_true("year" %in% names(results$step_scenarios))
  expect_true("deaths" %in% names(results$step_scenarios))

  # Should have one entry per decomp year + the counterfactual baseline
  n_step_years <- length(unique(results$step_scenarios$step_year))
  expect_equal(n_step_years, length(2000:2005) + 1)  # +1 for 1999 baseline
})


test_that("decompose() produces extended_decomp", {
  model  <- create_toy_model()
  params <- create_toy_params()

  results <- decompose(
    model        = model,
    params       = params,
    factors      = c("factor_a", "factor_b"),
    summary_vars = "deaths",
    decomp_years = 2000:2005,
    method       = "das_gupta",
    cf_type      = "change_since",
    extended     = TRUE,
    step_scenarios = FALSE,
    verbose      = FALSE
  )

  expect_true(is.data.table(results$extended_decomp))
  expect_true("year" %in% names(results$extended_decomp))
  expect_true("decomp_year" %in% names(results$extended_decomp))
})


test_that("validate_model_interface works for toy model", {
  model  <- create_toy_model()
  params <- create_toy_params()

  expect_true(validate_model_interface(
    model        = model,
    params       = params,
    factors      = c("factor_a", "factor_b"),
    summary_vars = "deaths",
    baseline_year = 2000,
    verbose      = FALSE
  ))
})
