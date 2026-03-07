# ============================================================================
# decomposer: decompose.R
#
# Main entry point: orchestrates the full decomposition pipeline.
#
# The decompose() function:
#   1. Prepares the model (one-time setup)
#   2. Computes baseline values for each factor
#   3. Generates the factorial run table
#   4. Loops over decomposition years:
#      a. Runs all counterfactual combinations (with optional warm starts)
#      b. Collects summaries into a data.table
#      c. Runs the decomposition algebra
#      d. Accumulates step scenarios and extended decomposition
#   5. Returns all outputs in a structured list
# ============================================================================

#' Run a full decomposition analysis
#'
#' This is the main user-facing function. Given a model object that implements
#' the decomposer S3 interface, it performs a complete factorial decomposition
#' of the specified factors across all decomposition years.
#'
#' @param model          An S3 model object. Must have methods for
#'   \code{run_simulation()}, \code{summarize_results()},
#'   \code{build_counterfactual()}, and \code{get_baseline_value()}.
#' @param params         Model parameters (model-specific list/object).
#' @param factors        Character vector of factor names to decompose.
#' @param summary_vars   Character vector of summary variable names (columns
#'   in the output of \code{summarize_results()}).
#' @param decomp_years   Integer vector of years at which to perform
#'   decomposition (e.g., \code{2000:2023}).
#' @param method         Character, decomposition method: \code{"das_gupta"}
#'   (default) or \code{"shapley"}.
#' @param cf_type        Character, counterfactual type:
#'   \code{"change_since"} (default) — counterfactual is the value at the
#'   baseline year; or \code{"total_impact"} — counterfactual is zero / no
#'   intervention.
#' @param baseline_year  Integer, the reference year for \code{"change_since"}
#'   counterfactuals. Defaults to \code{min(decomp_years)}.
#' @param decomp_scalar  Numeric, lower bound for the factor levels in the
#'   run table. Default 0 (binary on/off).
#' @param extended       Logical, whether to compute the extended factor
#'   decomposition (effects at all output years >= decomp year). Default TRUE.
#' @param step_scenarios Logical, whether to compute step scenarios (all-on
#'   time series per decomp year). Default TRUE.
#' @param verbose        Logical, print progress messages. Default TRUE.
#' @param ...            Additional arguments passed to model methods.
#'
#' @return A list with components:
#'   \describe{
#'     \item{decomp}{data.table — standard year-by-year decomposition (one row
#'       per decomp year × measure, factor effects in columns)}
#'     \item{step_scenarios}{data.table — all-on time series per decomp year
#'       (columns: step_year, year, plus summary vars). NULL if
#'       \code{step_scenarios = FALSE}.}
#'     \item{extended_decomp}{data.table — extended decomposition (factor
#'       effects at all years >= decomp year). NULL if \code{extended = FALSE}.}
#'     \item{run_table}{data.table — the factorial design table used.}
#'     \item{params}{List of decomposition parameters used.}
#'   }
#' @export
decompose <- function(model,
                      params,
                      factors,
                      summary_vars,
                      decomp_years,
                      method         = "das_gupta",
                      cf_type        = CF_CHANGE_SINCE,
                      baseline_year  = min(decomp_years),
                      decomp_scalar  = 0,
                      extended       = TRUE,
                      step_scenarios = TRUE,
                      verbose        = TRUE,
                      ...) {

  # ---- Validate inputs ----
  stopifnot(is.character(factors), length(factors) > 0)
  stopifnot(is.character(summary_vars), length(summary_vars) > 0)
  stopifnot(is.numeric(decomp_years), length(decomp_years) > 0)
  stopifnot(cf_type %in% c(CF_CHANGE_SINCE, CF_TOTAL_IMPACT))
  stopifnot(method %in% c("das_gupta", "shapley"))

  decomp_years <- sort(as.integer(decomp_years))

  # ---- Build decomposition parameter bundle ----
  decomp_params <- list(
    decomp_vars   = factors,
    summary_vars  = summary_vars,
    decomp_method = method,
    cf_type       = cf_type,
    baseline_year = baseline_year
  )

  # ---- Generate run table ----
  run_table <- make_run_table(factors, decomp_scalar)
  all_on_id  <- max(run_table$run_id)
  all_off_id <- min(run_table$run_id)

  if (verbose) {
    message(sprintf(
      "Decomposition: %d factors, %d combinations, %d years, method=%s, cf_type=%s",
      length(factors), nrow(run_table), length(decomp_years), method, cf_type
    ))
  }

  # ---- Prepare model (one-time setup) ----
  if (verbose) message("Preparing model...")
  prep <- prepare_decomp(model, params, ...)
  model        <- prep$model
  params       <- prep$params
  full_sim     <- prep$full_sim
  full_summary <- prep$full_summary

  use_warm_start <- supports_warm_start(model, ...)

  # ---- Compute baseline values for all factors ----
  if (verbose) message("Computing baseline values...")
  baseline_values <- list()
  for (fname in factors) {
    baseline_values[[fname]] <- get_baseline_value(
      model, params, fname, cf_type, baseline_year, ...
    )
  }

  # ---- Accumulators ----
  step_list       <- list()
  ext_decomp_list <- list()

  # ---- Main decomposition loop ----
  decomp_by_year <- rbindlist(lapply(decomp_years, function(y) {
    if (verbose) cat("  decomp_year:", y, "\n")

    warm_start_year <- y - 1L
    output_years    <- seq(y, max(decomp_years))

    # Run all counterfactual combinations for this decomp year
    year_sims <- lapply(run_table$run_id, function(run_id) {
      run_spec <- as.list(run_table[run_id, factors, with = FALSE])
      run_spec <- unlist(run_spec)

      # All-on case: use the pre-computed full simulation
      if (all(run_spec == 1) && !is.null(full_summary)) {
        summary_subset <- full_summary[year %in% output_years]
        return(list(summary = summary_subset))
      }

      # Build counterfactual parameters
      params_cf <- params
      mod_factors <- names(run_spec[run_spec == 0])
      for (fname in mod_factors) {
        params_cf <- build_counterfactual(
          model, params_cf, fname, y, cf_type, baseline_values, ...
        )
      }

      # Run simulation (warm start or full)
      if (use_warm_start && !is.null(full_sim)) {
        sim_cf <- run_from_warm_start(
          model, full_sim, params_cf, output_years,
          warm_start_year = warm_start_year, ...
        )
      } else {
        sim_cf <- run_simulation(model, params_cf, ...)
      }

      summary_cf <- summarize_results(model, sim_cf, ...)

      # If warm start, summary may already be filtered to output_years
      # If full run, filter to output years
      if ("year" %in% names(summary_cf)) {
        summary_cf <- summary_cf[year %in% output_years]
      }

      return(list(summary = summary_cf))
    })

    # ---- Build summary table ----
    summary_dt <- rbindlist(lapply(seq_along(year_sims), function(r) {
      dt <- year_sims[[r]]$summary
      dt$run_id <- run_table$run_id[r]
      return(dt)
    }))
    summary_dt <- merge(run_table, summary_dt, by = "run_id")
    summary_dt[, decomp_year := y]

    # ---- Step scenarios ----
    if (step_scenarios) {
      sv_cols <- intersect(summary_vars, names(summary_dt))
      step_cols <- c("year", sv_cols)

      step_list[[as.character(y)]] <<- cbind(
        data.table(step_year = y),
        summary_dt[run_id == all_on_id, step_cols, with = FALSE]
      )

      # Also save counterfactual baseline from the first decomp year
      if (y == decomp_years[1]) {
        step_list[[as.character(decomp_years[1] - 1L)]] <<- cbind(
          data.table(step_year = decomp_years[1] - 1L),
          summary_dt[run_id == all_off_id, step_cols, with = FALSE]
        )
      }
    }

    # ---- Extended factor decomposition ----
    if (extended) {
      out_years_decomp <- seq(y, max(decomp_years))
      ext <- rbindlist(lapply(out_years_decomp, function(t) {
        yd <- run_decomp(summary_dt[year == t], decomp_params)
        yd[, year := t]
        yd
      }))
      ext[, decomp_year := y]
      ext_decomp_list[[as.character(y)]] <<- ext
    }

    # ---- Standard decomposition at the decomp year ----
    year_decomp <- run_decomp(summary_dt[year == y], decomp_params)
    return(year_decomp)
  }))

  # ---- Assemble outputs ----
  result <- list(
    decomp          = decomp_by_year,
    step_scenarios  = if (step_scenarios) rbindlist(step_list) else NULL,
    extended_decomp = if (extended) rbindlist(ext_decomp_list) else NULL,
    run_table       = run_table,
    params          = decomp_params
  )

  if (verbose) message("Decomposition complete.")
  return(result)
}
