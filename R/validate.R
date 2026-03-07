# ============================================================================
# decomposer: validate.R
#
# Validation utilities for checking that a model correctly implements the
# decomposer S3 interface.
# ============================================================================

#' Validate that a model implements the decomposer interface
#'
#' Checks that the required S3 methods exist for the given model class and
#' runs a minimal smoke test if parameters and factors are provided.
#'
#' @param model          An S3 model object
#' @param params         Optional: model parameters for smoke test
#' @param factors        Optional: character vector of factor names
#' @param summary_vars   Optional: character vector of summary variable names
#' @param baseline_year  Optional: integer baseline year
#' @param verbose        Logical, print diagnostic messages
#'
#' @return Invisible TRUE if all checks pass; stops with an error otherwise.
#' @export
validate_model_interface <- function(model, params = NULL, factors = NULL,
                                     summary_vars = NULL, baseline_year = NULL,
                                     verbose = TRUE) {
  cls <- class(model)[1]
  if (verbose) message(sprintf("Validating decomposer interface for class '%s'", cls))

  # Check required methods exist
  required <- c("run_simulation", "summarize_results",
                "build_counterfactual", "get_baseline_value")

  for (fn_name in required) {
    method_name <- paste0(fn_name, ".", cls)
    if (!is.function(tryCatch(getS3method(fn_name, cls), error = function(e) NULL))) {
      stop(sprintf("Required method '%s' not found for class '%s'.", fn_name, cls))
    }
    if (verbose) message(sprintf("  [OK] %s.%s", fn_name, cls))
  }

  # Check optional methods
  optional <- c("prepare_decomp", "supports_warm_start",
                "get_warm_start_state", "run_from_warm_start")
  for (fn_name in optional) {
    has_method <- is.function(
      tryCatch(getS3method(fn_name, cls), error = function(e) NULL)
    )
    if (verbose) {
      status <- if (has_method) "[OK]" else "[--]"
      message(sprintf("  %s %s.%s (optional)", status, fn_name, cls))
    }
  }

  # Smoke test if params provided
  if (!is.null(params) && !is.null(factors) && !is.null(summary_vars)) {
    if (verbose) message("Running smoke test...")

    # Test run_simulation + summarize_results
    sim <- run_simulation(model, params)
    summary <- summarize_results(model, sim)

    if (!inherits(summary, "data.table") && !inherits(summary, "data.frame")) {
      stop("summarize_results() must return a data.table or data.frame.")
    }
    if (!"year" %in% names(summary)) {
      stop("summarize_results() output must contain a 'year' column.")
    }
    missing_vars <- setdiff(summary_vars, names(summary))
    if (length(missing_vars) > 0) {
      stop(sprintf(
        "summarize_results() output missing summary_vars: %s",
        paste(missing_vars, collapse = ", ")
      ))
    }
    if (verbose) message("  [OK] run_simulation + summarize_results")

    # Test get_baseline_value
    if (!is.null(baseline_year)) {
      for (fname in factors) {
        bv <- get_baseline_value(model, params, fname, CF_CHANGE_SINCE,
                                 baseline_year)
        if (verbose) message(sprintf("  [OK] get_baseline_value('%s')", fname))
      }
    }

    # Test build_counterfactual
    test_year <- if (!is.null(baseline_year)) baseline_year + 1L else 2001L
    baseline_values <- list()
    if (!is.null(baseline_year)) {
      for (fname in factors) {
        baseline_values[[fname]] <- get_baseline_value(
          model, params, fname, CF_CHANGE_SINCE, baseline_year
        )
      }
    }

    params_cf <- build_counterfactual(
      model, params, factors[1], test_year, CF_CHANGE_SINCE, baseline_values
    )
    if (verbose) message(sprintf("  [OK] build_counterfactual('%s')", factors[1]))

    if (verbose) message("Smoke test passed.")
  }

  if (verbose) message("Validation complete.")
  invisible(TRUE)
}
