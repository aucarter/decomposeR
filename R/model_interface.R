# ============================================================================
# decomposer: model_interface.R
#
# S3 generic functions that any discrete-time simulation model must implement
# to use the decomposer framework.
#
# Required methods:
#   - run_simulation()        Run the model with given parameters
#   - summarize_results()     Extract summary measures from simulation output
#   - build_counterfactual()  Construct counterfactual parameters for a factor
#   - get_baseline_value()    Get baseline value for a factor (for change_since)
#
# Optional methods (for performance):
#   - prepare_decomp()        One-time model preparation before decomp loop
#   - supports_warm_start()   Whether the model supports warm starts
#   - get_warm_start_state()  Extract model state at a given year
#   - run_from_warm_start()   Run model from a saved state
# ============================================================================

#' @title Counterfactual type constants
#' @description
#' Two types of counterfactual analysis are supported:
#' \itemize{
#'   \item \code{CF_CHANGE_SINCE}: The counterfactual value is the factor's
#'     value at the baseline year (typically the first decomposition year).
#'     This measures the impact of changes in the factor \emph{since} that
#'     year.
#'   \item \code{CF_TOTAL_IMPACT}: The counterfactual value is zero (or the
#'     model's notion of "no intervention"). This measures the \emph{total}
#'     impact of the factor, not just changes since a reference point.
#' }
#' @export
CF_CHANGE_SINCE <- "change_since"

#' @export
CF_TOTAL_IMPACT <- "total_impact"


# ---------------------------------------------------------------------------
#  Required S3 generics
# ---------------------------------------------------------------------------

#' Run a simulation with the given parameters
#'
#' Execute the discrete-time simulation model and return raw simulation output.
#' This is called once for each counterfactual combination at each decomposition
#' year.
#'
#' @param model   An S3 model object (class determines dispatch)
#' @param params  Model parameters (possibly modified for counterfactual)
#' @param ...     Additional arguments passed to the method
#'
#' @return Raw simulation output (model-specific format). Will be passed to
#'   \code{summarize_results()}.
#' @export
run_simulation <- function(model, params, ...) {

  UseMethod("run_simulation")
}


#' Summarize simulation output into a data.table
#'
#' Extract summary measures (e.g., deaths, infections) from raw simulation
#' output. Must return a \code{data.table} with at minimum a \code{year}
#' column and one column per summary variable specified in the decomposition.
#'
#' @param model       An S3 model object
#' @param sim_output  Raw simulation output from \code{run_simulation()}
#' @param ...         Additional arguments
#'
#' @return A \code{data.table} with columns: \code{year}, plus one column per
#'   summary variable (e.g., \code{deaths}, \code{infections}).
#' @export
summarize_results <- function(model, sim_output, ...) {
  UseMethod("summarize_results")
}


#' Construct counterfactual parameters for a factor
#'
#' Given the observed parameters, produce a modified parameter set where the
#' specified factor is set to its counterfactual value from
#' \code{decomp_year} onward.
#'
#' @param model        An S3 model object
#' @param params       Observed model parameters
#' @param factor_name  Character, name of the factor to modify
#' @param decomp_year  Integer, the year from which the counterfactual applies
#' @param cf_type      Character, one of \code{CF_CHANGE_SINCE} or
#'   \code{CF_TOTAL_IMPACT}
#' @param baseline_values  Named list of baseline values for each factor
#'   (pre-computed by \code{get_baseline_value()})
#' @param ...          Additional arguments
#'
#' @return Modified model parameters with the factor set to its counterfactual
#'   value.
#' @export
build_counterfactual <- function(model, params, factor_name, decomp_year,
                                 cf_type, baseline_values, ...) {
  UseMethod("build_counterfactual")
}


#' Get the baseline value for a factor
#'
#' Returns the value of a factor at the baseline year (for
#' \code{CF_CHANGE_SINCE}) or the zero/null value (for
#' \code{CF_TOTAL_IMPACT}). Called once per factor before the decomposition
#' loop begins.
#'
#' @param model     An S3 model object
#' @param params    Observed model parameters
#' @param factor_name  Character, name of the factor
#' @param cf_type   Character, \code{CF_CHANGE_SINCE} or \code{CF_TOTAL_IMPACT}
#' @param baseline_year  Integer, the reference year for \code{CF_CHANGE_SINCE}
#' @param ...       Additional arguments
#'
#' @return The baseline value for the factor (format is model-specific).
#' @export
get_baseline_value <- function(model, params, factor_name, cf_type,
                               baseline_year, ...) {
  UseMethod("get_baseline_value")
}


# ---------------------------------------------------------------------------
#  Optional S3 generics (with defaults)
# ---------------------------------------------------------------------------

#' Prepare the model for decomposition
#'
#' One-time preparation step called before the decomposition loop. Use this
#' for expensive setup operations like running an initial simulation to extract
#' denominators, converting counts to percentages, etc.
#'
#' The default method returns the model and params unchanged.
#'
#' @param model   An S3 model object
#' @param params  Model parameters
#' @param ...     Additional arguments
#'
#' @return A list with components:
#'   \item{model}{Possibly modified model object}
#'   \item{params}{Possibly modified parameters}
#'   \item{full_sim}{Optional: output of a full (observed) simulation, used
#'     for warm starts and as the "all-on" reference}
#'   \item{full_summary}{Optional: summary of the full simulation}
#' @export
prepare_decomp <- function(model, params, ...) {
  UseMethod("prepare_decomp")
}

#' @export
prepare_decomp.default <- function(model, params, ...) {
  sim <- run_simulation(model, params, ...)
  summary <- summarize_results(model, sim, ...)
  list(
    model        = model,
    params       = params,
    full_sim     = sim,
    full_summary = summary
  )
}


#' Check whether a model supports warm starts
#'
#' If TRUE, the decomposer will use \code{get_warm_start_state()} and
#' \code{run_from_warm_start()} to avoid re-simulating years before each
#' decomposition year.
#'
#' The default method returns FALSE.
#'
#' @param model  An S3 model object
#' @param ...    Additional arguments
#'
#' @return Logical
#' @export
supports_warm_start <- function(model, ...) {
  UseMethod("supports_warm_start")
}

#' @export
supports_warm_start.default <- function(model, ...) {
  FALSE
}


#' Extract model state at a given year for warm restart
#'
#' @param model       An S3 model object
#' @param sim_output  Full simulation output from \code{run_simulation()}
#' @param year        Integer, the year at which to extract state
#' @param ...         Additional arguments
#'
#' @return A model state object (model-specific format)
#' @export
get_warm_start_state <- function(model, sim_output, year, ...) {
  UseMethod("get_warm_start_state")
}

#' @export
get_warm_start_state.default <- function(model, sim_output, year, ...) {
  stop(
    "get_warm_start_state() not implemented for class '",
    paste(class(model), collapse = "', '"),
    "'. Set supports_warm_start() to return TRUE and implement this method."
  )
}


#' Run a simulation from a saved warm-start state
#'
#' @param model         An S3 model object
#' @param warm_state    State object from \code{get_warm_start_state()}
#' @param params        Model parameters (possibly counterfactual)
#' @param output_years  Integer vector of years to produce output for
#' @param ...           Additional arguments
#'
#' @return Raw simulation output (same format as \code{run_simulation()})
#' @export
run_from_warm_start <- function(model, warm_state, params, output_years, ...) {
  UseMethod("run_from_warm_start")
}

#' @export
run_from_warm_start.default <- function(model, warm_state, params,
                                        output_years, ...) {
  stop(
    "run_from_warm_start() not implemented for class '",
    paste(class(model), collapse = "', '"),
    "'. Set supports_warm_start() to return TRUE and implement this method."
  )
}
