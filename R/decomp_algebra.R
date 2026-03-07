# ============================================================================
# decomposer: decomp_algebra.R
#
# Model-agnostic decomposition algebra: Das Gupta and Shapley methods.
#
# These functions operate on a summary data.table that contains simulation
# outputs for all 2^n (or 3^n, etc.) counterfactual combinations. They are
# purely mathematical and have no knowledge of the underlying model.
# ============================================================================

#' Run decomposition across locations and decomp years
#'
#' Applies \code{decomp_algebra()} to each location × decomp-year group in
#' the data.
#'
#' @param data   A \code{data.table} with columns: \code{ihme_loc_id},
#'   \code{decomp_year}, one column per decomp variable (0/1 indicator), and
#'   one column per summary variable.
#' @param params A list with at least:
#'   \describe{
#'     \item{summary_vars}{Character vector of summary variable column names}
#'     \item{decomp_vars}{Character vector of factor column names (0/1 indicators)}
#'     \item{decomp_method}{Character, \code{"das_gupta"} or \code{"shapley"}}
#'   }
#'
#' @return A \code{data.table} with one row per location × decomp-year ×
#'   measure, and one column per factor giving its decomposition effect.
#' @export
run_decomp <- function(data, params) {
  group_cols <- intersect(c("ihme_loc_id", "decomp_year"), names(data))

  if (length(group_cols) == 0) {
    return(decomp_algebra(data, params))
  }

  groups <- unique(data[, group_cols, with = FALSE])
  decomp <- rbindlist(lapply(seq_len(nrow(groups)), function(i) {
    row <- groups[i]
    # Build filter expression
    subset_dt <- data
    for (col in group_cols) {
      subset_dt <- subset_dt[get(col) == row[[col]]]
    }
    result <- decomp_algebra(subset_dt, params)
    for (col in group_cols) {
      result[[col]] <- row[[col]]
    }
    result
  }))

  return(decomp)
}


#' Decompose all summary variables for a single group
#'
#' Applies \code{decomp_pair()} to each summary variable.
#'
#' @param data   A \code{data.table} for a single location × decomp-year with
#'   one row per counterfactual combination.
#' @param params Parameter list (see \code{run_decomp}).
#'
#' @return A \code{data.table} with one row per summary variable.
#' @export
decomp_algebra <- function(data, params) {
  dt <- data
  decomp <- rbindlist(lapply(params$summary_vars, function(summary_var) {
    decomp_pair(dt, params, summary_var)
  }))
  return(decomp)
}


#' Decompose a single summary variable using Das Gupta or Shapley weights
#'
#' For each factor, computes the weighted average marginal effect of switching
#' the factor from 0 (counterfactual) to 1 (observed), averaging over all
#' combinations of the other factors.
#'
#' @param dt           A \code{data.table} with one row per counterfactual
#'   combination.
#' @param params       Parameter list with \code{decomp_vars} and
#'   \code{decomp_method}.
#' @param summary_var  Character, the name of the summary variable column.
#'
#' @return A one-row \code{data.frame} with one column per factor (named by
#'   factor) plus a \code{measure} column.
#' @export
decomp_pair <- function(dt, params, summary_var) {
  path_totals <- dt[[summary_var]]
  vars <- params$decomp_vars
  n_vars <- length(vars)

  if (params$decomp_method %in% c("das_gupta", "shapley")) {
    # Das Gupta / Shapley weights: 1 / (n * C(n-1, k))
    # k = number of other vars that are "on"
    # These two methods produce identical weights for factorial decomposition.
    # Shapley: w(k) = k!(n-1-k)!/n! → denom = n!/(k!(n-1-k)!) = n*C(n-1,k)
    # Das Gupta: same formula derived from symmetric standardization.
    denoms <- n_vars * choose(n_vars - 1, 0:(n_vars - 1))
  } else {
    stop(sprintf("Unknown decomp_method: '%s'. Use 'das_gupta' or 'shapley'.",
                 params$decomp_method))
  }

  effects <- t(unlist(lapply(vars, function(x) {
    other <- setdiff(vars, x)
    after_idx  <- which(dt[[x]] == 1)
    before_idx <- which(dt[[x]] == 0)
    total_diffs <- path_totals[after_idx] - path_totals[before_idx]

    # Count number of "other" variables that are 1 (on) for each pair
    # k ranges from 0 to n-1; denom_idx = k + 1 for 1-based indexing
    denom_idx <- as.vector(
      apply(dt[after_idx, other, with = FALSE], 1, function(row) sum(row == 1) + 1)
    )

    effect <- sum(total_diffs / denoms[denom_idx])
    return(effect)
  })))

  colnames(effects) <- vars
  effects <- as.data.frame(effects)
  effects$measure <- summary_var

  return(effects)
}
