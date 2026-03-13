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

  data[, decomp_algebra(.SD, params), by = group_cols]
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
  vars <- params$decomp_vars
  n_vars <- length(vars)
  sv_cols <- intersect(params$summary_vars, names(dt))

  if (params$decomp_method %in% c("das_gupta", "shapley")) {
    denoms <- n_vars * choose(n_vars - 1, 0:(n_vars - 1))
  } else {
    stop(sprintf("Unknown decomp_method: '%s'. Use 'das_gupta' or 'shapley'.",
                 params$decomp_method))
  }

  # Process all summary vars at once via matrix operations
  val_mat <- as.matrix(dt[, sv_cols, with = FALSE])
  effects <- matrix(0, nrow = length(sv_cols), ncol = n_vars)

  for (idx in seq_along(vars)) {
    x <- vars[idx]
    other <- setdiff(vars, x)
    after_idx  <- which(dt[[x]] == 1)
    before_idx <- which(dt[[x]] == 0)

    diffs <- val_mat[after_idx, , drop = FALSE] -
             val_mat[before_idx, , drop = FALSE]

    if (length(other) > 0) {
      k <- rowSums(as.matrix(dt[after_idx, other, with = FALSE]) == 1)
    } else {
      k <- rep(0L, length(after_idx))
    }
    weights <- 1 / denoms[k + 1L]

    effects[, idx] <- colSums(diffs * weights)
  }

  result <- data.table(measure = sv_cols)
  for (idx in seq_along(vars)) {
    set(result, i = NULL, j = vars[idx], value = effects[, idx])
  }
  return(result)
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
    denoms <- n_vars * choose(n_vars - 1, 0:(n_vars - 1))
  } else {
    stop(sprintf("Unknown decomp_method: '%s'. Use 'das_gupta' or 'shapley'.",
                 params$decomp_method))
  }

  effects <- vapply(vars, function(x) {
    other <- setdiff(vars, x)
    after_idx  <- which(dt[[x]] == 1)
    before_idx <- which(dt[[x]] == 0)
    total_diffs <- path_totals[after_idx] - path_totals[before_idx]

    if (length(other) > 0) {
      k <- rowSums(as.matrix(dt[after_idx, other, with = FALSE]) == 1)
    } else {
      k <- rep(0L, length(after_idx))
    }
    sum(total_diffs / denoms[k + 1L])
  }, numeric(1))

  effects <- as.data.frame(t(effects))
  effects$measure <- summary_var
  return(effects)
}
