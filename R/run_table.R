# ============================================================================
# decomposer: run_table.R
#
# Generate the factorial design table for decomposition. Each row is one
# counterfactual combination: for n factors, there are 2^n rows (binary
# on/off) or 3^n rows if decomp_scalar = 0 (allowing intermediate levels).
# ============================================================================

#' Generate the factorial run table for decomposition
#'
#' Creates a \code{data.table} with one row per counterfactual combination.
#' Each factor column contains 0 (counterfactual) or 1 (observed), encoding
#' whether that factor is "on" or "off" for that run.
#'
#' @param factors        Character vector of factor names.
#' @param decomp_scalar  Numeric, the lower bound of the factor scale.
#'   Default is 0 (binary: 0 = off, 1 = on).
#'
#' @return A \code{data.table} with columns: \code{run_id} (integer), and one
#'   column per factor with values in \code{decomp_scalar:1}.
#' @export
make_run_table <- function(factors, decomp_scalar = 0) {
  run_table <- data.table::data.table(
    expand.grid(
      replicate(length(factors), list(decomp_scalar:1))
    )
  )
  names(run_table) <- factors
  run_table[, run_id := .I]
  data.table::setcolorder(
    run_table, c(ncol(run_table), seq_len(ncol(run_table) - 1))
  )
  return(run_table)
}
