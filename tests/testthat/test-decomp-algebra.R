# ============================================================================
# tests/testthat/test-decomp-algebra.R
#
# Unit tests for the decomposition algebra (model-agnostic).
# ============================================================================

library(testthat)
library(data.table)

test_that("make_run_table generates correct factorial design", {
  rt <- make_run_table(c("a", "b"), decomp_scalar = 0)
  expect_equal(nrow(rt), 4)  # 2^2
  expect_true("run_id" %in% names(rt))
  expect_true(all(c("a", "b") %in% names(rt)))
  expect_equal(rt$run_id, 1:4)

  rt3 <- make_run_table(c("x", "y", "z"), decomp_scalar = 0)
  expect_equal(nrow(rt3), 8)  # 2^3
})


test_that("decomp_pair correctly decomposes a simple additive example", {
  # Simple additive case: outcome = 10*A + 20*B
  # A=0,B=0 -> 0; A=1,B=0 -> 10; A=0,B=1 -> 20; A=1,B=1 -> 30
  dt <- data.table(
    A = c(0, 1, 0, 1),
    B = c(0, 0, 1, 1),
    outcome = c(0, 10, 20, 30)
  )
  params <- list(
    decomp_vars = c("A", "B"),
    decomp_method = "das_gupta",
    summary_vars = "outcome"
  )

  result <- decomp_pair(dt, params, "outcome")
  expect_equal(result$A, 10, tolerance = 1e-10)
  expect_equal(result$B, 20, tolerance = 1e-10)
  expect_equal(result$measure, "outcome")
})


test_that("decomposition effects sum to total difference (Das Gupta)", {
  # Random example with 3 factors
  set.seed(42)
  rt <- make_run_table(c("f1", "f2", "f3"))
  dt <- copy(rt)
  # Assign outcome as some nonlinear function

  dt[, outcome := 100 + 30 * f1 + 50 * f2 + 20 * f3 + 10 * f1 * f2 + 5 * f2 * f3]

  params <- list(
    decomp_vars = c("f1", "f2", "f3"),
    decomp_method = "das_gupta",
    summary_vars = "outcome"
  )

  result <- decomp_algebra(dt, params)

  total_diff <- dt[f1 == 1 & f2 == 1 & f3 == 1, outcome] -
                dt[f1 == 0 & f2 == 0 & f3 == 0, outcome]
  sum_effects <- result$f1 + result$f2 + result$f3
  expect_equal(sum_effects, total_diff, tolerance = 1e-10)
})


test_that("decomposition effects sum to total difference (Shapley)", {
  rt <- make_run_table(c("f1", "f2", "f3"))
  dt <- copy(rt)
  dt[, outcome := 100 + 30 * f1 + 50 * f2 + 20 * f3 + 10 * f1 * f2 + 5 * f2 * f3]

  params <- list(
    decomp_vars = c("f1", "f2", "f3"),
    decomp_method = "shapley",
    summary_vars = "outcome"
  )

  result <- decomp_algebra(dt, params)

  total_diff <- dt[f1 == 1 & f2 == 1 & f3 == 1, outcome] -
                dt[f1 == 0 & f2 == 0 & f3 == 0, outcome]
  sum_effects <- result$f1 + result$f2 + result$f3
  expect_equal(sum_effects, total_diff, tolerance = 1e-10)
})


test_that("run_decomp handles grouped data", {
  dt <- data.table(
    ihme_loc_id = rep(c("AAA", "BBB"), each = 4),
    decomp_year = rep(2020, 8),
    A = rep(c(0, 1, 0, 1), 2),
    B = rep(c(0, 0, 1, 1), 2),
    outcome = c(0, 10, 20, 30, 100, 110, 120, 130)
  )
  params <- list(
    decomp_vars = c("A", "B"),
    decomp_method = "das_gupta",
    summary_vars = "outcome"
  )

  result <- run_decomp(dt, params)
  expect_equal(nrow(result), 2)  # one per location
  expect_true(all(result$A == 10))
  expect_true(all(result$B == 20))
})


test_that("unknown decomp method raises error", {
  dt <- data.table(A = c(0, 1), outcome = c(10, 20))
  params <- list(
    decomp_vars = "A",
    decomp_method = "unknown",
    summary_vars = "outcome"
  )
  expect_error(decomp_pair(dt, params, "outcome"), "Unknown decomp_method")
})
