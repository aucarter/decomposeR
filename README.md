# decomposer

A model-agnostic decomposition framework for discrete-time simulations.

## Overview

`decomposer` attributes changes in simulation outcomes to individual input
factors using **Das Gupta** or **Shapley** decomposition methods. Any
calibrated discrete-time simulation model can use this package by implementing
a small set of S3 methods.

## Two Counterfactual Types

The package supports two modes of counterfactual analysis:

| Type | `cf_type` | Counterfactual value | Interpretation |
|------|-----------|---------------------|----------------|
| **Change since** | `"change_since"` | Value at a baseline year | Impact of *changes in the factor since* the baseline year |
| **Total impact** | `"total_impact"` | Zero / no intervention | *Total* impact of the factor (as if it never existed) |

## Implementing the Interface

To use `decomposer` with your model, create an S3 class and implement these
**required** methods:

```r
# 1. Run your simulation with given parameters
run_simulation.my_model <- function(model, params, ...) { ... }

# 2. Summarize simulation output into a data.table with a 'year' column
#    plus one column per summary variable (e.g., deaths, infections)
summarize_results.my_model <- function(model, sim_output, ...) { ... }

# 3. Construct counterfactual parameters for a single factor
build_counterfactual.my_model <- function(model, params, factor_name,
                                          decomp_year, cf_type,
                                          baseline_values, ...) { ... }

# 4. Get the baseline value for a factor
get_baseline_value.my_model <- function(model, params, factor_name, cf_type,
                                        baseline_year, ...) { ... }
```

And these **optional** methods (for performance/setup):

```r
# One-time preparation before decomp loop (e.g., initial simulation,
# count-to-percentage conversion)
prepare_decomp.my_model <- function(model, params, ...) { ... }

# Warm-start support (avoid re-simulating years before decomp year)
supports_warm_start.my_model <- function(model, ...) { TRUE }
get_warm_start_state.my_model <- function(model, sim_output, year, ...) { ... }
run_from_warm_start.my_model <- function(model, warm_state, params,
                                          output_years, ...) { ... }
```

## Quick Start

```r
library(decomposer)

# Create your model object (S3 class with methods above)
model <- create_my_model(...)

# Run decomposition
results <- decompose(
  model        = model,
  params       = my_params,
  factors      = c("treatment_a", "treatment_b", "demographic_x"),
  summary_vars = c("deaths", "infections"),
  decomp_years = 2000:2023,
  method       = "das_gupta",   # or "shapley"
  cf_type      = "change_since" # or "total_impact"
)

# Results
results$decomp           # Standard decomposition (factor effects per year)
results$step_scenarios   # All-on time series per decomp year
results$extended_decomp  # Extended decomposition (effects at future years)
results$run_table        # Factorial design table
```

## Validation

Before running a full decomposition, validate that your model correctly
implements the interface:

```r
validate_model_interface(
  model        = my_model,
  params       = my_params,
  factors      = c("factor_a", "factor_b"),
  summary_vars = c("outcome_1"),
  baseline_year = 2000
)
```

## Decomposition Methods

- **Das Gupta**: Weights marginal effects by `1 / (n × C(n-1, k))` where
  `k` is the number of other factors in the "on" state. Standard epidemiological
  decomposition method.

- **Shapley**: Equal weights across all coalition sizes. Game-theoretic
  attribution method.

Both methods satisfy the property that factor effects sum to the total
difference between the fully-observed and fully-counterfactual scenarios.
