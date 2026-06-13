test_that("rfcipDemand_controls returns default and custom elasticity limits", {
  default_ctrl <- rfcipDemand_controls()
  expect_type(default_ctrl, "list")
  expect_named(default_ctrl, "elasticity_limits")
  expect_equal(default_ctrl$elasticity_limits, fcip_elasticity_limits)
  
  custom_limits <- list(
    gamma = c(-1.5, 0, -1.5),
    theta = c(-1.2, 0, -1.2),
    total = c(-2.0, 0, -2.0)
  )
  custom_ctrl <- rfcipDemand_controls(elasticity_limits = custom_limits)
  expect_identical(custom_ctrl$elasticity_limits, custom_limits)
})

test_that("fixed_effect_model_data_prep demeans variables and keeps FE metadata", {
  dt <- data.table::data.table(
    pool = c("A", "A", "B", "B"),
    commodity_year = c(2019, 2020, 2019, 2020),
    x = c(1, 3, 2, 4),
    y = c(10, 20, 30, 40)
  )
  
  res <- fixed_effect_model_data_prep(
    data = dt,
    varlist = "x",
    panel = "pool",
    time = "commodity_year",
    output = "y"
  )
  
  expect_equal(res$NFE, 2)
  expect_equal(res$data$x, c(1.5, 3.5, 1.5, 3.5), tolerance = 1e-12)
  expect_true(all(c("y_mean", "y_mean_i") %in% names(res$data)))
})

test_that("fixed_effect_model_data_prep creates default output and drops singletons", {
  dt <- data.table::data.table(
    pool = c("A", "A", "B"),
    commodity_year = c(2019, 2020, 2019),
    w = c(1, 2, 3),
    x = c(5, 7, 9)
  )
  
  res <- fixed_effect_model_data_prep(
    data = dt,
    varlist = "x",
    panel = "pool",
    time = "commodity_year",
    wvar = "w",
    output = NULL
  )
  
  expect_equal(res$NFE, 1)
  expect_equal(nrow(res$data), 2)
  expect_true(all(res$data$output == 1))
  expect_true("w" %in% names(res$data))
})

test_that("fcip_demand_sys_prep drops invalid rows and removes constant partials", {
  dt <- data.table::data.table(
    gamma = c(1, 2, 3, 4),
    theta = c(2, 3, 4, 5),
    rate = c(0.1, 0.2, 0.3, 0.4),
    x = c(10, 11, 12, 13),
    z = c(5, 6, 7, 8),
    p = c(1, 2, 3, Inf),
    p_const = c(7, 7, 7, 7),
    pool = c(1, 1, 2, 2),
    commodity_year = c(2019, 2020, 2019, 2020)
  )
  
  fields <- list(
    outcome = c("gamma", "theta"),
    endogenous = "rate",
    included = "x",
    excluded = "z",
    partial = c("p", "p_const"),
    FE = FALSE,
    disag = "pool"
  )
  
  res <- fcip_demand_sys_prep(data = dt, fields = fields)
  expect_equal(res$NFE, 0)
  expect_equal(res$partial, "p")
  expect_equal(nrow(res$data), 3)
  expect_true(all(is.finite(res$data$p)))
})

test_that("fcip_demand_sys_partial builds instruments and tilda variables", {
  dt <- data.frame(
    gamma = c(1, 2, 3, 4, 5, 6),
    theta = c(2, 3, 4, 5, 6, 7),
    rate = c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45),
    x = c(5, 6, 7, 8, 9, 10),
    z = c(1, 2, 1, 2, 1, 2),
    p = c(10, 11, 12, 13, 14, 15)
  )
  
  fields <- list(
    outcome = c("gamma", "theta"),
    endogenous = "rate",
    included = "x",
    excluded = "z",
    partial = "p"
  )
  
  out <- fcip_demand_sys_partial(data = dt, fields = fields)
  
  expect_true("instr_rate" %in% names(out$data))
  expect_setequal(out$tilda_included, "tilda_x")
  expect_setequal(out$tilda_endogenous, "tilda_rate")
  expect_setequal(out$tilda_excluded, "instr_rate")
  expect_true(all(c("tilda_gamma", "tilda_theta", "tilda_rate", "tilda_x") %in% names(out$data)))
})

test_that("fcip_demand_sys_partial warns on missing columns and copies when no partial", {
  dt <- data.frame(
    gamma = c(1, 2, 3, 4),
    theta = c(2, 3, 4, 5),
    rate = c(0.2, 0.3, 0.4, 0.5),
    x = c(5, 6, 7, 8)
  )
  
  fields_missing <- list(
    outcome = c("gamma", "theta"),
    endogenous = "rate",
    included = c("x", "missing_col"),
    excluded = NULL,
    partial = NULL
  )
  
  expect_warning(
    out_warn <- fcip_demand_sys_partial(data = dt, fields = fields_missing),
    "Ignoring missing columns"
  )
  
  expect_null(out_warn$tilda_excluded)
  expect_equal(out_warn$data$tilda_rate, out_warn$data$rate)
  expect_equal(out_warn$data$tilda_gamma, out_warn$data$gamma)
  expect_equal(out_warn$data$tilda_theta, out_warn$data$theta)
})
