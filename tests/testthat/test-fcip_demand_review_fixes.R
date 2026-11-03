simulate_demand_data <- function(seed, b_gamma, b_theta, pool_effect_sd = 0) {
  set.seed(seed)
  d <- data.table::data.table(pool = rep(1:300, each = 10), commodity_year = rep(2010:2019, 300))
  d[, a := stats::rnorm(1, 0, pool_effect_sd), by = pool]
  d[, z := stats::rnorm(.N) + 0.5 * a]
  d[, u := stats::rnorm(.N)]
  d[, x := stats::rnorm(.N)]
  d[, rate := 0.8 * z + 0.5 * u + 0.3 * x + stats::rnorm(.N)]
  d[, g := b_gamma * rate + 0.2 * x + u + a + stats::rnorm(.N)]
  d[, t := b_theta * rate + 0.1 * x + 0.5 * u + a + stats::rnorm(.N)]
  d[]
}

test_model <- function(FE = FALSE) {
  list(name = "sim", FE = FE, outcome = c("g", "t"), endogenous = "rate",
       excluded = "z", included = NULL, partial = "x", disag = NULL)
}

test_that("constrained elasticities respect all three limits", {
  skip_if_not_installed("lavaan")
  d <- simulate_demand_data(2, b_gamma = 0.3, b_theta = 0.2)  # both positive
  lim <- rfcipDemand_controls()$elasticity_limits

  res <- fcip_demand_sys_estimate(test_model(), d, constrained_elasticities = TRUE)
  est <- stats::setNames(res$Estimate, res$demand)

  expect_true(all(res$constrained == 1))
  expect_lte(est[["gamma"]], lim$gamma[1] + 1e-6)
  expect_lte(est[["theta"]], lim$theta[1] + 1e-6)
  expect_lte(est[["total"]], lim$total[1] + 1e-6)
})

test_that("malformed elasticity limits fail loudly", {
  skip_if_not_installed("lavaan")
  d <- data.frame(instr_rate = 1, tilda_rate = 1, tilda_gamma = 1, tilda_theta = 1)
  bad <- list(elasticity_limits = list(gamma = -0.1, theta = NA_real_, total = -0.1))
  expect_error(fcip_demand_elasticities_lavaan(d, control = bad), "theta")
})

test_that("FE = TRUE gives within estimates with valid standard errors", {
  d <- simulate_demand_data(3, b_gamma = -0.4, b_theta = -0.1, pool_effect_sd = 3)

  fe <- fcip_demand_sys_estimate(test_model(FE = TRUE), d)
  fe <- fe[grepl("tilda_rate", coef)]

  # Reference: demean everything by pool by hand, then estimate without FE
  dm <- data.table::copy(d)
  for (v in c("g", "t", "rate", "z", "x")) dm[, (v) := get(v) - mean(get(v)), by = pool]
  ref <- fcip_demand_sys_estimate(test_model(FE = FALSE), dm)
  ref <- ref[grepl("tilda_rate", coef)]

  expect_equal(fe$Estimate, ref$Estimate, tolerance = 1e-6)
  expect_true(all(is.finite(fe$StdError) & fe$StdError > 0))
  # Same up to the degrees-of-freedom correction for the absorbed effects
  expect_equal(fe$StdError, ref$StdError, tolerance = 0.1)
})

test_that("disaggregation keeps levels with 30+ observations in any year", {
  d <- simulate_demand_data(4, b_gamma = -0.4, b_theta = -0.1)
  d[, level := "kept"]
  thin <- data.table::copy(d[commodity_year == 2010][1:29])
  thin[, level := "dropped"]
  d <- rbind(d, thin)
  m <- test_model(); m$disag <- "level"
  res <- fcip_demand_sys_estimate(m, d)
  expect_true("kept" %in% res$level)
  expect_false("dropped" %in% res$level)
})
