test_that("returns named list with expected elements", {
  res <- adjust_agent_outcomes_by_elasticity(
    alternate_premium_per_liability   = 0.10,
    insured_acres_elasticity          = -0.4,
    baseline_coverage_type            = "A",
    coverage_level_elasticity         = -0.5,
    baseline_coverage_level           = 0.70,
    baseline_insured_acres            = 100,
    baseline_liability_per_acre       = 200,
    baseline_premium_per_liability    = 0.08,
    baseline_subsidy_per_premium      = 0.60,
    baseline_indemnity_per_acre       = 50,
    final_revenue_per_acre            = 120,
    assumption                        = 0
  )
  expect_type(res, "list")
  expect_setequal(
    names(res),
    c("coverage_level_percent","insured_acres",
      "liability_amount","total_premium_amount","subsidy_amount",
      "indemnity_amount","price_change_pct","coverage_type_code")
  )
})

test_that("price_change_pct is computed correctly", {
  res <- adjust_agent_outcomes_by_elasticity(
    alternate_premium_per_liability   = 0.10,  # +25% vs 0.08
    insured_acres_elasticity          = 0,
    baseline_coverage_type            = "A",
    coverage_level_elasticity         = 0,
    baseline_coverage_level           = 0.70,
    baseline_insured_acres            = 100,
    baseline_liability_per_acre       = 200,
    baseline_premium_per_liability    = 0.08,
    baseline_subsidy_per_premium      = 0.60,
    baseline_indemnity_per_acre       = 50,
    final_revenue_per_acre            = 120,
    assumption                        = 0
  )
  expect_equal(res$price_change_pct, 25)  # 100*((0.10/0.08) - 1)
})

test_that("assumption 0: coverage and acres fixed; totals consistent", {
  alt_p  <- 0.10
  base_p <- 0.08
  base <- list(cov=0.70, acres=100, liab_pa=200, indm_pa=50, subs_share=0.60, rev_pa=120)
  
  res <- adjust_agent_outcomes_by_elasticity(
    alternate_premium_per_liability   = alt_p,
    insured_acres_elasticity          = -0.4,  # irrelevant here
    coverage_level_elasticity         = -0.5,  # irrelevant here
    baseline_coverage_type            = "A",
    baseline_coverage_level           = base$cov,
    baseline_insured_acres            = base$acres,
    baseline_liability_per_acre       = base$liab_pa,
    baseline_premium_per_liability    = base_p,
    baseline_subsidy_per_premium      = base$subs_share,
    baseline_indemnity_per_acre       = base$indm_pa,
    final_revenue_per_acre            = base$rev_pa,
    assumption                        = 0
  )
  
  expect_equal(res$coverage_level_percent, base$cov)
  expect_equal(res$insured_acres, base$acres)
  
  # Cross-check via adjust_indemnity_liability_per_acre (same coverage → per-acre unchanged)
  adj <- adjust_indemnity_liability_per_acre(
    coverage_level_percent      = base$cov,
    final_revenue_per_acre      = base$rev_pa,
    baseline_coverage_level     = base$cov,
    baseline_liability_per_acre = base$liab_pa,
    baseline_indemnity_per_acre = base$indm_pa
  )
  expect_equal(res$liability_amount, adj$adj_Liability_per_acre * base$acres, tolerance = 1e-10)
  expect_equal(res$indemnity_amount, adj$adj_Indemnity_per_acre * base$acres, tolerance = 1e-10)
  expect_equal(res$total_premium_amount, alt_p * res$liability_amount, tolerance = 1e-10)
  expect_equal(res$subsidy_amount, 0.60 * res$total_premium_amount, tolerance = 1e-10)
})

test_that("assumption 1: only acres respond to price", {
  base <- list(cov=0.70, acres=100, liab_pa=200, indm_pa=50, subs=0.60, rev_pa=120)
  alt_p <- 0.10; base_p <- 0.08; gamma <- -0.4
  pc <- 100 * (alt_p/base_p - 1) # 25
  
  res <- adjust_agent_outcomes_by_elasticity(
    alternate_premium_per_liability   = alt_p,
    insured_acres_elasticity          = gamma,
    coverage_level_elasticity         = -0.5,  # irrelevant here
    baseline_coverage_type            = "A",
    baseline_coverage_level           = base$cov,
    baseline_insured_acres            = base$acres,
    baseline_liability_per_acre       = base$liab_pa,
    baseline_premium_per_liability    = base_p,
    baseline_subsidy_per_premium      = base$subs,
    baseline_indemnity_per_acre       = base$indm_pa,
    final_revenue_per_acre            = base$rev_pa,
    assumption                        = 1
  )
  
  exp_acres <- base$acres * (1 + (gamma * pc)/100)  # 100*(1 - 0.1) = 90
  expect_equal(res$insured_acres, exp_acres, tolerance = 1e-10)
  expect_equal(res$coverage_level_percent, base$cov)
  
  # per-acre unchanged (coverage unchanged)
  adj <- adjust_indemnity_liability_per_acre(
    coverage_level_percent      = base$cov,
    final_revenue_per_acre      = base$rev_pa,
    baseline_coverage_level     = base$cov,
    baseline_liability_per_acre = base$liab_pa,
    baseline_indemnity_per_acre = base$indm_pa
  )
  expect_equal(res$liability_amount, adj$adj_Liability_per_acre * exp_acres, tolerance = 1e-10)
  expect_equal(res$indemnity_amount, adj$adj_Indemnity_per_acre * exp_acres, tolerance = 1e-10)
})

test_that("assumption 2: only coverage responds with rounding and caps", {
  base <- list(cov=0.70, acres=100, liab_pa=200, indm_pa=50, subs=0.60, rev_pa=120)
  alt_p <- 0.10; base_p <- 0.08; theta <- -0.5
  pc <- 100 * (alt_p/base_p - 1) # 25
  
  # Expected coverage: 0.7 * (1 - 0.5*0.25) = 0.6125 → round to 0.60; cap [0,0.85]
  exp_cov <- 0.60
  
  res <- adjust_agent_outcomes_by_elasticity(
    alternate_premium_per_liability   = alt_p,
    insured_acres_elasticity          = -0.4,  # irrelevant here
    coverage_level_elasticity         = theta,
    baseline_coverage_type            = "A",
    baseline_coverage_level           = base$cov,
    baseline_insured_acres            = base$acres,
    baseline_liability_per_acre       = base$liab_pa,
    baseline_premium_per_liability    = base_p,
    baseline_subsidy_per_premium      = base$subs,
    baseline_indemnity_per_acre       = base$indm_pa,
    final_revenue_per_acre            = base$rev_pa,
    assumption                        = 2
  )
  
  expect_equal(res$coverage_level_percent, exp_cov, tolerance = 1e-12)
  expect_equal(res$insured_acres, base$acres)
  
  # Cross-check totals via adjust_indemnity_liability_per_acre with new coverage
  adj <- adjust_indemnity_liability_per_acre(
    coverage_level_percent      = exp_cov,
    final_revenue_per_acre      = base$rev_pa,
    baseline_coverage_level     = base$cov,
    baseline_liability_per_acre = base$liab_pa,
    baseline_indemnity_per_acre = base$indm_pa
  )
  expect_equal(res$liability_amount, adj$adj_Liability_per_acre * base$acres, tolerance = 1e-10)
  expect_equal(res$indemnity_amount, adj$adj_Indemnity_per_acre * base$acres, tolerance = 1e-10)
  expect_equal(res$total_premium_amount, alt_p * res$liability_amount, tolerance = 1e-10)
  expect_equal(res$subsidy_amount, base$subs * res$total_premium_amount, tolerance = 1e-10)
})

test_that("assumption 2: coverage capped to 0.85 and floored to 0 if < 0.5", {
  # Cap to 0.85
  res_cap <- adjust_agent_outcomes_by_elasticity(
    alternate_premium_per_liability   = 0.04,  # -50% vs 0.08
    insured_acres_elasticity          = 0,
    coverage_level_elasticity         = -0.5,  # coverage increases
    baseline_coverage_type            = "A",
    baseline_coverage_level           = 0.70,
    baseline_insured_acres            = 1,
    baseline_liability_per_acre       = 1,
    baseline_premium_per_liability    = 0.08,
    baseline_subsidy_per_premium      = 0.50,
    baseline_indemnity_per_acre       = 0,
    final_revenue_per_acre            = 0,
    assumption                        = 2
  )
  expect_equal(res_cap$coverage_level_percent, 0.85) # 0.7 * 1.25 = 0.875 → round 0.90 → cap 0.85
  
  # Floor to 0 if < 0.5 after rounding
  res_floor <- adjust_agent_outcomes_by_elasticity(
    alternate_premium_per_liability   = 0.20,  # +150% vs 0.08
    insured_acres_elasticity          = 0,
    coverage_level_elasticity         = -0.5,  # coverage decreases
    baseline_coverage_type            = "A",
    baseline_coverage_level           = 0.55,
    baseline_insured_acres            = 1,
    baseline_liability_per_acre       = 1,
    baseline_premium_per_liability    = 0.08,
    baseline_subsidy_per_premium      = 0.50,
    baseline_indemnity_per_acre       = 0,
    final_revenue_per_acre            = 0,
    assumption                        = 2
  )
  expect_equal(res_floor$coverage_level_percent, 0.00) # 0.55 * (1 - 0.75) = 0.1375 → 0.15 → < 0.5 → 0
})

test_that("assumption 3: both coverage and acres respond", {
  base <- list(cov=0.70, acres=100, liab_pa=200, indm_pa=50, subs=0.60, rev_pa=120)
  alt_p <- 0.10; base_p <- 0.08; gamma <- -0.4; theta <- -0.5
  pc <- 100 * (alt_p/base_p - 1) # 25
  
  res <- adjust_agent_outcomes_by_elasticity(
    alternate_premium_per_liability   = alt_p,
    insured_acres_elasticity          = gamma,
    coverage_level_elasticity         = theta,
    baseline_coverage_type            = "A",
    baseline_coverage_level           = base$cov,
    baseline_insured_acres            = base$acres,
    baseline_liability_per_acre       = base$liab_pa,
    baseline_premium_per_liability    = base_p,
    baseline_subsidy_per_premium      = base$subs,
    baseline_indemnity_per_acre       = base$indm_pa,
    final_revenue_per_acre            = base$rev_pa,
    assumption                        = 3
  )
  
  # Expected coverage (rounded) and acres
  exp_cov   <- 0.60  # from earlier calc
  exp_acres <- base$acres * (1 + (gamma * pc)/100)  # 90
  expect_equal(res$coverage_level_percent, exp_cov, tolerance = 1e-12)
  expect_equal(res$insured_acres, exp_acres, tolerance = 1e-10)
  
  # Totals cross-check via adjust_indemnity_liability_per_acre
  adj <- adjust_indemnity_liability_per_acre(
    coverage_level_percent      = exp_cov,
    final_revenue_per_acre            = base$rev_pa,
    baseline_coverage_level     = base$cov,
    baseline_liability_per_acre = base$liab_pa,
    baseline_indemnity_per_acre = base$indm_pa
  )
  expect_equal(res$liability_amount, adj$adj_Liability_per_acre * exp_acres, tolerance = 1e-10)
  expect_equal(res$indemnity_amount, adj$adj_Indemnity_per_acre * exp_acres, tolerance = 1e-10)
  expect_equal(res$total_premium_amount, alt_p * res$liability_amount, tolerance = 1e-10)
  expect_equal(res$subsidy_amount, base$subs * res$total_premium_amount, tolerance = 1e-10)
})

test_that("assumption must be length 1", {
  expect_error(
    adjust_agent_outcomes_by_elasticity(
      alternate_premium_per_liability   = 0.10,
      insured_acres_elasticity          = 0,
      coverage_level_elasticity         = 0,
      baseline_coverage_type            = "A",
      baseline_coverage_level           = 0.7,
      baseline_insured_acres            = 100,
      baseline_liability_per_acre       = 200,
      baseline_premium_per_liability    = 0.08,
      baseline_subsidy_per_premium      = 0.6,
      baseline_indemnity_per_acre       = 50,
      final_revenue_per_acre            = 120,
      assumption                        = c(0,1)   # invalid
    ),
    "length\\(assumption\\) == 1"
  )
})
