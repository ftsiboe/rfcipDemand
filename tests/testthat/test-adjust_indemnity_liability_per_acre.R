test_that("returns named list with correct lengths", {
  res <- adjust_indemnity_liability_per_acre(
    coverage_level_percent      = 0.7,
    revenue_per_acre            = 60,
    baseline_coverage_level     = 0.8,
    baseline_liability_per_acre = 100,
    baseline_indemnity_per_acre = 0
  )
  expect_type(res, "list")
  expect_setequal(names(res), c("adj_Liability_per_acre", "adj_Indemnity_per_acre"))
  expect_length(res$adj_Liability_per_acre, 1)
  expect_length(res$adj_Indemnity_per_acre, 1)
})

test_that("covers the five mutually exclusive indemnity cases", {
  # Vectorized inputs covering cases:
  # 1) baseline > alt, baseline indemnity == 0   -> adj indm = 0
  # 2) baseline > alt, baseline indemnity >  0   -> adj indm = base indm - (base liab - adj liab)
  # 3) baseline == alt                           -> adj indm = base indm
  # 4) baseline < alt, baseline indemnity >  0   -> adj indm = adj liab - production_to_count
  # 5) baseline < alt, baseline indemnity == 0   -> adj indm = adj liab - revenue_per_acre
  baseline_cov <- c(0.80, 0.80, 0.75, 0.70, 0.70)
  alt_cov      <- c(0.70, 0.70, 0.75, 0.80, 0.80)
  base_liab    <- rep(100, 5)
  base_indm    <- c(0, 10, 5, 6, 0)
  revenue_pa   <- rep(60, 5)
  
  res <- adjust_indemnity_liability_per_acre(
    coverage_level_percent      = alt_cov,
    revenue_per_acre            = revenue_pa,
    baseline_coverage_level     = baseline_cov,
    baseline_liability_per_acre = base_liab,
    baseline_indemnity_per_acre = base_indm
  )
  
  # Expected adjusted liabilities
  exp_adj_liab <- (base_liab / baseline_cov) * alt_cov
  expect_equal(res$adj_Liability_per_acre, exp_adj_liab, tolerance = 1e-10)
  
  # production_to_count (floored at 0)
  ptc <- pmax(base_liab - base_indm, 0)
  
  # Expected adjusted indemnities by case
  exp_adj_indm <- c(
    # 1
    0,
    # 2
    base_indm[2] - (base_liab[2] - exp_adj_liab[2]),
    # 3
    base_indm[3],
    # 4
    exp_adj_liab[4] - ptc[4],
    # 5
    exp_adj_liab[5] - revenue_pa[5]
  )
  
  expect_equal(res$adj_Indemnity_per_acre, exp_adj_indm, tolerance = 1e-10)
})

test_that("recycles scalars to vector length", {
  # Scalar alt coverage & revenue recycled over vector baselines
  res <- adjust_indemnity_liability_per_acre(
    coverage_level_percent      = 0.8,       # scalar → recycle
    revenue_per_acre            = 60,        # scalar → recycle
    baseline_coverage_level     = c(0.8, 0.7),
    baseline_liability_per_acre = c(100, 120),
    baseline_indemnity_per_acre = c(0, 10)
  )
  # Check lengths
  expect_length(res$adj_Liability_per_acre, 2)
  expect_length(res$adj_Indemnity_per_acre, 2)
  
  # Spot-check first element (equal coverage → indm unchanged)
  expect_equal(res$adj_Liability_per_acre[1], 100)  # 100/0.8*0.8
  expect_equal(res$adj_Indemnity_per_acre[1], 0)
  
  # Second element (baseline < alt, baseline had indemnity > 0)
  exp_adj_liab2 <- (120 / 0.7) * 0.8
  ptc2 <- pmax(120 - 10, 0)
  expect_equal(res$adj_Liability_per_acre[2], exp_adj_liab2, tolerance = 1e-10)
  expect_equal(res$adj_Indemnity_per_acre[2], exp_adj_liab2 - ptc2, tolerance = 1e-10)
})

test_that("length mismatch throws an error", {
  expect_error(
    adjust_indemnity_liability_per_acre(
      coverage_level_percent      = c(0.7, 0.8),   # length 2
      revenue_per_acre            = 60,            # scalar ok
      baseline_coverage_level     = c(0.8, 0.8, 0.8), # length 3 → mismatch
      baseline_liability_per_acre = 100,
      baseline_indemnity_per_acre = 0
    ),
    "All inputs must have length 1 or the same length."
  )
})

test_that("guardrail warning when baseline_coverage_level <= 0", {
  expect_warning(
    adjust_indemnity_liability_per_acre(
      coverage_level_percent      = 0.7,
      revenue_per_acre            = 60,
      baseline_coverage_level     = 0,   # triggers warning
      baseline_liability_per_acre = 100,
      baseline_indemnity_per_acre = 0
    ),
    "baseline_coverage_level contains non-positive values"
  )
})

test_that("NA inputs fall through to default = NA for adjusted indemnity", {
  res <- adjust_indemnity_liability_per_acre(
    coverage_level_percent      = 0.8,
    revenue_per_acre            = 60,
    baseline_coverage_level     = 0.8,
    baseline_liability_per_acre = 100,
    baseline_indemnity_per_acre = NA_real_  # makes all fcase conditions NA
  )
  expect_true(is.na(res$adj_Indemnity_per_acre))
  # liability still computed deterministically
  expect_equal(res$adj_Liability_per_acre, 100)
})
