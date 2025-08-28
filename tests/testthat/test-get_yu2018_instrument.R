# tests/testthat/test-get_yu2018_instrument.R
test_that("get_yu2018_instrument filters, rounds coverage, aggregates, and pivots", {
  library(data.table)
  
  dt <- data.table(
    commodity_year        = c(2020, 2020, 2020, 2021, 2021),
    delivery_type         = c("RBUP", "RBUP", "OTHER", "FBUP", "RBUP"),
    insurance_plan_code   = c(1, 2, 1, 3, 90),
    coverage_level_percent= c(0.65, 0.76, 0.80, 0.75, 0.651),
    subsidy_amount        = c(10, 40, 5, 30, 20),
    total_premium_amount  = c(20, 80, 5, 60, 40)
  )
  
  out <- get_yu2018_instrument(dt)
  
  # Rows = years present; columns must include the two kept levels
  expect_true(all(c("subsidy_rate_65", "subsidy_rate_75") %in% names(out)))
  expect_equal(sort(out$commodity_year), c(2020, 2021))
  
  # After filtering & rounding:
  # 2020: 0.65 => 10/20=0.5; 0.76 -> 0.75 => 40/80=0.5
  # 2021: 0.651 -> 0.65 => 20/40=0.5; 0.75 => 30/60=0.5
  setorder(out, commodity_year)
  expect_equal(out$subsidy_rate_65, c(0.5, 0.5), tolerance = 1e-12)
  expect_equal(out$subsidy_rate_75, c(0.5, 0.5), tolerance = 1e-12)
})
