# tests/testthat/test-format_fcip_demand_table.R
library(testthat)
library(dplyr)
library(tibble)

test_that("format_fcip_demand_table errors when required columns are missing", {
  df_bad <- tibble::tibble(
    demand   = "theta",
    coef     = "tilda_rate",
    Estimate = -0.12,
    StdError = 0.03
    # Pvalue missing -> should error
  )
  expect_error(format_fcip_demand_table(df_bad, var_labels = c()))
})

test_that("format_fcip_demand_table formats, labels, and orders panels correctly (lower-case demand keys)", {
  # synthetic input covering all panels/blocks + star thresholds
  df <- tibble::tibble(
    demand   = c(
      # theta
      rep("theta", 2),
      # gamma
      rep("gamma", 2),
      # total
      "total",
      # Covariance matrix keys (live outside demand panels)
      rep(NA_character_, 3),
      # Additional statistics
      rep(NA_character_, 4)
    ),
    coef     = c(
      # theta
      "tilda_rate", "alpha",
      # gamma
      "tilda_rate", "beta",
      # total
      "tilda_rate",
      # Covariance
      "residCov_11", "residCov_22", "residCov_12",
      # Other stats
      "N", "NFE", "JTest", "FTest"
    ),
    Estimate = c(
      # theta
      -0.12345, 0.5001,
      # gamma
      -0.2002, 1.234,
      # total
      -0.3009,
      # Covariance
      0.111, 0.222, 0.333,
      # Other stats (scalars: SE NA)
      1000, 25, 1.2, 15.678
    ),
    StdError = c(
      # theta
      0.045, 0.050,
      # gamma
      0.100, 0.200,
      # total
      0.120,
      # Covariance
      NA, NA, NA,
      # Other stats
      NA, NA, NA, NA
    ),
    Pvalue = c(
      # theta: *** and no-stars (p = NA)
      0.009, NA,
      # gamma: * and **
      0.060, 0.020,
      # total: no-stars
      0.200,
      # Covariance (ignored)
      NA, NA, NA,
      # Other stats (ignored)
      NA, NA, NA, NA
    )
  )
  
  var_labels <- c(
    # panel variables
    tilda_rate = "Instrumented rate",
    alpha      = "Constant (Theta)",
    beta       = "Constant (Gamma)",
    # covariance block
    residCov_11 = "Var(Gamma)",
    residCov_22 = "Var(Theta)",
    residCov_12 = "Cov(Gamma,Theta)",
    # other stats
    N     = "Observations",
    NFE   = "FE groups",
    JTest = "Hansen J",
    FTest = "1st-stage F"
  )
  
  out <- format_fcip_demand_table(df, var_labels)
  
  # shape
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("Variables", "Estimates"))
  
  # exact panel order and headers (ignore names and be robust to case)
  expected_vars <- c(
    "Coverage level",
    "Instrumented rate", "Constant (Theta)",
    "Insured acres",
    "Instrumented rate", "Constant (Gamma)",
    "Total protection response",
    "Instrumented rate",
    "Covariance matrix",
    "Var(Gamma)", "Var(Theta)", "Cov(Gamma,Theta)",
    "Additional statistics",
    "Observations", "FE groups", "Hansen J", "1st-stage F"
  )
  expect_equal(tolower(unname(out$Variables)), tolower(unname(expected_vars)))
  
  # number of rows: 17
  expect_equal(nrow(out), 17)
  
  # formatting & stars
  # theta tilda_rate: *** at p=0.009
  expect_equal(out$Estimates[out$Variables == "Instrumented rate"][1], "-0.123*** (0.045)")
  # theta constant: no stars (p = NA but SE present => parentheses shown)
  expect_equal(out$Estimates[out$Variables == "Constant (Theta)"], "0.500 (0.050)")
  # gamma tilda_rate: * at p = 0.060
  expect_equal(out$Estimates[out$Variables == "Instrumented rate"][2], "-0.200* (0.100)")
  # gamma constant: ** at p = 0.020
  expect_equal(out$Estimates[out$Variables == "Constant (Gamma)"], "1.234** (0.200)")
  
  # header estimates should be empty strings (allow "" or NA defensively)
  header_idxs <- which(tolower(out$Variables) %in% tolower(c(
    "Coverage level", "Insured acres", "Total protection response",
    "Covariance matrix", "Additional statistics"
  )))
  expect_true(all(is.na(out$Estimates[header_idxs]) | out$Estimates[header_idxs] == ""))
  
  # The entry right after the Total header is the total panel's tilda_rate row
  total_hdr_idx <- which(tolower(out$Variables) == "total protection response")
  expect_length(total_hdr_idx, 1)
  total_idx <- total_hdr_idx + 1
  expect_equal(out$Estimates[total_idx], "-0.301 (0.120)")
  
  # Covariance & other stats: SE is NA -> plain number, 3 decimals
  expect_equal(out$Estimates[out$Variables == "Var(Gamma)"], "0.111")
  expect_equal(out$Estimates[out$Variables == "Var(Theta)"], "0.222")
  expect_equal(out$Estimates[out$Variables == "Cov(Gamma,Theta)"], "0.333")
  expect_equal(out$Estimates[out$Variables == "Observations"], "1000.000")
  expect_equal(out$Estimates[out$Variables == "FE groups"], "25.000")
  expect_equal(out$Estimates[out$Variables == "Hansen J"], "1.200")
  expect_equal(out$Estimates[out$Variables == "1st-stage F"], "15.678")
})
