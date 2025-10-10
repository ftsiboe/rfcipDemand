test_that("prep: filters, coverage normalization, aggregation, ratios, enrichment gating", {
  with_mocked_bindings(
    download.file = download.file_mock, {
      # Use one year to keep it tiny
      df <- fcip_demand_data_prep_sob(
        study_years = 2011L,
        identifiers = .identifiers_full
      )
      
      # Only 'Individual' plans survive (ARPI 'Area' dropped)
      expect_true(all(df$insurance_plan_code %in% c(2L, 4L)))
      
      # Coverage normalization checks (snap, clamp, percent->prop), AFTER filtering to Individual plans.
      # For the tiny fixture, plan 3 (ARPI/Area) is dropped, so 55% no longer appears.
      # Expected: 0.52 -> 0.50; 0.97 -> 0.95
      expect_setequal(sort(unique(df$coverage_level_percent_avg)), c(0.50, 0.95))
      
      # Weighted average computed via per-row contributions then summed
      # Manually compute one group's aggregate and compare
      raw <- .make_sob_for_year(2011L)
      raw[, coverage_level_percent := {
        x <- as.numeric(coverage_level_percent)
        x <- ifelse(x > 1, x/100, x)
        x <- round(x/0.05)*0.05
        pmin(pmax(x, 0.5), 0.95)
      }]
      # Pick commodity_code == 11 AND keep only Individual plans (plan 2)
      g <- raw[commodity_code == 11 & insurance_plan_code == 2L]
      wavg_manual <- sum(g$net_reporting_level_amount * g$coverage_level_percent) /
        sum(g$net_reporting_level_amount)
      
      # pull from output
      got <- df[commodity_code == 11,
                unique(coverage_level_percent_wavg)]
      expect_equal(got, wavg_manual, tolerance = 1e-12)
      
      # Aggregate coverage = sum(liability)/sum(liability/coverage)
      cov_agg_manual <- {
        num <- sum(g$liability_amount)
        den <- sum(g$liability_amount / g$coverage_level_percent)
        num / den
      }
      got2 <- df[commodity_code == 11 & insurance_plan_code == 2L,unique(coverage_level_percent_aggregate)]
      expect_equal(got2, cov_agg_manual, tolerance = 1e-12)
      
      # Ratios present and correct for one known row
      one <- df[commodity_code == 11][1]
      expect_equal(one$premium_per_liability,
                   one$total_premium_amount / one$liability_amount)
      expect_equal(one$subsidy_per_premium,
                   one$subsidy_amount / one$total_premium_amount)
      
      # Optional enrichment: commodity/practice recodes should be present
      expect_true(all(c("commodity_name","CROP","commodity_group","irrigation_recode","organic_recode") %in% names(df)))
    })
})
