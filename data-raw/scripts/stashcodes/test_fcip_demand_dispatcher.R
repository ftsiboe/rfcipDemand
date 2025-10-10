test_that("dispatcher: end-to-end smoke with mocks; expected columns exist", {
  
  # Make a 'fatter' SOB only for this test: expand each raw row into 40 distinct
  # unit_structure_code groups so the ≥30 obs/year rule is satisfied post-aggregation.
  download.file_mock_many <- function(url, destfile, ..., mode = "wb", quiet = TRUE) {
    if (grepl("sobtpu_", url)) {
      yr <- as.integer(sub(".*sobtpu_(\\d{4})\\.rds.*", "\\1", url))
      dt <- .make_sob_for_year(yr)
      dt <- dt[rep(seq_len(.N), each = 40)]
      # Create 40 distinct identifier groups per original row
      dt[, unit_structure_code := rep(1:40, times = .N / 40)]
      saveRDS(dt, destfile)
      return(0L)
    }
    # Delegate all other downloads to the standard mock
    download.file_mock(url, destfile, ..., mode = mode, quiet = quiet)
  }
  
  with_mocked_bindings(
    download.file = download.file_mock_many, {
      out <- fcip_demand_data_dispatcher(
        study_years  = 2011L,
        identifiers  = .identifiers_full
      )
      
      # Should now survive finalize() support filter
      expect_s3_class(out, "data.table")
      expect_true(nrow(out) > 0)
      
      # Sanity: KS FIPS mapped
      expect_true(all(c("state_name","state_abbreviation") %in% names(out)))
      expect_true(any(out$state_abbreviation == "KS"))
      
      # Expected columns from all stages
      expect_true(all(c(
        # prep
        "coverage_level_percent_aggregate","coverage_level_percent_max","coverage_level_dominant",
        "premium_per_liability","subsidy_per_premium",
        # controls
        "price","tau","subsidy_rate_65","subsidy_rate_75","rent","index_for_price_recived",
        # reconcile
        "county_acreage",
        # finalize
        "pool","singleton","subsidy_bins","period_farmbill","period_combo",
        "state_name","state_abbreviation"
      ) %in% names(out)))
    }
  )
})
