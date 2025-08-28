test_that("controls: price merge + imputation, instrument tau fallback, rents/index attach", {
  with_mocked_bindings(
    download.file = download.file_mock, {
      # Start from prep to ensure keys exist (no need to mock usmap here)
      base <- fcip_demand_data_prep_sob(
        study_years = 2011L,
        identifiers = .identifiers_full
      )
      df <- fcip_demand_data_controls(base)
      
      # Price: projected NA -> harvest fallback; otherwise projected used
      w_row <- df[commodity_name == "WHEAT" & practice_code == 1L][1]
      c_row <- df[commodity_name == "CORN"  & practice_code == 2L][1]
      expect_equal(w_row$price, 6.0)   # fallback
      expect_equal(c_row$price, 5.2)   # projected
      
      # Imputation: if any NA remained, it should be resolved within (yr, state, crop, type, practice) then fallback to (yr, state, crop, type)
      expect_false(any(is.na(df$price)))
      
      # Instruments: tau uses tau_adm if available else tau_sob
      expect_equal(df[commodity_code == 11L, unique(tau)], 0.10)
      expect_equal(df[commodity_code == 22L, unique(tau)], 0.15)
      
      # Subsidy benchmark rates present
      expect_true(all(c("subsidy_rate_65","subsidy_rate_75") %in% names(df)))
      
      # State rents & NASS index attached (inner-like join via intersect)
      expect_true("rent" %in% names(df))
      expect_true("index_for_price_recived" %in% names(df))
      
      expect_true(all(c("WHEAT","CORN") %in% df$commodity_name))
    })
})
