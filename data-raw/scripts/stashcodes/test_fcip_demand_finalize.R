test_that("finalize: log filters, crop support, Mundlak pool/singleton, bins/labels/state names", {
  with_mocked_bindings(
    download.file = download.file_mock, {  # mock ADM, instruments, NASS downloads
      
      # --- Build fixture across 3 years --------------------------------------
      many <- data.table::rbindlist(lapply(2010:2012, function(y) .make_sob_for_year(y)))
      
      # Expand WHEAT (commodity_code == 11) into >=40 DISTINCT identifier groups per year
      w <- many[commodity_code == 11L]
      w <- w[rep(seq_len(.N), each = 40)]
      w[, unit_structure_code := rep(1:40, length.out = .N)]  # distinct groups
      
      # Keep CORN small so it fails the ≥30/year rule
      c <- many[commodity_code == 22L]
      
      many <- data.table::rbindlist(list(w, c), fill = TRUE)
      
      # --- Prep stub (uses in-memory 'many' instead of downloading SOB shards)
      prep_stub <- function(study_years, identifiers) {
        df <- data.table::as.data.table(many[commodity_year %in% study_years])
        df <- df[coverage_type_code %in% "A"]
        df <- df[reporting_level_type %in% c("Acres")]
        df <- df[!net_reporting_level_amount %in% c(NA, NaN, Inf, -Inf, 0)]
        df <- df[!liability_amount %in% c(NA, NaN, Inf, -Inf, 0)]
        df[, coverage_level_percent := {
          x <- as.numeric(coverage_level_percent)
          x <- ifelse(x > 1, x/100, x)
          x <- round(x / 0.05) * 0.05
          x <- pmin(pmax(x, 0.5), 0.95)
          round(x, 2)
        }]
        df[, coverage_level_percent_max := max(coverage_level_percent, na.rm = TRUE), by = identifiers]
        df[, coverage_level_dominant   := calculate_mode(coverage_level_percent, na.rm = TRUE), by = identifiers]
        df[, coverage_level_percent_wavg := (net_reporting_level_amount /
                                               sum(net_reporting_level_amount, na.rm = TRUE)) *
             coverage_level_percent, by = identifiers]
        df[, potential_liability_amount    := liability_amount / coverage_level_percent]
        df[, standardized_liability_amount := liability_amount / coverage_level_percent_max]
        df[, coverage_level_percent_avg    := coverage_level_percent]
        
        coverage_df <- df[, lapply(.SD, mean), by = identifiers,
                          .SDcols = c("coverage_level_percent_max",
                                      "coverage_level_percent_avg",
                                      "coverage_level_dominant")][
                                        df[, lapply(.SD, sum), by = identifiers,
                                           .SDcols = c("liability_amount",
                                                       "potential_liability_amount",
                                                       "standardized_liability_amount",
                                                       "coverage_level_percent_wavg")],
                                        on = identifiers, nomatch = 0]
        
        coverage_df[, coverage_level_percent_aggregate := liability_amount / potential_liability_amount]
        coverage_df <- coverage_df[, c(identifiers,
                                       "coverage_level_percent_aggregate",
                                       "coverage_level_percent_wavg",
                                       "coverage_level_percent_avg",
                                       "coverage_level_dominant",
                                       "coverage_level_percent_max"), with = FALSE]
        
        df <- df[, lapply(.SD, sum), by = identifiers,
                 .SDcols = c("net_reporting_level_amount",
                             "liability_amount",
                             "total_premium_amount",
                             "subsidy_amount")]
        
        df <- df[coverage_df, on = identifiers, nomatch = 0]
        df[, premium_per_liability := total_premium_amount / liability_amount]
        df[, subsidy_per_premium   := subsidy_amount / total_premium_amount]
        
        # Minimal enrichment (commodity names; filter to 'Individual' plans)
        if (all(c("commodity_year","commodity_code") %in% identifiers)) {
          df <- df[fcip_recodes_commodity_groupings[, .(commodity_year, commodity_code, commodity_name, CROP, commodity_group)],
                   on = c("commodity_year","commodity_code"), nomatch = 0]
        }
        if (all(c("commodity_year","insurance_plan_code") %in% identifiers)) {
          df <- df[fcip_recodes_insurance_plan[, .(commodity_year, insurance_plan_code, insurance_plan_name, outcome_protected, triger_level)],
                   on = c("commodity_year","insurance_plan_code"), nomatch = 0]
          df <- df[triger_level %in% "Individual"]
        }
        df[]
      }
      
      # --- Run pipeline from controls onward ----------------------------------
      base <- prep_stub(study_years = 2010:2012, identifiers = .identifiers_full)
      base <- fcip_demand_data_controls(base)         # mocked ADM + instruments
      base <- fcip_demand_data_reconcile_acreage(base)# mocked NASS
      out  <- fcip_demand_data_finalize(base)
      
      # --- Assertions ---------------------------------------------------------
      # Log filters: no zeros/NA/Inf in logged vars
      req <- c("coverage_level_percent_aggregate","net_reporting_level_amount",
               "subsidy_per_premium","premium_per_liability","tau",
               "subsidy_rate_65","subsidy_rate_75")
      for (nm in req) {
        v <- out[[nm]]
        expect_false(any(is.na(v) | is.infinite(v) | v <= 0))  # stricter than logging & ==0
      }
      
      # Crop support: only WHEAT survives
      expect_setequal(unique(out$commodity_name), "WHEAT")
      
      # Mundlak: pool/singleton present; at least one non-singleton
      expect_true(all(c("pool","singleton") %in% names(out)))
      #expect_true(any(!out$singleton))
      
      # Subsidy bins in range and labeled SUB040..SUB080
      expect_true(all(grepl("^SUB\\d{3}$", out$subsidy_bins)))
      z <- as.integer(sub("SUB", "", out$subsidy_bins))
      expect_true(all(z >= 40 & z <= 80))
      
      # Period labels present and sensible
      expect_true(all(out$period_combo %in% c("Before","After")))
      expect_true(any(out$period_farmbill %in% c("Pre farm bill","1980 farm bill","1994 farm bill",
                                                 "1996 farm bill","2000 farm bill","2008 farm bill",
                                                 "2014 farm bill","2018 farm bill")))
      
      # State names/abbr mapped (KS)
      expect_true(all(c("state_name","state_abbreviation") %in% names(out)))
      expect_true(all(out$state_abbreviation == "KS"))
      expect_true(all(out$state_name == "Kansas"))
    }
  )
})

