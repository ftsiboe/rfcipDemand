test_that("generate_fcip_instruments works", {
  fcip_contiguous_county <- tempfile(fileext = ".rds")
  download.file(
    paste0("https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/adm_extracts/fcip_contiguous_county.rds"),
    fcip_contiguous_county, mode = "wb", quiet = TRUE)
  fcip_contiguous_county <- readRDS(fcip_contiguous_county)
  
  
  sobcov_all <- tempfile(fileext = ".rds")
  download.file(
    "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/sob/sobcov_all.rds",
    sobcov_all, mode = "wb", quiet = TRUE)
  sobcov_all <- readRDS(sobcov_all)
  sob <- sobcov_all[, .(
    insured_area     = sum(net_reported_quantity, na.rm = TRUE),
    liability_amount = sum(liability_amount, na.rm = TRUE),
    indemnity_amount = sum(indemnity_amount, na.rm = TRUE)), 
    by = c("commodity_year","state_code","state_abbreviation",
           "county_code","county_name","commodity_code","commodity_name")]
  
  # Calculate Loss Cost Ratio (LCR) for risk assessment
  sob[, lcr := indemnity_amount / liability_amount]
  sob <- sob[commodity_code %in% 41 & state_code %in% 38]
  # Estimate FCIP Instrumental Variables (Unloaded Rates)
  instruments <- estimate_fcip_instruments(statplan = sob,year=2015)
  
  # merge Instrument (i.e., target rate) aggregated directly from RMA’s actuarial data master 
  adm <- tempfile(fileext = ".rds")
  download.file(
    "https://github.com/ftsiboe/USFarmSafetyNetLab/releases/download/adm_extracts/fcip_aph_base_rate.rds",
    adm, mode = "wb", quiet = TRUE)
  adm <- readRDS(adm)
  adm <- as.data.table(adm)
  
  instruments <- merge( instruments,adm, by= intersect(names(instruments), names(adm)), all  = TRUE)
  instruments <- as.data.table(instruments)
  
  # formulate and merge national subsidy rate instrument as described by (Yu et al., 2018)
  instrument_yu2018 <- get_yu2018_instrument(sobcov_all)
  
  instruments <- merge(instruments,instrument_yu2018,
                       by  = intersect(names(instruments), names(instrument_yu2018)),
                       all = TRUE)
  instruments <- as.data.table(instruments)
  # tau_final: Same as tau_adm with missing data filled in with tau_sob (as is). 
  instruments[, tau_final := tau_adm] 
  instruments[is.na(tau_adm) | !is.finite(tau_adm) | tau_adm == 0, tau_final := tau_sob]
  
  instruments <- merge(
    unique( sob[
      , .SD, .SDcols = 
        c("commodity_year","state_abbreviation","state_code","county_name","county_code",
          "commodity_name","commodity_code")]),instruments,
    by  = c("commodity_year","state_code","county_code","commodity_code"),
    all = TRUE)
  
  instruments <- instruments[
    , .SD, .SDcols = 
      c( "commodity_year","state_abbreviation","state_code","county_name","county_code",
         "commodity_name","commodity_code","tau_sob","tau_adm","tau_final",
         "subsidy_rate_65","subsidy_rate_75")]
  
  instruments <- instruments[!is.na(tau_final) & is.finite(tau_final) & tau_final != 0]
  
  expect_true(nrow(instruments) > 0)
  
  expect_true(all(c("tau_sob","tau_adm","tau_final","subsidy_rate_65","subsidy_rate_75")
                  %in% names(instruments)))
})
