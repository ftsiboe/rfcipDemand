#' Prep SOBTPU and compute coverage/financial aggregates for demand estimation
#'
#' @description
#' Loads RMA Summary of Business (SOBTPU) data, filters valid observations, normalizes
#' coverage levels, computes coverage summaries, and collapses financials at a chosen
#' identifier grain. Optionally enriches with recode tables depending on which keys are
#' present in `identifiers`.
#'
#' @section What this stage produces:
#' - Coverage metrics: `coverage_level_percent_{max,avg,dominant,wavg,aggregate}`
#' - Financial totals: `net_reporting_level_amount`, `liability_amount`,
#'   `total_premium_amount`, `subsidy_amount`
#' - Ratios: `premium_per_liability`, `subsidy_per_premium`
#'
#' @section Data source:
#' Downloads a released `.rds`:
#' `USFarmSafetyNetLab/sob/sobtpu_all.rds` (GitHub Releases).
#'
#' @param study_years Integer vector of commodity years to include.
#'   Defaults to `2001:(as.numeric(format(Sys.Date(), "%Y")) - 1)`.
#' @param identifiers Character vector of grouping keys that define the aggregation grain.
#'   Must be columns present in SOBTPU (e.g., `"commodity_year"`, `FCIP_INSURANCE_POOL`,
#'   `"insurance_plan_code"`, `"unit_structure_code"`, and-if desired-additional keys like
#'   `"commodity_code"` or `"practice_code"`). Enrichment joins for recodes are performed
#'   only when the required keys are included in `identifiers` (see details below).
#'
#' @details
#' **Normalization of coverage**: values > 1 are treated as percentages and converted to
#' proportions, snapped to a 0.05 grid, and clamped to 0.50:0.95.
#'
#' **Aggregation**:
#' - Per-identifier coverage summaries: max, mean, mode (dominant), and weighted average
#'   (weights = `net_reporting_level_amount`).
#' - `potential_liability_amount = liability_amount / coverage_level_percent`
#' - `coverage_level_percent_aggregate = liability_amount / potential_liability_amount`
#'
#' **Optional enrichment** (requires certain keys in `identifiers`):
#' - Commodity grouping (`commodity_year + commodity_code`)
#' - Practice recodes (`commodity_year + commodity_code + practice_code`)
#' - Insurance plan recodes (`commodity_year + insurance_plan_code`, filtered to
#'   `triger_level == "Individual"`)
#'
#' **Environment requirements** (for optional enrichment):
#' - `fcip_recodes_commodity_groupings`, `fcip_recodes_practice`,
#'   `fcip_recodes_insurance_plan`.
#'
#' @return A `data.table` at the chosen identifier grain with coverage aggregates and the
#'   columns listed under **What this stage produces**.
#' @family Estimation Data
#' @import data.table
#' @importFrom utils download.file
#' @keywords internal
#' @noRd
fcip_demand_data_prep_sob <- function(
    study_years = 2001:(as.numeric(format(Sys.Date(), "%Y")) - 1),
    identifiers = c("commodity_year",FCIP_INSURANCE_POOL,"insurance_plan_code","unit_structure_code")
) {
  
  stopifnot(length(study_years) > 0)
  
  temporary_dir <- tempdir()
  
  # Download SOBTPU release 
  piggyback::pb_download(
    file = "sobtpu_all.rds",
    dest = temporary_dir,
    repo = "ftsiboe/USFarmSafetyNetLab",
    tag  = "sob",
    overwrite = TRUE)
  df <- readRDS(file.path(temporary_dir,"sobtpu_all.rds"))
  data.table::setDT(df)

  # Base filters 
  df <- df[commodity_year %in% study_years]
  df <- df[coverage_type_code %in% "A"]
  df <- df[reporting_level_type %in% c("Acres")]
  df <- df[!net_reporting_level_amount %in% c(NA, NaN, Inf, -Inf, 0)]
  df <- df[!liability_amount %in% c(NA, NaN, Inf, -Inf, 0)]
  
  # Normalize coverage_level_percent
  df[, coverage_level_percent := {
    x <- as.numeric(coverage_level_percent)
    x <- fifelse(x > 1, x/100, x)       # convert % to proportion
    x <- round(x / 0.05) * 0.05         # snap to nearest 0.05
    x <- pmin(pmax(x, 0.5), 0.95)       # clamp to [0.50, 0.95]
    round(x, 2)                         # remove float artifacts
  }] 
  
  # Group-level coverage stats
  mode <- function(x, na.rm = TRUE) {
    if (na.rm) x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  df[, coverage_level_percent_max  := max(coverage_level_percent, na.rm = TRUE), by = identifiers]
  df[, coverage_level_dominant     := mode(coverage_level_percent, na.rm = TRUE), by = identifiers]
  df[, coverage_level_percent_wavg := (net_reporting_level_amount/sum(net_reporting_level_amount, na.rm = TRUE))*coverage_level_percent, by = identifiers]
  
  # Liability-derived measures 
  df[, potential_liability_amount    := liability_amount/coverage_level_percent]
  df[, standardized_liability_amount := liability_amount/coverage_level_percent_max]
  df[, coverage_level_percent_avg    := coverage_level_percent]
  
  # Combine coverage summaries (means + sums) 
  coverage_df <- df[, lapply(.SD, mean), by = identifiers,
                    .SDcols = c("coverage_level_percent_max","coverage_level_percent_avg","coverage_level_dominant")][
                      df[, lapply(.SD, sum), by = identifiers,
                         .SDcols = c("liability_amount","potential_liability_amount","standardized_liability_amount","coverage_level_percent_wavg")],
                      on = identifiers, nomatch = 0]
  
  coverage_df[, coverage_level_percent_aggregate := liability_amount/potential_liability_amount]
  coverage_df <- coverage_df[, c(identifiers,
                                 "coverage_level_percent_aggregate","coverage_level_percent_wavg",
                                 "coverage_level_percent_avg","coverage_level_dominant",
                                 "coverage_level_percent_max"), with = FALSE]
  
  # Collapse financials to the same grain
  df <- df[, lapply(.SD, sum), by = identifiers,
           .SDcols = c("net_reporting_level_amount","liability_amount","total_premium_amount","subsidy_amount")]
  
  df <- df[coverage_df, on = identifiers, nomatch = 0]
  rm(coverage_df); gc()
  
  # Ratios
  df[, premium_per_liability := total_premium_amount/liability_amount]
  df[, subsidy_per_premium   := subsidy_amount/total_premium_amount]
  
  # Optional: enrich with characteristics (only if keys are present) 
  if(all(c("commodity_year","commodity_code") %in% identifiers)){
    df <- df[fcip_recodes_commodity_groupings[, c("commodity_year","commodity_code","commodity_name","CROP","commodity_group"), with = FALSE],
             on = c("commodity_year","commodity_code"), nomatch = 0]
  }
  
  if(all(c("commodity_year","commodity_code","practice_code") %in% identifiers)){
    df <- df[fcip_recodes_practice[, c("commodity_year","commodity_code","practice_code","irrigation_recode","organic_recode"), with = FALSE],
             on = c("commodity_year","commodity_code","practice_code"), nomatch = 0]
  }
  
  if(all(c("commodity_year","insurance_plan_code") %in% identifiers)){
    df <- df[fcip_recodes_insurance_plan[, c("commodity_year","insurance_plan_code","insurance_plan_name","outcome_protected","triger_level"), with = FALSE],
             on = c("commodity_year","insurance_plan_code"), nomatch = 0]
    df <- df[triger_level %in% "Individual"]
  }
  
  df[]
}


#' Add recodes, prices, instruments, rental rates, and price index
#'
#' @description
#' Adds commodity prices (ADM; projected price with fallback to harvest price),
#' instruments (`tau` plus benchmark subsidy rates 65/75), state-level land rental rates,
#' and NASS "index for price received". Also performs within-group price imputation to fill
#' sparse county-year * crop-type-practice cells.
#'
#' @section Data sources:
#' - ADM price release: `adm_extracts/fcip_commodity_price.rds`
#' - Instrument release: `reps/fcip_demand_instruments.rds` (uses `tau_adm`, fallback `tau_sob`)
#' - Assumes in-memory tables: `nassSurveyRentalRates`, `nassSurveyPriceRecivedIndex`
#'
#' @param df A `data.table` produced by `fcip_demand_data_prep_sob()`.
#'
#' @return The same `data.table` with added columns:
#'   `price`, `tau`, `subsidy_rate_65`, `subsidy_rate_75`, `rent`, `index_for_price_recived`.
#'
#' @note Column name `index_for_price_recived` follows the source spelling.
#' @family Estimation Data
#' @import data.table
#' @importFrom utils download.file
#' @keywords internal
#' @noRd
fcip_demand_data_controls <- function(df) {
  
  temporary_dir <- tempdir()
  
  df <- data.table::as.data.table(df)
  
  # ADM commodity price
  piggyback::pb_download(
    file = "fcip_commodity_price.rds",
    dest = temporary_dir,
    repo = "ftsiboe/USFarmSafetyNetLab",
    tag  = "sob",
    overwrite = TRUE)
  adm_price <- readRDS(file.path(temporary_dir,"fcip_commodity_price.rds"))
  data.table::setDT(adm_price)
  adm_price <- adm_price[, lapply(.SD, mean), by = intersect(names(df),names(adm_price)), .SDcols = c("projected_price","harvest_price")]
  df <- merge(df,adm_price,by = intersect(names(df),names(adm_price)),all.x = TRUE)
  rm(adm_price);gc()
  
  # Price imputation 
  # (1) Using the data from the RMA sources above, the expected price was
  # first taken as the projected price, if unavailable the harvest price is considered. 
  df[,price := ifelse(projected_price %in% c(NA,0,Inf,-Inf,NaN),harvest_price,projected_price)]
  df[price %in% c(NA, 0, Inf, -Inf, NaN), price := NA]
  
  # (2) The missing expected price that persists is replaced with averages within groups
  tryCatch({ 
    df[price %in% c(NA, 0, Inf, -Inf, NaN), price := mean(price, na.rm = TRUE),
       by = c("commodity_year","state_code","commodity_name","type_code","practice_code")]
    df[price %in% c(NA, 0, Inf, -Inf, NaN), price := NA]
  }, error = function(e){NULL})
  
  # (3) The missing expected price that persists is replaced with averages within groups
  tryCatch({ 
    df[price %in% c(NA, 0, Inf, -Inf, NaN), price := mean(price, na.rm = TRUE),
       by = c("commodity_year","state_code","commodity_name","type_code")]
    df[price %in% c(NA, 0, Inf, -Inf, NaN), price := NA]
  }, error = function(e){NULL})
  
  # (4) The missing expected price that persists is replaced with averages within groups
  tryCatch({ 
    df[price %in% c(NA, 0, Inf, -Inf, NaN), price := mean(price, na.rm = TRUE),
       by = c("commodity_year","state_code","commodity_name")]
    df[price %in% c(NA, 0, Inf, -Inf, NaN), price := NA]
  }, error = function(e){NULL})
  
  # (5) Normalize price to sample mean by commodity
  tryCatch({ 
    df[, price := price/mean(price, na.rm = TRUE), by = c("commodity_code")]
  }, error = function(e){NULL})
  
  df <- df[, c("projected_price","harvest_price") := NULL]
  
  # Instruments (tau and benchmark subsidy rates)
  piggyback::pb_download(
    file = "fcip_commodity_price.rds",
    dest = temporary_dir,
    repo = "ftsiboe/USFarmSafetyNetLab",
    tag  = "reps",
    overwrite = TRUE)
  fcip_instruments <- readRDS(file.path(temporary_dir,"fcip_demand_instruments.rds"))
  data.table::setDT(fcip_instruments)
  fcip_instruments[,tau := tau_adm]
  fcip_instruments[tau %in% c(NA,Inf,-Inf,NaN,0),tau := tau_sob]
  
  df <- df[fcip_instruments[, c("commodity_year","state_code","county_code","commodity_code","tau","subsidy_rate_65","subsidy_rate_75"), with = FALSE], 
           on = c("commodity_year","state_code","county_code","commodity_code"), nomatch = 0]
  rm(fcip_instruments);gc()
  
  # Per-acre cost of crop production was approximated with state-level rental rates retrieved from NASS Quick Stats.
  df <- df[nassSurveyRentalRates[, lapply(.SD, mean), by = intersect(names(df), names(nassSurveyRentalRates)), .SDcols = "rent"],
           on = intersect(names(df), names(nassSurveyRentalRates)), nomatch = 0]
  
  # NASS index for price received
  df <- df[nassSurveyPriceRecivedIndex[, lapply(.SD, mean), by = intersect(names(df), names(nassSurveyPriceRecivedIndex)), .SDcols = "index_for_price_recived"],
           on = intersect(names(df), names(nassSurveyPriceRecivedIndex)), nomatch = 0]
  
  # Convert to real terms
  df[, price := price/index_for_price_recived]
  df[, rent  := rent/index_for_price_recived]
  
  df[]
}


#' Reconcile county acreage from FSA and NASS
#'
#' @description
#' Builds county-year planted acres from FSA (`fsaCropAcreageCC` joined via `fsa_crop_linker`)
#' and merges NASS county series (planted, bearing, harvested). Sets `county_acreage`
#' choosing the first non-missing in the order: FSA planted - NASS planted - NASS bearing - NASS harvested. Intermediate columns are dropped.
#'
#' @section Data sources:
#' - Package data: `fsaCropAcreageCC` (loaded via `data(fsaCropAcreageCC)`)
#' - Linker: `fsa_crop_linker` (columns: `crop_cd_fsa`, `crop`, `crop_yr`)
#' - Release download: `nass_extracts/nass_production_data.rds`
#'
#' @param df A `data.table` from `fcip_demand_data_controls()'.
#'
#' @return The same `data.table` with a single `county_acreage` column and without
#'   `nassSurvey_AREA_*` or `fsa_planted_acres` intermediates.
#' @family Estimation Data
#' @import data.table rfsa
#' @importFrom utils data
#' @importFrom tidyr spread
#' @keywords internal
#' @noRd
fcip_demand_data_reconcile_acreage <- function(df){
  
  temporary_dir <- tempdir()
  df <- data.table::as.data.table(df)
  
  # Build FSA planted acres by county-year
  crop_linker <- unique(as.data.frame(fsa_crop_linker)[c("crop_cd_fsa","crop_rma","crop_yr")])
  names(crop_linker) <- c("crop_cd","commodity_name","crop_yr")
  crop_linker <- data.table::as.data.table(crop_linker)
  crop_linker <- crop_linker[!crop_cd %in% NA]

  piggyback::pb_download(
    file = "fsaCropAcreageData.rds",
    dest = temporary_dir,
    repo = "ftsiboe/USFarmSafetyNetLab",
    tag  = "fsa_extracts",
    overwrite = TRUE)
  fsa <- readRDS(file.path(temporary_dir,"fsaCropAcreageData.rds"))
  data.table::setDT(fsa)

  fsa <- fsa[crop_linker, on = c("crop_yr","crop_cd"), nomatch = 0,allow.cartesian=TRUE]
  fsa[,fips := stringr::str_pad(fips,pad="0",5)]
  fsa <- as.data.table(tidyr::separate(fsa,"fips",into=c("state_code","county_code"),sep=2))
  fsa[,state_code := as.numeric(as.character(state_code))]
  fsa[,county_code := as.numeric(as.character(county_code))]
  fsa <- fsa[, .(fsa_planted_acres = sum(planted_acres, na.rm = TRUE)),
             by = .(crop_yr, state_code, county_code, commodity_name)]
  data.table::setnames(fsa,old = c("crop_yr"),new = c("commodity_year"))
  
  # unique(fsa$commodity_name)
  # unique(df$commodity_code)
  
  #table(fsa$commodity_name,fsa$commodity_year)
  
  df <- merge(df, fsa, by = c("commodity_year","state_code","county_code","commodity_name"), all.x = TRUE)
  rm(fsa,crop_linker); gc()
  
  # NASS county series (planted/bearing/harvested)
  
  piggyback::pb_download(
    file = "nassSurveyCropProductionData.rds",
    dest = temporary_dir,
    repo = "ftsiboe/USFarmSafetyNetLab",
    tag  = "nass_extracts",
    overwrite = TRUE)
  nass <- readRDS(file.path(temporary_dir,"nassSurveyCropProductionData.rds"))
  data.table::setDT(nass)

  nass <- nass[agg_level_desc %in% "COUNTY" &
                 statisticcat_desc %in% c("nassSurvey_AREA_HARVESTED","nassSurvey_AREA_PLANTED","nassSurvey_AREA_BEARING"),
               .(value = sum(value, na.rm = TRUE)),
               by = .(commodity_year, state_code, county_code, commodity_name, statisticcat_desc)]
  
  nass <- nass |> tidyr::spread(statisticcat_desc, value)
  nass <- data.table::as.data.table(nass)
  nass$commodity_name <- toupper(as.character(nass$commodity_name))
  df <- merge(df, nass,
              by = c("commodity_year","state_code","county_code","commodity_name"),
              all.x = TRUE)
  rm(nass); gc()
  
  # Ensure NASS columns exist
  needed <- c("nassSurvey_AREA_PLANTED", "nassSurvey_AREA_BEARING", "nassSurvey_AREA_HARVESTED", "fsa_planted_acres")
  for (nm in needed) if (!nm %in% names(df)) df[, (nm) := NA_real_]
  df[, (needed) := lapply(.SD, as.numeric), .SDcols = needed]
  
  bad2na <- function(v) { v[is.na(v) | !is.finite(v) | v == 0] <- NA_real_; v }
  df[, county_acreage := data.table::fcoalesce(
    bad2na(fsa_planted_acres),
    bad2na(nassSurvey_AREA_PLANTED),
    bad2na(nassSurvey_AREA_BEARING),
    bad2na(nassSurvey_AREA_HARVESTED)
  )]
  
  # Drop intermediates
  df <- df[, c("nassSurvey_AREA_HARVESTED","nassSurvey_AREA_PLANTED","nassSurvey_AREA_BEARING","fsa_planted_acres") := NULL]
  
  df[]
}


#' Finalize FCIP demand dataset
#'
#' @description
#' Applies required log-based validity filters, enforces crop support thresholds, creates
#' Mundlak pooling identifiers, forms subsidy-share bins, attaches Farm Bill period labels,
#' derives a simple before/after indicator, and adds state names/abbreviations.
#'
#' @details
#' - **Filters**: drops rows where logs of key variables are `0`, `NA`, `Inf`, or `-Inf`.
#' - **Support thresholds**: keep crops with >= 30 obs per year for >= 10 years.
#' - **Mundlak pooling**: produces `pool` and a `singleton` flag per cross-sectional key.
#' - **Binning**: `subsidy_bins` in 0.02 steps from 0.40 to 0.80 (inclusive, with clamp).
#' - **Labels**: `period_farmbill` factor (pre-1980 ... 2018) and `period_combo` ("Before"/"After" 2012).
#'
#' @param df A `data.table` from fcip_demand_data_reconcile_acreage().
#'
#' @return Final `data.table` ready for estimation.
#' @family Estimation Data
#' @import data.table
#' @importFrom stats na.omit
#' @importFrom usmap fips_info
#' @keywords internal
#' @noRd
fcip_demand_data_finalize <- function(df){
  
  df <- data.table::as.data.table(df)
  
  # Required log filters 
  df <- df[!log(coverage_level_percent_aggregate)  %in% c(0, NA, Inf, -Inf, NaN)]
  df <- df[!log(net_reporting_level_amount)        %in% c(0, NA, Inf, -Inf, NaN)]
  df <- df[!log(subsidy_per_premium)               %in% c(0, NA, Inf, -Inf, NaN)]
  df <- df[!log(premium_per_liability)             %in% c(0, NA, Inf, -Inf, NaN)]
  df <- df[!log(tau)                               %in% c(0, NA, Inf, -Inf, NaN)]
  df <- df[!log(subsidy_rate_65)                   %in% c(0, NA, Inf, -Inf, NaN)]
  df <- df[!log(subsidy_rate_75)                   %in% c(0, NA, Inf, -Inf, NaN)]
  df <- df[!log(county_acreage)                    %in% c(0, NA, Inf, -Inf, NaN)]
  
  # Crop support thresholds 
  available_years <- length(unique(df$commodity_year))
  croplist <- df[, .(n = length(net_reporting_level_amount)), by = .(commodity_year, commodity_name)][
    n >= 30L, .(n = length(n)), by = .(commodity_name)][n >= pmin(available_years,10)]
  df <- df[commodity_name %in% croplist$commodity_name]
  rm(croplist); gc()
  
  # Mundlak pooling identifiers
  keys <- c("state_code","county_code","commodity_code","type_code","practice_code","insurance_plan_code","unit_structure_code")
  keys <- names(df)[names(df) %in% keys]
  mundlak <- df[, .(singleton = length(commodity_year) %in% 1), by = keys]
  mundlak[, pool := .I]
  df <- df[mundlak, on = intersect(names(df), names(mundlak)), nomatch = 0]
  rm(mundlak); gc()
  
  df <- na.omit(df, cols = c("pool","commodity_year","net_reporting_level_amount",
                             "coverage_level_percent_aggregate","subsidy_per_premium",
                             "premium_per_liability","tau","subsidy_rate_65","subsidy_rate_75"))
  
  # Subsidy bins
  step <- 0.02; lo <- 0.40; hi <- 0.80
  df[, subsidy_bins := {
    x <- subsidy_per_premium
    x <- pmin(pmax(x, lo), hi)
    b <- lo + floor((x - lo + 1e-12) / step) * step
    ifelse(is.na(b), NA_character_, sprintf("SUB%03d", round(b * 100)))
  }]
  
  # Farm Bill labels and period combo
  df[, period_farmbill := fcase(
    commodity_year < 1980, 0L,
    commodity_year >= 1980 & commodity_year < 1994, 1L,
    commodity_year >= 1994 & commodity_year < 1996, 2L,
    commodity_year >= 1996 & commodity_year < 2000, 3L,
    commodity_year >= 2000 & commodity_year < 2008, 4L,
    commodity_year >= 2008 & commodity_year < 2014, 5L,
    commodity_year >= 2014 & commodity_year < 2018, 6L,
    commodity_year >= 2018, 7L,
    default = NA_integer_
  )]
  labs <- c("Pre farm bill","1980 farm bill","1994 farm bill","1996 farm bill",
            "2000 farm bill","2008 farm bill","2014 farm bill","2018 farm bill")
  df[, `:=`(
    period_farmbill = labs[as.integer(period_farmbill) + 1L],
    period_combo = fcase(commodity_year >= 2012, "After",
                         commodity_year <  2012, "Before",
                         default = NA_character_)
  )]
  
  # State names/abbreviations
  states <- data.table::as.data.table(usmap::fips_info(unique(df$state_code)))
  states[, fips := as.numeric(as.character(fips))]
  data.table::setnames(states, old = c("abbr","fips","full"),
                       new = c("state_abbreviation","state_code","state_name"))
  df <- merge(df, states, by = "state_code", all.x = TRUE)
  rm(states); gc()
  
  df[]
}


#' Build dataset to estimate Federal Crop Insurance Program (FCIP) demand (modular pipeline)
#'
#' @description
#' End-to-end pipeline that:
#' (1) prepares SOBTPU and coverage aggregates,
#' (2) adds prices/instruments/rental rates/price index,
#' (3) reconciles county acreage (FSA + NASS), and
#' (4) finalizes bins/labels/pooling for demand estimation.
#'
#' @details
#' Aligned with Asche, Bekkerman, & Li (2023), *Food Policy*, 119(3):102505
#' (\doi{10.1016/j.foodpol.2023.102505}). Requires internet access to download
#' release `.rds` assets and several in-memory lookup tables (see stage docs).
#'
#' @param study_years Integer vector of commodity years to include.
#'   Defaults to `2001:(as.numeric(format(Sys.Date(), "%Y")) - 1)`.
#' @param identifiers Character vector of grouping keys that define the aggregation grain.
#'   Must be columns present in SOBTPU (e.g., `"commodity_year"`, `FCIP_INSURANCE_POOL`,
#'   `"insurance_plan_code"`, `"unit_structure_code"`, and-if desired-additional keys like
#'   `"commodity_code"` or `"practice_code"`). Enrichment joins for recodes are performed
#'   only when the required keys are included in `identifiers`
#' 
#' @return A `data.table` ready for FCIP demand estimation.
#' @family Estimation Data
#' @export
fcip_demand_data_dispatcher <- function(
    study_years = 2001:(as.numeric(format(Sys.Date(), "%Y")) - 1),
    identifiers = c("commodity_year",FCIP_INSURANCE_POOL,"insurance_plan_code","unit_structure_code")
) {
  df <- fcip_demand_data_prep_sob(study_years = study_years, identifiers = identifiers)
  df <- fcip_demand_data_controls(df)
  df <- fcip_demand_data_reconcile_acreage(df)
  df <- fcip_demand_data_finalize(df)
  df[]
}
