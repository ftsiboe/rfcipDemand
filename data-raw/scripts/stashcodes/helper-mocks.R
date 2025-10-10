# Helper + fixtures for all tests ---------------------------------------------

library(testthat)
library(data.table)

# Minimal mode helper used by prep step
calculate_mode <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Global lookups referenced by the pipeline (tiny, deterministic)
fcip_recodes_commodity_groupings <- data.table(
  commodity_year = rep(2010:2012, each = 2),
  commodity_code = c(11L, 22L, 11L, 22L, 11L, 22L),
  commodity_name = c("WHEAT","CORN","WHEAT","CORN","WHEAT","CORN"),
  CROP = c("Wheat","Corn","Wheat","Corn","Wheat","Corn"),
  commodity_group = c("Grain","Grain","Grain","Grain","Grain","Grain")
)

fcip_recodes_practice <- data.table(
  commodity_year = rep(2010:2012, each = 4),
  commodity_code = rep(c(11L,22L), times = 6),
  practice_code  = rep(c(1L, 2L), times = 6),
  irrigation_recode = rep(c("NIRR","IRR"), times = 6),
  organic_recode    = rep(c("CONV","ORG"), times = 6)
)

fcip_recodes_insurance_plan <- data.table(
  commodity_year = rep(2010:2012, each = 3),
  insurance_plan_code = c(2L, 3L, 4L, 2L, 3L, 4L, 2L, 3L, 4L),
  insurance_plan_name = c("YP","ARPI","RP"),
  outcome_protected   = "Yield",
  triger_level        = c("Individual","Area","Individual") # note: typo 'triger' matches source
)

nass_state_rental_rates <- data.table(
  commodity_year = 2010:2012,
  state_code = 20L,                 # KS
  rent = c(40, 42, 44)
)

nass_index_for_price_recived <- data.table(
  commodity_year = rep(2010:2012, each = 2),
  commodity_name = rep(c("WHEAT","CORN"), times = 3),
  index_for_price_recived = c(100, 98, 105, 99, 110, 101)
)

# FSA acreage + linker (tiny and tidy)
fsa_crop_linker <- data.table(
  crop_cd_fsa = c(111, 222),
  crop       = c("WHEAT","CORN"),
  crop_yr    = c(2011, 2011)
)

# rfsa::fsaCropAcreage expected via utils::data(); provide an in-memory twin
  fsaCropAcreage <- data.frame(
      crop_yr        = c(2011L, 2011L),
      state_cd       = c(20L, 20L),
      county_cd      = c(45L, 45L),         # use plain integer (no leading zero literal)
      crop_cd        = c(111L, 222L),
      planted_acres  = c(1000, NA_real_),
      stringsAsFactors = FALSE
    )

# Build a small SOBTPU shard per year to be written by the mocked downloader
.make_sob_for_year <- function(y) {
  # Two groups; include a few coverage forms to test normalization/snap/clamp
  dt <- data.table(
    commodity_year = y,
    state_code = 20L,
    county_code = 45L,
    commodity_code = c(11L,11L,22L,22L),
    type_code = 1L,
    practice_code = c(1L,2L,1L,2L),
    insurance_plan_code = c(2L, 3L, 2L, 4L), # 3L will be filtered (Area)
    unit_structure_code = 1L,
    coverage_type_code = "A",
    reporting_level_type = "Acres",
    net_reporting_level_amount = c(10, 20, 30, 40),
    liability_amount = c(1000, 2000, 3000, 4000),
    total_premium_amount = c(50, 100, 150, 200),
    subsidy_amount = c(30, 60, 90, 120),
    coverage_level_percent = c(0.52, 55, 0.49, 0.97) # test snap, pct->prop, clamp
  )
  dt
}

# ADM price release to be merged; one NA projected_price to test harvest fallback
.make_adm_price <- function() {
  data.table(
    commodity_year = c(2011, 2011),
    state_code = 20L,
    commodity_name = c("WHEAT","CORN"),
    type_code = 1L,
    practice_code = c(1L, 2L),
    projected_price = c(NA_real_, 5.2),
    harvest_price   = c(6.0, 5.0)
  )
}

# Instruments; one row requires tau_sob fallback
.make_instruments <- function() {
  data.table(
    commodity_year = c(2011, 2011),
    state_code = 20L,
    county_code = 45L,
    commodity_code = c(11L,22L),
    tau_adm = c(NA_real_, 0.15),
    tau_sob = c(0.10, 0.20),
    subsidy_rate_65 = c(0.59, 0.62),
    subsidy_rate_75 = c(0.55, 0.55)
  )
}

# NASS county series for three stat cats; one harvested-only path
.make_nass <- function() {
  # WHEAT: PLANTED available
  # CORN: PLANTED explicitly 0 (so fallback continues), HARVESTED=700
  data.table::rbindlist(list(
    data.table(
      agg_level_desc     = "COUNTY",
      statisticcat_desc  = "nassSurvey_AREA_PLANTED",
      value              = 900,
      commodity_year     = 2011L,
      state_code         = 20L,
      county_code        = 45L,
      commodity_name     = "WHEAT"
    ),
    data.table(
      agg_level_desc     = "COUNTY",
      statisticcat_desc  = "nassSurvey_AREA_PLANTED",
      value              = 0,          # ensures the column exists and triggers fallback
      commodity_year     = 2011L,
      state_code         = 20L,
      county_code        = 45L,
      commodity_name     = "CORN"
    ),
    data.table(
      agg_level_desc     = "COUNTY",
      statisticcat_desc  = "nassSurvey_AREA_HARVESTED",
      value              = 700,
      commodity_year     = 2011L,
      state_code         = 20L,
      county_code        = 45L,
      commodity_name     = "CORN"
    )
  ), use.names = TRUE, fill = TRUE)
}

# usmap::fips_info mock result (for KS only)
fips_info_mock <- function(fips_vec) {
  data.frame(
    fips = as.character(fips_vec),
    abbr = rep("KS", length(fips_vec)),
    full = rep("Kansas", length(fips_vec)),
    stringsAsFactors = FALSE
  )
}

# download.file mock that writes the appropriate RDS by URL "kind"
download.file_mock <- function(url, destfile, ..., mode = "wb", quiet = TRUE) {
  obj <- if (grepl("sobtpu_", url)) {
    # pick year from URL
    yr <- as.integer(sub(".*sobtpu_(\\d{4})\\.rds.*", "\\1", url))
    .make_sob_for_year(yr)
  } else if (grepl("fcip_commodity_price\\.rds", url)) {
    .make_adm_price()
  } else if (grepl("fcip_demand_instruments\\.rds", url)) {
    .make_instruments()
  } else if (grepl("nass_production_data\\.rds", url)) {
    .make_nass()
  } else {
    stop("Unexpected URL in mock: ", url)
  }
  saveRDS(obj, destfile)
  0L
}

# Clean, deterministic identifiers for tests (avoid FCIP_INSURANCE_POOL symbol)
.identifiers_full <- c(
  "commodity_year","state_code","county_code",
  "commodity_code","type_code","practice_code",
  "insurance_plan_code","unit_structure_code"
)
