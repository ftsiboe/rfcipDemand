# Build the datasets shipped in rfcipDemand/data/
#
# Conventional usethis layout: this script lives in data-raw/ (excluded from the
# built package by .Rbuildignore) and writes data/<name>.rda with
# usethis::use_data(). Documentation is hand-written in R/data.R.
#
# Only the datasets listed in `datasets` below are built. A new .rds file in a
# source folder changes nothing until it is added here. All datasets are read
# and checked first; data/ is written only if every one passes, so a failed run
# leaves the package as it was.
#
# Run from the package root:
#   source("data-raw/datasets.R")

source(file.path(dirname(getwd()), "fcipResources", "data-raw", "scripts", "environment_setup.R"))

# ---- Source folders ----------------------------------------------------------
roots <- c(
  internal = "data-raw/internal_data",
  fsa      = file.path(dirname(dirname(wd$adm)), "usda_fsa", "fsa_acreage", "processed"),
  nass     = file.path(dirname(dirname(wd$adm)), "usda_nass", "quick_stats_database", "processed"),
  adm      = file.path(wd$adm, "processed", "adm_extracts")
)

# ---- Manifest ----------------------------------------------------------------
# name: object name in data/ (also the .rds base name in the source folder,
#       after removing a leading "YYYY_A00000_" and a "_YTD" suffix)
# root: which folder in `roots` holds the yearly .rds files
# keys: columns that must be present (and, with `unique_keys`, unique)
datasets <- list(
  fcip_elasticity_limits = list(
    root = "internal", keys = c("gamma", "theta", "total"), unique_keys = FALSE),
  fcip_contiguous_county = list(
    root = "adm", keys = c("state_code", "county_code", "contiguous_state_code", "contiguous_county_code"),
    unique_keys = FALSE),
  fcip_recodes_commodity_groupings = list(
    root = "adm", keys = c("commodity_year", "commodity_code"), unique_keys = TRUE),
  fcip_recodes_insurance_plan = list(
    root = "adm", keys = c("commodity_year", "insurance_plan_code"), unique_keys = TRUE),
  # Not unique: under the placeholder commodity_code 9999, ADM lists two names
  # for some practice codes (340 keys in the 2026 build). Real commodities are unique.
  fcip_recodes_practice = list(
    root = "adm", keys = c("commodity_year", "commodity_code", "practice_code"), unique_keys = FALSE),
  premium_subsidy_schedule = list(
    root = "adm", keys = c("commodity_year", "insurance_plan_code", "coverage_level_percent",
                           "coverage_type_code", "unit_structure_code"), unique_keys = FALSE),
  nassSurveyPriceRecivedIndex = list(
    root = "nass", keys = c("commodity_year", "index_for_price_recived"), unique_keys = FALSE),
  nassSurveyRentalRates = list(
    root = "nass", keys = c("state_code", "commodity_year", "rent"), unique_keys = FALSE)
)

# ---- Helpers -----------------------------------------------------------------
dataset_base_name <- function(path) {
  gsub("^\\d{4}_[A-Za-z]\\d{5}_|_YTD|\\.rds$", "", basename(path))
}

read_dataset <- function(name, spec) {
  dir <- roots[[spec$root]]
  if (!dir.exists(dir)) stop("Source folder for `", name, "` not found: ", dir)

  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  files <- files[dataset_base_name(files) == name]
  if (length(files) == 0L) stop("No .rds files for `", name, "` in ", dir)

  data <- lapply(files, function(p) {
    df <- readRDS(p)
    if (!is.data.frame(df)) stop("Not a data frame: ", p)
    df <- as.data.frame(df)
    df[] <- lapply(df, as.character)
    df
  })
  data <- dplyr::bind_rows(data)
  data <- suppressMessages(readr::type_convert(data))
  data <- data.table::as.data.table(data)

  missing <- setdiff(spec$keys, names(data))
  if (length(missing)) stop("`", name, "` is missing columns: ", paste(missing, collapse = ", "))
  if (nrow(data) == 0L) stop("`", name, "` has no rows")
  if (isTRUE(spec$unique_keys) && anyDuplicated(data, by = spec$keys)) {
    stop("`", name, "` has duplicate rows on: ", paste(spec$keys, collapse = ", "))
  }

  message(sprintf("%-34s %8d rows  %3d cols  from %d file(s)", name, nrow(data), ncol(data), length(files)))
  data
}

# ---- Build: read and check everything, then write ---------------------------
built <- Map(read_dataset, names(datasets), datasets)

for (name in names(built)) {
  assign(name, built[[name]])
  do.call(usethis::use_data, list(as.name(name), overwrite = TRUE, compress = "xz"))
}

# Datasets in data/ that are no longer in the manifest
stale <- setdiff(tools::file_path_sans_ext(list.files("data", pattern = "\\.rda$")), names(datasets))
if (length(stale)) warning("data/ has files not in the manifest: ", paste(stale, collapse = ", "))

rm(list = c(names(built), "built"))
