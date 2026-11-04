# remotes::install_github("dylan-turner25/rfcip", force = TRUE,upgrade="never")
# usethis::use_github_action()

# Run in your project:
rm(list=ls(all=TRUE));gc()

if(toupper(as.character(Sys.info()[["sysname"]])) %in% "WINDOWS"){
  source( file.path(dirname(dirname(getwd())),"code-library.R"))
  file.copy(from = file.path(codeLibrary,"estimators/fixed_effect_model_data_prep.R"),
            to = "R/", overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
}

# Optional rebuilds of upstream inputs (slow; off by default)
# - RUN_INSTRUMENTS: rebuilds data-raw/replications/crop_insurance_instruments/
#   fcip_demand_instruments.rds. It is used by fcip_demand_data_dispatcher() only
#   after you upload it to the `instruments` release (create_release.R).
# - RUN_ELASTICITY_LIMITS: re-estimates the program-level elasticity limits and
#   writes data/fcip_elasticity_limits.rda directly.
# Each script runs in its own environment, so their rm(list = ls()) calls do not
# clear this session.
RUN_INSTRUMENTS       <- FALSE
RUN_ELASTICITY_LIMITS <- FALSE

if(RUN_INSTRUMENTS) {
  source("data-raw/replications/crop_insurance_instruments/generate_fcip_instruments.R",
         local = new.env())
}

if (RUN_ELASTICITY_LIMITS) {
  library(data.table)
  source("data-raw/scripts/estimate_fcip_elasticity_limits.R", local = new.env())
}

# Rebuild data/ from the manifest in data-raw/scripts/datasets.R.
# Docs for the datasets are hand-written in R/data.R.
source("data-raw/scripts/datasets.R")

# Sanity pass through R/ sources: shows any non-ASCII characters per file
for (i in list.files("R", full.names = TRUE)) {
  print(paste0("********************", i, "********************"))
  tools::showNonASCIIfile(i)
}

# Rebuild documentation from roxygen comments
devtools::document()

# Check man pages only
devtools::check_man()

# Build PDF manual into the current working directory
devtools::build_manual(path = getwd())

# Optional: run tests / full package check (uncomment when needed)
# devtools::test()
devtools::check()

data <- fcip_demand_data_dispatcher(
  study_years = 2001:as.numeric(format(Sys.Date(),"%Y")),
  identifiers = c("commodity_year", "state_code","county_code","commodity_code","type_code",
                  "practice_code", "insurance_plan_code", "unit_structure_code"))
length(unique(fcip_recodes_commodity_groupings$commodity_name))
