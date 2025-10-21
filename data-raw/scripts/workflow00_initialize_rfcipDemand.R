# remotes::install_github("dylan-turner25/rfcip", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfsa", force = TRUE,upgrade="never")
# usethis::use_github_action()

# Run in your project:
rm(list=ls(all=TRUE));gc()

# Clean generated artifacts
unlink(c("NAMESPACE","./R/helper_data.R",
         list.files("./data", full.names = TRUE),
         list.files("./man", full.names = TRUE)))

list_function <- c(
  "https://raw.githubusercontent.com/ftsiboe/USFarmSafetyNetLab/heads/main/data-raw/scripts/library/estimators/fixed_effect_model_data_prep.R",
  "https://raw.githubusercontent.com/ftsiboe/USFarmSafetyNetLab/heads/main/R/build_internal_datasets.R"
)

for(i in list_function){
  download.file(i, paste0("./R/",basename(i)), mode = "wb", quiet = TRUE)
}

source("data-raw/scripts/run_internal_datasets_rfcipDemand.R")

unlink(list.files("R",full.names = TRUE,pattern = "build_internal_datasets.R"))

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

df <- fcip_demand_data_dispatcher(
  study_years = 2001:2025,
  identifiers = c("commodity_year", "state_code","county_code","commodity_code","type_code",
                  "practice_code", "insurance_plan_code", "unit_structure_code"))
length(unique(fcip_recodes_commodity_groupings$commodity_name))
