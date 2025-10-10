# remotes::install_github("dylan-turner25/rfcip", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfsa", force = TRUE,upgrade="never")
# usethis::use_github_action()

# Run in your project:
rm(list=ls(all=TRUE));gc()

# Clean generated artifacts
unlink(c("NAMESPACE","./R/helper_data.R",
         list.files("./data", full.names = TRUE),
         list.files("./man", full.names = TRUE)))

for(i in c("fixed_effect_model_data_prep")){
  download.file(
    paste0("https://raw.githubusercontent.com/ftsiboe/USFarmSafetyNetLab/heads/main/R/estimators/",i,".R"),
    paste0("./R/",i,".R"), mode = "wb", quiet = TRUE)
}

for(i in c("calculate_mode")){
  download.file(
    paste0("https://raw.githubusercontent.com/ftsiboe/USFarmSafetyNetLab/heads/main/R/",i,".R"),
    paste0("./R/",i,".R"), mode = "wb", quiet = TRUE)
}

source("data-raw/scripts/stashcodes/build_internal_datasets.R")

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
