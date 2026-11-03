

# Optional quick auth sanity check: if gh is installed and configured,
if(requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)

# create and uplaod data release
tryCatch({
  piggyback::pb_release_delete(repo = "ftsiboe/rfcipDemand", tag = "instruments")
}, error = function(e){NULL})

# Create a GitHub Release
piggyback::pb_release_create(
  repo = "ftsiboe/rfcipDemand",
  tag  = "instruments",
  name = "FCIP-Instruments",
  body = readr::read_file(
    file.path("./data-raw/replications/crop_insurance_instruments",
              "README.md")))

# Initialize a local "new release" context for uploads (piggyback convenience helper).
piggyback::pb_new_release( repo = "ftsiboe/rfcipDemand", tag  = "instruments")

# Upload. overwrite = TRUE ensures reruns replace existing assets.
list_assets <- list.files("./data-raw/replications/crop_insurance_instruments", full.names = TRUE, recursive = TRUE)
list_assets <- list_assets[! grepl("README|job_generate",list_assets)]

piggyback::pb_upload(list_assets,repo = "ftsiboe/rfcipDemand", tag  = "instruments",overwrite = TRUE)


