
rm(list = ls(all = TRUE))

devtools::document()


temporary_dir <- tempdir()

piggyback::pb_download(
  file = "sobtpu_all.rds",
  dest = temporary_dir,
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag  = "sob",
  overwrite = TRUE)

piggyback::pb_download(
  file = "fcip_commodity_price.rds",
  dest = temporary_dir,
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag  = "adm_extracts",
  overwrite = TRUE)

















# Verify auth first (nice sanity check)
if (requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)

# Replications
piggyback::pb_release_create(
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag  = "reps",
  name = "Replications",
  body = "Various items aggregated from replications"
)











