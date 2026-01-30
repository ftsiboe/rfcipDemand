
rm(list = ls(all = TRUE))

devtools::document()

# Verify auth first (nice sanity check)
if (requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)

# Replications
piggyback::pb_release_create(
  repo = "ftsiboe/USFarmSafetyNetLab",
  tag  = "reps",
  name = "Replications",
  body = "Various items aggregated from replications"
)











