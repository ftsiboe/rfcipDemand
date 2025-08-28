
rm(list=ls(all=TRUE));gc()
if (!requireNamespace("covr", quietly = TRUE)) install.packages("covr")
devtools::document()

if (!dir.exists("./data-raw/badges/lib")) {
  dir.create("./data-raw/badges/lib", recursive = TRUE)
}

library(covr)
detach("package:rfcipDemand", unload = TRUE)
covr::report(file="data-raw/badges/test-coverage-report.html")
