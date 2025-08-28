# remotes::install_github("dylan-turner25/rmaADM", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfcip", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfsa", force = TRUE,upgrade="never")
# usethis::use_github_action()
rm(list=ls(all=TRUE));gc()
unlink(c("NAMESPACE","./R/helper_data.R",
         list.files("./data", full.names = TRUE),
         list.files("./man", full.names = TRUE)))
source("data-raw/scripts/build_internal_datasets.R")
rm(list=ls(all=TRUE))
devtools::document()
for(i in list.files("R",full.names = T)){
  print(paste0("********************",i,"********************"))
  tools::showNonASCIIfile(i)
}
devtools::check_man()
devtools::build_manual(path = getwd())
#devtools::test()

devtools::check()
































