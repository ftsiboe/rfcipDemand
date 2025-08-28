# remotes::install_github("dylan-turner25/rmaADM", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfcip", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfsa", force = TRUE,upgrade="never")
# usethis::use_github_action()
rm(list=ls(all=TRUE));gc()
unlink(c("NAMESPACE","./R/helper_data.R",
         list.files("./data", full.names = TRUE),
         list.files("./man", full.names = TRUE)))
source("data-raw/build_internal_datasets.R")
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

devtools::document()
unique(unlist(layouts_fcip))[
  !unique(unlist(layouts_fcip)) %in% 
    c(FCIP_FORCE_CHARACTER_KEYS,
      FCIP_FORCE_AMOUNT_VARIABLES,
      FCIP_FORCE_NUMERIC_KEYS,
      FCIP_INSURANCE_POOL,
      FCIP_INSURANCE_ELECTION_RCODED)]
