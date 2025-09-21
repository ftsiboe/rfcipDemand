# remotes::install_github("dylan-turner25/rfcip", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfsa", force = TRUE,upgrade="never")
# usethis::use_github_action()
rm(list=ls(all=TRUE));gc()
unlink(c("NAMESPACE","./R/helper_data.R",
         list.files("./data", full.names = TRUE),
         list.files("./man", full.names = TRUE)))

for(i in c("calculate_mode","fixed_effect_model_data_prep")){
  download.file(
    paste0("https://raw.githubusercontent.com/ftsiboe/USFarmSafetyNetLab/refs/heads/main/R/",i,".R"),
    paste0("./R/",i,".R"), mode = "wb", quiet = TRUE)
}

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
