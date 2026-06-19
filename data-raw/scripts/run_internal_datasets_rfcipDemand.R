
rm(list=ls(all=TRUE));gc();library(rfcip);library(data.table);library(dplyr)

source("R/build_internal_datasets.R")

source(file.path(dirname(getwd()),"fcipResources","data-raw","scripts","environment_setup.R"))

dir_data_release <- file.path(gsub("rfcipDemand","USFarmSafetyNetLab",getwd()),"data-raw/release")

build_internal_datasets(
  package_directory = getwd(),
  source_files = c(
    list.files("data-raw/internal_data" ,full.names = T,pattern = ".rds$"),
    list.files(file.path(dirname(dirname(wd$adm)),"usda_fsa","fsa_acreage","processed") ,full.names = T,pattern = ".rds$"),
    list.files(file.path(dirname(dirname(wd$adm)),"usda_nass","quick_stats_database","processed"),full.names = T,pattern = ".rds$"),
    list.files(file.path(wd$adm,"processed","adm_extracts") ,full.names = T,pattern = ".rds$"))
  , size_threshold = 1 )



