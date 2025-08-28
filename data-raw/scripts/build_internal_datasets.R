rm(list=ls(all=TRUE));gc();library(rfcip);library(data.table);library(dplyr)

dir_data_release <- "C:/GitHub/USFarmSafetyNetLab/data-raw/release"

#   Maximum file size (in megabytes) allowed for inclusion.  Any dataset whose
#   largest yearly file exceeds this threshold is skipped entirely.  Defaults to `1`.
size_threshold = 1

source_files <- c(
  list.files(paste0(dir_data_release,"/fsa") ,full.names = T,pattern = ".rds$"),
  #list.files(paste0(dir_data_release,"/ice") ,full.names = T,pattern = ".rds$"),
  list.files(paste0(dir_data_release,"/nass"),full.names = T,pattern = ".rds$"),
  list.files(paste0(dir_data_release,"/fsa") ,full.names = T,pattern = ".rds$"),
  list.files(paste0(dir_data_release,"/adm") ,full.names = T,pattern = ".rds$"))

# id "./data" doesn't exist, create it
if(!dir.exists("./data")) {
  dir.create("./data")
}

# Filter only actual files (not directories)
files <- source_files[file.info(source_files)$isdir == FALSE]

# Get file sizes
sizes <- file.info(files)$size

# Create data frame
file_info <- data.frame(
  file_path = files,
  size_bytes = sizes,
  size_mb = sizes / (1024 * 1024), # Convert to MB
  stringsAsFactors = FALSE
)

# Add a column with the file name without any of the parent folders
file_info$file_name <- basename(source_files)
file_info$file_name <- gsub("[0-9]{4}/", "", file_info$file_name)
file_info$file_name <- gsub(".rds", "", file_info$file_name)

# keep only files that are less than the size threshold (in MB). Applied to
# maximum size over all years
max_sizes <- file_info |>
  group_by(.data$file_name) |>
  summarize(max_size = max(.data$size_mb)) |>
  filter(.data$max_size < size_threshold)

file_info <- file_info |>
  filter(.data$file_name %in% max_sizes$file_name)

# if "./R/helper_data.R" already exists, rename it with the date appended
if(file.exists("./R/helper_data.R")){
  file.rename("./R/helper_data.R", paste0("./R/helper_data_", Sys.Date(), ".R"))
}

# create a new file with the header for the documentation
write("#' @title Simulator Helper Datasets\n",
      file = "./R/helper_data.R", append = FALSE)

# for each unique value in the file_name column,
# load all the datasets corresponding to that file name,
# combine them, save as .rda, and document in helper_data.R
for(f in unique(file_info$file_name)){
  
  # get the file paths for the current file name
  file_paths <- file_info[file_info$file_name == f, "file_path"]
  
  # load and combine the datasets
  data <- file_paths |>
    purrr::map(~ {
      df <- readRDS(.x)                 # read in the data frame
      df[] <- lapply(df, as.character) # convert all columns to character
      df                               # return the data frame
    }) |>
    dplyr::bind_rows()                 # bind rows
  
  # convert columns to their appropriate types
  data <- suppressMessages(readr::type_convert(data))
  
  # derive the output name from file paths
  file_out <- unique(gsub("^\\d{4}_[A-Za-z]\\d{5}_|_YTD|\\.rds", "",
                          basename(file_paths)))
  
  # assign and save as .rda
  assign(file_out, data)
  save(list = file_out, file = paste0("./data/", file_out, ".rda"), compress = "xz")
  
  # extract data source for documentation
  data_source <- data$data_source[1]
  
  # build a roxygen doc entry
  doc_entry <- paste0(
    "#' @name ", file_out, "\n",
    "#' @title ", file_out, "\n",
    "#' @description A combined dataset for ", file_out, "\n",
    "#' @format A data frame with ", nrow(data), " rows and ", ncol(data),
    " columns covering ", min(data$commodity_year), "-", max(data$commodity_year), ".\n",
    "#' @source ", data_source, "\n",
    "#' @usage data(", file_out, ")\n",
    "\"", file_out, "\""
  )
  
  # append the doc entry
  write(doc_entry, file = "./R/helper_data.R", append = TRUE)
}


