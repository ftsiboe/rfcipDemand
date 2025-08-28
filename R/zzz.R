.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)
  
  # set global timeout limit
  options(timeout = 360000)

  options(future.globals.maxSize = 20 * 1024^3)  # 20 GiB

  # Note: data.table's `:=` creates these columns at runtime. We register them here
  # so that R CMD check does not flag no visible binding for global variable.
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      strsplit(
        "
      ",
        "\\s+"
      )[[1]]
    )
  }
}




