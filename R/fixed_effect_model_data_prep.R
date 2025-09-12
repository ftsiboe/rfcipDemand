#' Prepare and demean data for fixed-effects models
#'
#' This function
#'   1. Filters to complete cases on the specified panel, time, weight, variables, and output  
#'   2. If `output` is NULL, creates a dummy output column filled with 1s  
#'   3. Drops any panel with only one observation  
#'   4. Computes within-panel means for the output + each variable in `varlist` (`_mean_i`)  
#'   5. Computes overall sample means for the same set of variables (`_mean`)  
#'   6. Replaces each variable in `varlist` by `value - within_panel_mean + overall_mean`  
#'
#' @param data    A data.frame or data.table containing the data.
#' @param varlist Character vector of variable names to be demeaned.
#' @param panel   Character vector of column name(s) defining the panel identifier.
#' @param time    Character scalar name of the time variable.
#' @param wvar    Character scalar name of a variable to keep but _not_ demean (optional, default NULL).
#' @param output  Character scalar name of an output variable whose means are computed but not altered; if NULL, a dummy column named `"output"` is created (optional, default NULL).
#'
#' @return A list with components  
#'   - **data**: a data.table containing  
#'       - the original `panel`, `time`, `wvar`, `varlist`, and `output` columns  
#'       - two mean columns for each of `c(output, varlist)`:  
#'         `<name>_mean_i` (within-panel) and `<name>_mean` (overall)  
#'   - **NFE**: the number of panels with more than one observation
#' @family Estimators panel models
#' @import data.table
#' @export
fixed_effect_model_data_prep <- function(
    data, 
    varlist, 
    panel, 
    time, 
    wvar = NULL, 
    output = NULL) {
  
  # 1) ensure data.table
  dt <- as.data.table(data)
  
  # 2) if no output specified, create dummy
  if (is.null(output)) {
    output <- "output"
    dt[, output := 1]
  }
  
  # 3) keep only needed columns, drop incomplete rows
  keep_cols <- unique(c(panel, time, wvar, varlist, output))
  dt <- dt[, ..keep_cols]
  dt <- dt[complete.cases(dt)]
  
  # 4) drop panels with only one obs, count panels remaining
  dt[, obs := .N, by = panel]
  dt <- dt[obs > 1]
  NFE <- nrow(unique(dt[, c(panel), with = FALSE]))

  # 5) compute within-panel means (_mean_i)
  mean_vars <- c(output, varlist)
  dt[, paste0(mean_vars, "_mean_i") := lapply(.SD, mean),by = panel, .SDcols = mean_vars]
  
  # 6) compute overall sample means (_mean)
  dt[, ALL := 1L]
  dt[, paste0(mean_vars, "_mean") := lapply(.SD, mean),by = ALL, .SDcols = paste0(mean_vars, "_mean_i")]
  
  # 7) demean each variable in varlist
  dt[, (varlist) := lapply(varlist, function(v)
    get(v) - get(paste0(v, "_mean_i")) + get(paste0(v, "_mean")))]
  
  # 8) select final columns, drop any leftover NAs
  final_cols <- c(
    panel, time, wvar,
    varlist, output,
    paste0(output, "_mean"),
    paste0(output, "_mean_i"))
  
  dt <- dt[, ..final_cols]
  dt <- dt[complete.cases(dt)]
  
  # 9) return
  list(data = dt,NFE  = NFE)
}

