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
  NFE <- dt[, uniqueN(panel)]
  
  # 5) compute within-panel means (_mean_i)
  mean_vars <- c(output, varlist)
  dt[, paste0(mean_vars, "_mean_i") := lapply(.SD, mean),by = panel, .SDcols = mean_vars]
  
  # 6) compute overall sample means (_mean)
  dt[, ALL := 1L]
  dt[, paste0(mean_vars, "_mean") := lapply(.SD, mean),by = ALL, .SDcols = mean_vars]
  
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

#' Panel-based spatial smoothing estimator
#'
#' This function
#'   1. Constructs spatially-varying treatment interactions (one variable per spatial unit)  
#'   2. Applies within-panel/time fixed-effects demeaning to both outcome and interactions  
#'   3. Fits an OLS model by hand (\code{lm.fit}) to recover one coefficient per spatial unit  
#'
#' @param data      A \code{data.table} or \code{data.frame} containing panel data.
#' @param output    Name of the outcome variable (character scalar).
#' @param treatment Name of the treatment variable whose spatial effects we estimate (character scalar).
#' @param time      Name of the time variable (character scalar).
#' @param panel     Name(s) of the panel identifier variable(s) (character vector).
#' @param spatialvar Name of the spatial grouping variable (e.g. county FIPS; character scalar).
#'
#' @return A \code{data.table} with columns:
#'   - \code{estimate}: the estimated spatial-unit coefficient  
#'   - \code{county_fips}: the spatial unit identifier (5-digit FIPS)  
#'   - \code{state_code}, \code{county_code}: parsed FIPS components  
#'
#' @details
#' Internally, we  
#'   1. Build \code{treatment_code} = \code{I(spatialvar==code) * treatment} for each spatial unit code.  
#'   2. Call \code{fixed_effect_model_data_prep()} to demean the outcome and all \code{treatment_code} variables.  
#'   3. Assemble the design matrix \code{X = [output_mean_i, treatment_*]} and response \code{y}.  
#'   4. Solve \eqn{\hat\beta = (\tilde X'\tilde X)^{-1}\tilde X'\tilde y} via \code{lm.fit}.  
#'   5. Return a row per spatial unit with its coefficient.  
#' @family Estimators panel models
#' @import data.table
#' @importFrom stats lm.fit
#' @export
panel_based_spatial_smoothing_estimator <- function(
    data,
    output,
    treatment,
    time,
    panel,
    spatialvar){
  
  # 1) create treatment-by-spatial-unit interaction columns
  spatialvar_list <- data[, unique(get(spatialvar))]
  data[ , paste0(treatment, '_', spatialvar_list) := 
          lapply(spatialvar_list, function(code) 
            as.integer(get(spatialvar) == code) * get(treatment)
          )
  ]
  
  # 2) demean outcome & interactions via panel/time fixed effects
  fe_prep <- fixed_effect_model_data_prep(
    data    = data,
    varlist = names(data)[grepl(paste0(treatment, '_'), names(data))],
    panel   = panel,
    time    = time,
    wvar    = spatialvar,
    output  = output
  )
  data <- fe_prep$data
  
  # 3) build response vector y and design matrix X
  y <- data[[output]]
  X <- as.matrix(data[, c(
    paste0(output, '_mean_i'),
    names(data)[grepl(paste0(treatment, '_'), names(data))]
  ), with = FALSE])
  
  # free memory
  rm(data); gc()
  
  # 4) fit OLS by hand using QR (lm.fit)
  coeffs <- lm.fit(X, y)$coefficients
  
  # 5) assemble results
  fit_df <- data.table(
    estimate     = coeffs,
    county_fips  = gsub(paste0(treatment, '_'), '', names(coeffs))
  )
  
  # keep only valid spatial units
  fit_df <- fit_df[county_fips %in% spatialvar_list]
  fit_df <- fit_df[complete.cases(fit_df)]
  
  return(fit_df)
}
