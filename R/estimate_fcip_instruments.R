#' Estimate FCIP Instrumental Variables (Unloaded Rates)
#'
#' Uses historical FCIP rate data to build instrumented unloaded-rate variables
#' following:
#'   1. Tsiboe & Turner (2023), Econometric identification of crop insurance participation  
#'      _Agricultural and Resource Economics Review_, 52(3):476-497.  
#'      \url{https://doi.org/10.1017/age.2023.13}  
#'
#' @param year Integer. The target crop year for which to construct instruments.
#' @param statplan A data.table containing FCIP rate elements, including at least:
#'   \describe{
#'     \item{commodity_year}{Year of the rate observation.}
#'     \item{state_code, county_code}{County identifiers.}
#'     \item{commodity_code}{Crop identifier.}
#'     \item{insured_area, lcr, contiguous_state_code, contiguous_county_code}{Fields
#'       required by `estimate_fcip_unloaded_rate()`.}
#'   }
#'
#' @import data.table
#' @return A data.table with one row per county-crop for the specified \code{year},
#'   containing:
#'   \describe{
#'     \item{state_code, county_code, commodity_code}{Keys.}
#'     \item{tau_sob}{Smoothed unloaded rate (uses contiguous-county means to fill zeros/NAs).}
#'     \item{commodity_year}{The input \code{year}, repeated.}
#'   }
#'
#' @details
#' 1. **Task list**: Identify all unique (state, county) pairs with data in the  
#'    2-21 years before \code{year}.  
#' 2. **Unloaded-rate calculation**: For each county in \code{task_list}, call  
#'    `estimate_fcip_unloaded_rate()` on the same 2-21 year window to get \code{tau}.  
#'    Errors return \code{NULL} so processing continues.  
#' 3. **Contiguous-county smoothing**:  
#'    - Build a lookup table of contiguous counties (using \code{contiguous_county}).  
#'    - For each contiguous group, compute the mean \code{tau} to get \code{tau_c}.  
#' 4. **Merge & fill**: Left-join the raw \code{adm} and \code{contiguous_adm};  
#'    replace any zero/NA/Inf \code{tau} with the group mean \code{tau_c} into  
#'    \code{tau_sob}.  
#' 5. **Cleanup**: Drop helper columns (\code{tau}, \code{tau_c}), remove invalid rows,  
#'    add \code{commodity_year}, and return the result.
#' @family FCIP instruments
#' @export
estimate_fcip_instruments <- function(year, statplan) {
  
  # 1. Build list of (state, county) with at least 2-21 years of data before 'year'
  task_list <- unique(statplan[commodity_year %in% (year-2):(year-21), .(state_code, county_code)])
  
  # 2. For each county, compute the unloaded rate via the helper function
  adm <- data.table::rbindlist(
    lapply(
      1:nrow(task_list),
      function(i){
        tryCatch({
          estimate_fcip_unloaded_rate(
            statplan = statplan[commodity_year %in% (year-2):(year-21)],
            year   = year,
            state  = task_list$state_code[i],
            county = task_list$county_code[i])
        }, error = function(e){return(NULL)})
      }), fill = TRUE)
  setDT(adm)
  
  # 3. Prepare contiguous county mapping for smoothing
  contiguous_county <- fcip_contiguous_county
  setDT(contiguous_county)
  contiguous_county[, state_code := contiguous_state_code]
  contiguous_county[, county_code := contiguous_county_code]
  contiguous_adm <- unique(contiguous_county, by = c("state_code", "county_code"))
  
  # 4. For each contiguous group, compute the mean tau => tau_c
  contiguous_adm <- data.table::rbindlist(
    lapply(
      1:nrow(contiguous_adm),
      function(ss){
        tryCatch({
          # ss <- 1
          data <- contiguous_adm[ss][contiguous_county, on = .(state_code, county_code), nomatch = 0][
            adm, on = .(state_code, county_code), nomatch = 0]
          
          data <- data[, .(tau_c = mean(tau, na.rm = TRUE)),by = .(state_code, county_code, commodity_code)]
          
          return(data)
        }, error = function(e){return(NULL)})
      }), fill = TRUE)
  
  # 5. Merge raw rates with contiguous-county smoothed rates
  adm <- adm[contiguous_adm, on = intersect(names(adm), names(contiguous_adm)), nomatch = 0]
  
  # 6. Replace any invalid/zero tau with the smoothed tau_c
  adm[, tau_sob := fifelse(tau %in% c(NA, Inf, -Inf, NaN) | tau == 0, tau_c, tau)]
  
  # 7. Drop helper columns and invalid rows
  rm(contiguous_adm);gc()
  adm <- adm[, setdiff(names(adm), c("tau_c", "tau")), with = FALSE]
  adm <- adm[!tau_sob %in% c(NA, Inf, -Inf, NaN,0)]
  
  # 8. Tag with the target commodity_year and return
  adm[, commodity_year := year]
  gc()
  return(adm)
}

#' Estimate FCIP Unloaded (County) Rates
#'
#' Computes the unloaded loss cost rates (\code{tau}) for counties based on
#' the FCIC Rate Methodology Handbook (2009), pp. 65-70.
#'
#' @param statplan A \link[data.table]{data.table} containing FCIP rate elements
#'   with at least the columns:
#'   \describe{
#'     \item{state_code, county_code}{Identifiers for each county.}
#'     \item{contiguous_state_code, contiguous_county_code}{Mapping to county group.}
#'     \item{insured_area}{Total insured acres in the county.}
#'     \item{lcr}{Loss Cost Rate for each county.}
#'     \item{commodity_code}{Crop identifier.}
#'   }
#' @param year Integer. Crop year for which rates are being estimated
#'   (currently not used but reserved for future subsetting).
#' @param crop Optional vector of commodity codes to filter by crop.
#' @param state Optional vector of state codes to restrict the analysis.
#' @param county Optional vector of county codes to restrict the analysis.
#'
#' @return A \code{data.frame} with columns:
#'   \describe{
#'     \item{state_code, county_code, commodity_code}{Keys identifying county and crop.}
#'     \item{tau}{Estimated FCIP county unloaded rate.}
#'   }
#' @import data.table
#' @importFrom stats var
#' @details
#' 1. **Target data** is filtered to the selected state(s)/county(ies).  
#' 2. **Group data** finds contiguous-county groupings, unions them with the target.  
#' 3. Computes group-level statistics:
#'   - \code{c_alpha}: mean insured acres  
#'   - \code{c_u}: mean LCR  
#'   - \code{c_a}: variance of LCR  
#' 4. Computes target county statistics:
#'   - \code{c_x}: mean LCR  
#'   - \code{c_v}: variance of LCR  
#'   - \code{c_net_acre}: total insured acres  
#' 5. Applies the blending formula  
#'   \deqn{\tau = Z\,x + (1 - Z)\,u, \quad Z = P/(P + K)}  
#'   where  
#'   \eqn{P = c_{\!net\_acre}/c_\alpha,\quad K = c_v/c_a.}
#'
#' @references
#' FCIC Rate Methodology Handbook APH (2009), pp. 65-70.  
#' \url{https://legacy.rma.usda.gov/pubs/2008/ratemethodology.pdf}
#' @family FCIP instruments
#' @keywords internal
#' @noRd
estimate_fcip_unloaded_rate <- function(
    statplan,
    year   = 2011,
    crop   = NULL,
    state  = NULL,
    county = NULL) {
  # Documentation link:
  # https://legacy.rma.usda.gov/pubs/2008/ratemethodology.pdf
  
  # 1. Filter to the target county(ies)/state(s)
  target_data <- statplan[
    state_code %in% state & county_code %in% county
  ]
  
  # 2. Build the contiguous-county group for each target county
  contiguous_county <- fcip_contiguous_county
  group_data <- target_data[contiguous_county, on = .(state_code, county_code), nomatch = 0
  ][, .(state_code = contiguous_state_code, county_code = contiguous_county_code)]
  group_data <- unique(group_data)
  
  # 3. Combine group members with the original target_data
  group_data  <- unique(rbind(group_data[statplan  , on = .(state_code, county_code), nomatch = 0],target_data))
  
  # 4. Compute group-level insured area mean (c_alpha), LCR mean (c_u), LCR variance (c_a)
  group_data <- group_data[, .(
    c_alpha = mean(insured_area,na.rm=T),c_a = var(lcr,na.rm=T),
    c_u = mean(lcr,na.rm=T)), by = .(commodity_code)]
  
  # 5. Compute target county LCR stats: variance (c_v), mean (c_x), net insured acres (c_net_acre)
  target_data <- target_data[, .(
    c_v = var(lcr,na.rm=T), c_x = mean(lcr,na.rm=T),
    c_net_acre = sum(insured_area,na.rm=T)), by = .(state_code,county_code,commodity_code)]
  
  # 6. Join target county stats with group stats by commodity_code
  res <- target_data[group_data, on = .(commodity_code), nomatch = 0]
  
  # 7. Calculate P, K, Z, and tau according to the handbook formula
  res[, c_P := c_net_acre/c_alpha]
  res[, c_K := c_v/c_a]
  res[, c_Z := c_P/(c_P+c_K)]
  res[, tau := c_Z*c_x + (1-c_Z)*c_u] # County Unloaded Rate (same as target rate).
  
  # 8. Filter out invalid or zero tau values
  res <- res[!tau %in% c(NA, Inf, -Inf, NaN, 0), ]
  
  # 9. Return a clean data.frame with only the columns of interest
  res <- res[, .(state_code, county_code, commodity_code, tau)]
  
  return(res)
}


#' Formulate & Merge National Subsidy Rate Instrument (Yu et al., 2018)
#'
#' Downloads the historical Summary of Business RDS and computes
#' national subsidy-rate instruments at specified coverage levels,
#' following Yu et al. (2018).
#'
#' @param dt sobcov
#' @param delivery_systems Character vector. Delivery systems to include;
#'                   default \code{c("RBUP","FBUP")}.
#' @param plan_codes Integer vector. Insurance plan codes to include;
#'                   default \code{c(1:3, 90, 44, 25, 42)}.
#' @param coverage_levels Numeric vector. Percent coverage levels to keep;
#'                   default \code{c(65, 75)}.
#'
#' @return A data.table with columns: commodity_year, subsidy_rate_65, subsidy_rate_75.
#' @family FCIP instruments
#' @import data.table
#' @importFrom tidyr spread
#' @export
get_yu2018_instrument <- function(
    dt,
    delivery_systems  = c("RBUP", "FBUP"),
    plan_codes        = c(1:3, 90, 44, 25, 42),
    coverage_levels   = c(65, 75)) {
  # 2. Read into data.table
  dt <- data.table::as.data.table(dt)
  
  # 3. Filter to relevant delivery systems & plan codes
  dt <- dt[delivery_type %in% delivery_systems]
  dt <- dt[insurance_plan_code   %in% plan_codes]
  
  # 4. Create a label for each coverage level
  #    (round to nearest 5% then convert to a subsidy_rate_## string)
  dt[, coverage_level_percent := paste0("subsidy_rate_",(round((coverage_level_percent / 0.05)) * 0.05) * 100)]
  
  # 5. Keep only the coverage levels we need
  dt <- dt[coverage_level_percent %in% paste0("subsidy_rate_", coverage_levels)]
  
  # 6. Summarize total subsidy and premium by crop year & coverage level
  dt_sum <- dt[
    , .(
      subsidy_amount    = sum(subsidy_amount,    na.rm = TRUE),
      total_premium_amount = sum(total_premium_amount, na.rm = TRUE)
    ),by = .(commodity_year, coverage_level_percent)]
  
  # 7. Convert to rate = subsidy_amount / total premium
  dt_sum[, subsidy_rate := subsidy_amount / total_premium_amount]
  
  # 8. Reshape to wide: one column per coverage level subsidy rate (requires tidyr)
  dt_wide <- dt_sum[, .(commodity_year, coverage_level_percent, subsidy_rate)] |>
    tidyr::spread(coverage_level_percent, subsidy_rate)
  
  # 9. Return as data.table
  return(data.table::as.data.table(dt_wide))
}
