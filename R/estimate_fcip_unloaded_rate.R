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
#' @family FCIP rating helpers
#' @export
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