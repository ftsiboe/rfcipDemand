# Column names created by data.table's `:=` and used in non-standard evaluation.
# Registered with utils::globalVariables() below so R CMD check does not flag
# "no visible binding for global variable".
global_variables <-  strsplit(
  "
        . ..final_cols ..keep_cols ALL FCIP_INSURANCE_POOL agg_level_desc c_K
    c_P c_Z c_a c_alpha c_net_acre c_u c_v c_x commodity_code
    commodity_name commodity_year contiguous_county_code
    contiguous_state_code county_acreage  county_code
     coverage_level_dominant coverage_level_percent
    coverage_level_percent_aggregate coverage_level_percent_avg
    coverage_level_percent_max coverage_level_percent_wavg
    coverage_type_code  crop_yr delivery_type
    fcip_contiguous_county fcip_recodes_commodity_groupings
    fcip_recodes_insurance_plan fcip_recodes_practice fips
     fsa_planted_acres harvest_price insurance_plan_code
    insured_area lcr liability_amount n nassSurvey_AREA_BEARING
    nassSurvey_AREA_HARVESTED nassSurvey_AREA_PLANTED premium_subsidy_schedule
      coverage rd sp
    net_reporting_level_amount obs period_farmbill planted_acres pool
    potential_liability_amount premium_per_liability price    
    projected_price reporting_level_type standardized_liability_amount drawID gamma_elasticity rate rp_eligible tau0 theta trend
     state_code statisticcat_desc subsidy_amount subsidy_bins Estimate_lavaan FE constrained level name
    subsidy_per_premium subsidy_rate subsidy_rate_65 subsidy_rate_75 tau index_for_price_recived lhs op rent rhs
    tau_adm tau_c tau_sob total_premium_amount triger_level value Estimate N
    fcip_elasticity_limits Estimates Pvalue StdError demand nassSurveyPriceRecivedIndex nassSurveyRentalRates
      ",
  "\\s+"
)

if (getRversion() >= "2.15.1") {
  utils::globalVariables(global_variables[[1]])
}


#' Insurance pool identifier fields
#'
#' A character vector of column names that together define a unique insurance
#' pool in the Federal Crop Insurance Program (FCIP).
#'
#' @details
#' Insurance pools represent the most granular level of rate making within FCIP.
#' Each pool is uniquely identified by the combination of:
#' \itemize{
#'   \item \strong{state_code}: State FIPS code
#'   \item \strong{county_code}: County FIPS code
#'   \item \strong{commodity_code}: Crop commodity code
#'   \item \strong{type_code}: Crop type (e.g., grain vs. silage)
#'   \item \strong{practice_code}: Production practice (e.g., irrigated, organic)
#' }
#'
#' @format A \code{character} vector of field names.
#' @return A \code{character} vector specifying the columns used to define
#'   each FCIP insurance pool.
#' @examples
#' \dontrun{
#' # Default insurance pool fields
#' FCIP_INSURANCE_POOL
#'
#' # Use a coarser grain by passing your own identifiers
#' df <- fcip_demand_data_dispatcher(
#'   identifiers = c("commodity_year", "state_code", "county_code", "commodity_code")
#' )
#'}
#' @export
FCIP_INSURANCE_POOL <- c(
  "state_code",
  "county_code",
  "commodity_code",
  "type_code",
  "practice_code"
)
