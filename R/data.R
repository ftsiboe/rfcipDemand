# Documentation for the datasets in data/.
# Built by data-raw/datasets.R. Edit this file by hand; it is not regenerated.

#' Bounds on FCIP demand elasticities
#'
#' Program-level limits used to constrain calibrated demand elasticities. Each
#' limit is 50% of the magnitude of the corresponding program-level elasticity.
#' Used as the default `elasticity_limits` in [rfcipDemand_controls()].
#'
#' @format A data frame with one row and 4 columns:
#' \describe{
#'   \item{gamma}{Limit on the elasticity of insured acres (net reporting level).}
#'   \item{theta}{Limit on the elasticity of coverage level.}
#'   \item{total}{Limit on the total elasticity.}
#'   \item{data_source}{Text description of how the limits were derived.}
#' }
#' @source Estimated by `data-raw/scripts/estimate_fcip_elasticity_limits.R`.
"fcip_elasticity_limits"

#' Contiguous counties
#'
#' County adjacency from the RMA Actuarial Data Master, used to build
#' neighbouring-county instruments in [estimate_fcip_instruments()].
#'
#' @format A data frame with one row per county and contiguous county:
#' \describe{
#'   \item{record_type_code, record_category_code}{ADM record identifiers.}
#'   \item{reinsurance_year}{Reinsurance year of the ADM release.}
#'   \item{state_code, county_code}{FIPS codes of the county.}
#'   \item{contiguous_state_code, contiguous_county_code}{FIPS codes of the contiguous county.}
#'   \item{contiguous_county_type_code}{ADM contiguity type code.}
#'   \item{last_released_date, released_date, deleted_date}{ADM record dates.}
#'   \item{data_source}{Source description.}
#' }
#' @source USDA Risk Management Agency, Actuarial Data Master, record A01230.
"fcip_contiguous_county"

#' Commodity groupings
#'
#' Commodity names and groupings by commodity year, joined onto FCIP data by
#' [fcip_demand_data_dispatcher()].
#'
#' @format A data frame with one row per `commodity_year` and `commodity_code`:
#' \describe{
#'   \item{commodity_year}{Crop (commodity) year.}
#'   \item{commodity_code}{RMA commodity code.}
#'   \item{CROP}{Harmonized crop name.}
#'   \item{commodity_name}{RMA commodity name.}
#'   \item{commodity_group}{Commodity group, as `"group:subgroup"`.}
#'   \item{annimal_flag, annual_flag, perennial_flag}{Indicators for animal,
#'     annual and perennial commodities (`"1"`, `"0"`, or `"-Inf"` when unknown).}
#'   \item{selected_crops_flag}{1 for commodities in the package's core crop set.}
#'   \item{data_source}{Source description.}
#' }
#' @source USDA Risk Management Agency, Actuarial Data Master, records A00400
#'   and A00420, supplemented with legacy ADM files.
"fcip_recodes_commodity_groupings"

#' Insurance plan recodes
#'
#' Insurance plan names and classifications by commodity year, joined onto FCIP
#' data by [fcip_demand_data_dispatcher()].
#'
#' @format A data frame with one row per `commodity_year` and `insurance_plan_code`:
#' \describe{
#'   \item{commodity_year}{Crop (commodity) year.}
#'   \item{insurance_plan_code}{RMA insurance plan code.}
#'   \item{insurance_plan_name, insurance_plan_abbreviation}{RMA plan name and abbreviation.}
#'   \item{fcip_policy_type}{`"Basic"`, `"Endorsement"`, or `"Basic or Endorsement"`.}
#'   \item{outcome_protected}{What the plan insures, e.g. `"Yield"`, `"Revenue"`, `"Dollar"`.}
#'   \item{triger_level}{Loss trigger: `"Individual"` or `"Group"`.}
#'   \item{insurance_plan_type_rma}{RMA plan type.}
#'   \item{insurance_plan_recode}{Harmonized plan code used across years.}
#'   \item{data_source}{Source description.}
#' }
#' @source USDA Risk Management Agency, Actuarial Data Master, record A00460,
#'   supplemented with legacy ADM files.
"fcip_recodes_insurance_plan"

#' Practice recodes
#'
#' Practice names and irrigation, organic and fallow recodes by commodity year,
#' joined onto FCIP data by [fcip_demand_data_dispatcher()].
#'
#' @format A data frame with one row per `commodity_year`, `commodity_code` and
#'   `practice_code`:
#' \describe{
#'   \item{commodity_year}{Crop (commodity) year.}
#'   \item{commodity_code}{RMA commodity code.}
#'   \item{practice_code, practice_name}{RMA practice code and name.}
#'   \item{irrigation_recode}{`"IRR"`, `"NON-IRR"` or `"Other"`.}
#'   \item{organic_recode}{Organic status of the practice.}
#'   \item{fallow_recode}{`"FALLOW"` or `"CONTINUOUS CROPPING"`.}
#'   \item{data_source}{Source description.}
#' }
#' @source USDA Risk Management Agency, Actuarial Data Master, record A00510,
#'   supplemented with legacy ADM files.
"fcip_recodes_practice"

#' Premium subsidy schedule
#'
#' Premium subsidy rates by plan, coverage level, coverage type and unit
#' structure.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{commodity_year}{Crop (commodity) year.}
#'   \item{insurance_plan_code}{RMA insurance plan code.}
#'   \item{coverage_level_percent}{Coverage level, as a proportion (0.50-0.95).}
#'   \item{coverage_type_code}{RMA coverage type code (`"A"`, `"C"`, `"L"`).}
#'   \item{unit_structure_code}{RMA unit structure code, or `"ALL"`.}
#'   \item{subsidy_percent}{Premium subsidy rate, as a proportion.}
#'   \item{data_source}{Source description.}
#' }
#' @source USDA Risk Management Agency, Actuarial Data Master, supplemented with
#'   legacy ADM files.
"premium_subsidy_schedule"

#' NASS index of prices received
#'
#' Annual index of prices received by farmers, used as a deflator by
#' [fcip_demand_data_dispatcher()].
#'
#' @format A data frame with one row per year:
#' \describe{
#'   \item{commodity_year}{Year.}
#'   \item{index_for_price_recived}{Index of prices received.}
#'   \item{data_source}{Source description.}
#' }
#' @source USDA National Agricultural Statistics Service, Quick Stats.
"nassSurveyPriceRecivedIndex"

#' State cropland rental rates
#'
#' Average cropland cash rent by state and year, joined onto FCIP data by
#' [fcip_demand_data_dispatcher()].
#'
#' @format A data frame with one row per state and year:
#' \describe{
#'   \item{NAME}{State name.}
#'   \item{state_code}{State FIPS code.}
#'   \item{commodity_year}{Year.}
#'   \item{rent}{Cash rent, dollars per acre.}
#'   \item{data_source}{Source description.}
#' }
#' @source USDA National Agricultural Statistics Service, Quick Stats, via
#'   `get_state_rental_rates()`.
"nassSurveyRentalRates"
