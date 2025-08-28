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
    . ..final_cols ..keep_cols ALL FCIP_INSURANCE_POOL agg_level_desc c_K
    c_P c_Z c_a c_alpha c_net_acre c_u c_v c_x calculate_mode
    commodity_code commodity_name commodity_year contiguous_county_code
    contiguous_state_code county_acreage county_cd county_code
    county_fips coverage_level_dominant coverage_level_percent
    coverage_level_percent_aggregate coverage_level_percent_avg
    coverage_level_percent_max coverage_level_percent_wavg
    coverage_type_code crop_cd crop_yr delivery_type
    fcip_contiguous_county fcip_recodes_commodity_groupings
    fcip_recodes_insurance_plan fcip_recodes_practice fips fsaCropAcreage
    fsa_crop_linker fsa_planted_acres harvest_price head
    insurance_plan_code insured_area lcr liability_amount n
    nassSurvey_AREA_BEARING nassSurvey_AREA_HARVESTED
    nassSurvey_AREA_PLANTED nass_index_for_price_recived
    nass_state_rental_rates net_reporting_level_amount obs
    period_farmbill planted_acres pool potential_liability_amount
    premium_per_liability price projected_price reporting_level_type
    standardized_liability_amount state_cd state_code statisticcat_desc
    subsidy_amount subsidy_bins subsidy_per_premium subsidy_rate
    subsidy_rate_65 subsidy_rate_75 tau tau_adm tau_c tau_sob
    total_premium_amount triger_level value var
      ",
        "\\s+"
      )[[1]]
    )
  }
}




