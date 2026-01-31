#' Calibrate FCIP demand elasticities
#'
#' Fits a 2-equation FCIP demand system over an estimation window ending in
#' `calibration_year` and returns capped elasticities (-2, 0) by
#' disaggregation level using `fcip_demand_sys_estimate()`
#'
#' @param calibration_year Integer. Last year of the estimation window.
#' @param estimation_window Integer (>= 1). Number of years ending at `calibration_year`.
#' @param data `data.table`. Input panel. Must include:
#'   `commodity_year`, `commodity_code`, `drawID`, `pool`,
#'   `insurance_plan_code`, `price`,
#'   `net_reporting_level_amount`, `coverage_level_percent_aggregate`,
#'   `rent`, `county_acreage`,
#'   `total_premium_amount`, `subsidy_amount`, `liability_amount`,
#'   `tau`, `subsidy_rate_65`, `subsidy_rate_75`.
#' @param drawn_pools Optional `data.table` used to filter `data` via an inner
#'   join on intersecting columns.
#' @param disaggregate Optional character vector of grouping variables passed to
#'   the estimator (e.g., `"commodity_code"` or `c("state","commodity_code")`).
#' @param control a list of control parameters. see \code{\link{rfcipDemand_controls}}.
#' 
#' @return `data.table` with columns:
#'   `disag`, `level`, `gamma_elasticity`, `theta_elasticity`.
#'
#' @details
#' Internally, the function (i) restricts `data` to the estimation years,
#' (ii) collapses insurance plan codes and sets `price := 1` for non-RP crops,
#' (iii) constructs log variables, trend and year dummies, and (iv) estimates
#' the system with `rate` endogenous and `tau0` as the excluded instrument.
#' Estimates are truncated to (-2, 0) and returned in wide form.
#'
#' @import data.table
#' @importFrom stringr str_pad
#' @export
calibrate_fcip_demand_elasticities <- function(
    calibration_year,
    estimation_window,
    data,
    drawn_pools = NULL,
    disaggregate = NULL,
    control = rfcipDemand_controls()){

  # ensure data.table
  data <- data.table::as.data.table(data)

  if (!is.null(drawn_pools)) drawn_pools <- data.table::as.data.table(drawn_pools)

  estimation_years <- (calibration_year - estimation_window + 1):calibration_year

  # Filter to drawn pools
  if (!is.null(drawn_pools)) {
    data <- data[drawn_pools,
                 on = intersect(names(data), names(drawn_pools)),
                 nomatch = 0]

    data[, pool := paste0(stringr::str_pad(drawID,10,pad="0"), stringr::str_pad(pool,   10,pad="0"))]
  }

  data <- data[commodity_year %in% estimation_years]
  data[, n := .N, by = pool]
  data <- data[n > 1]

  # Set price to 1 for crops with no RP/RP-HPE options
  data[insurance_plan_code %in% c(1L, 90L), insurance_plan_code := 1L]
  data[insurance_plan_code %in% c(44L, 2L), insurance_plan_code := 2L]
  data[insurance_plan_code %in% c(25L, 42L, 3L), insurance_plan_code := 3L]
  data[, rp_eligible := max(as.numeric(insurance_plan_code %in% 2:3)),
       by = "commodity_code"]
  data[rp_eligible == 0, price := 1]

  # Construct variables (logs)
  data[, gamma  := log(net_reporting_level_amount/10000)]
  data[, theta  := log(coverage_level_percent_aggregate)]
  data[, price  := log(price)]
  data[, rent   := log(rent / 1000)]
  data[, county_acreage := log(county_acreage / 10000)]
  data[, rate   := log((total_premium_amount - subsidy_amount) / liability_amount)]
  data[, tau0   := log(tau * ((subsidy_rate_65 + subsidy_rate_75) / 2))]

  # coerce types
  data[, c("commodity_year","commodity_code") :=
         lapply(.SD, function(x) as.numeric(as.character(x))),
       .SDcols = c("commodity_year","commodity_code")]

  # trend & dummies
  data[, trend := commodity_year - min(commodity_year, na.rm = TRUE)]
  codes <- sort(unique(stats::na.omit(data$commodity_code)))
  data[, paste0("Crop_", codes) :=
         lapply(codes, function(u) as.integer(commodity_code == u) * trend)]
  yrs <- sort(unique(na.omit(data$commodity_year)))
  data[, paste0("year_", yrs) := lapply(yrs, function(y) as.integer(commodity_year == y))]

  # drop max-year dummy and Crop_41 if present
  cols_to_drop <- intersect(c(paste0("year_", max(data$commodity_year, na.rm = TRUE)), "Crop_41"),
                            names(data))
  if (length(cols_to_drop)) data[, (cols_to_drop) := NULL]

  # (optional but safer) drop non-finite rows before estimation
  data <- data[is.finite(gamma) & is.finite(theta) & is.finite(price) &
                 is.finite(rent) & is.finite(county_acreage) &
                 is.finite(rate) & is.finite(tau0)]

  elast <- fcip_demand_sys_estimate(
    model = list(
      name = "test", FE = FALSE,
      outcome = c("gamma","theta"),
      endogenous = "rate",
      excluded = "tau0",
      disag = disaggregate,
      included = NULL,
      partial = c("county_acreage","rent","price","trend",
                  names(data)[grepl("^year_", names(data))])
    ),
    data = data, constrained_elasticities = TRUE,
    control = control
  )

  elast[Estimate > 0 , Estimate := 0]
  elast[Estimate < -2, Estimate := -2]
  elast[, demand := paste0(demand, "_elasticity")]

  elast <- elast[, c("disag","level","demand","Estimate"), with = FALSE] |>
    tidyr::pivot_wider(names_from = demand, values_from = Estimate) |>
    data.table::as.data.table()

  elast <- elast[is.finite(gamma_elasticity)]

  return(elast)
}

