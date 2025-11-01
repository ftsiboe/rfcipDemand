#' Adjust FCIP outcomes by price elasticities
#'
#' Simulates changes in Federal Crop Insurance Program (FCIP) outcomes-
#' coverage level, insured acres, liability, premium, subsidy, and indemnity-
#' under an alternative premium-per-liability using elasticities of insured
#' acres and/or coverage level.
#'
#' ## Assumptions (set via `assumption`)
#' \itemize{
#'   \item \code{0}: Fixed demand - both coverage and acres remain at baseline.
#'   \item \code{1}: Acres respond to price (elasticity gamma); coverage fixed.
#'   \item \code{2}: Coverage responds to price (elasticity tetha); acres fixed.
#'   \item \code{3}: Both acres and coverage respond.
#' }
#'
#' ## Price response
#' Price response is applied using the percent change in price:
#' \deqn{\%\Delta p = 100 \times \left(\frac{\text{alt}}{\text{base}} - 1\right),}
#' and for any baseline quantity \eqn{q} with elasticity \eqn{e_q}:
#' \deqn{q_{\text{new}} = q_{\text{base}} \left[1 + \left(e_q \times \frac{\%\Delta p}{100}\right)\right].}
#'
#' ## Coverage adjustment
#' Coverage is rounded to the nearest 0.05 grid and truncated to the range
#' \eqn{(0, 0.85)}. Values less than 0.50 are set to \code{0} (i.e., no
#' coverage). Coverage type code is set to \code{"A"} for buy-up coverage and
#' \code{"C"} for CAT coverage. For CAT policies, scenario coverage levels at
#' 0.50/0.55 are adjusted to reflect CAT rules.
#'
#' Per-acre liability and indemnity are updated using
#' \code{\link{adjust_indemnity_liability_per_acre}}.
#'
#' ## Optional schedules
#' Coverage-specific scaling is possible via:
#' \itemize{
#'   \item \code{premium_subsidy_schedule}: length-8 numeric for coverage levels
#'         \code{0.50, 0.55, ..., 0.85}. Multiplies the
#'         \code{baseline_subsidy_per_premium} at the scenario coverage.
#'   \item \code{rate_differential_schedule}: length-8 numeric for the same grid.
#'         Multiplies the \code{alternate_premium_per_liability} at the scenario coverage.
#' }
#'
#' ## Missing or zero inputs
#' If either \code{insured_acres} or \code{coverage_level_percent} is \code{NA}
#' or \code{0} after adjustment, both are reset to \code{0} and all dollar
#' outcomes are set to \code{0}.
#'
#' @param alternate_premium_per_liability Numeric. Alternative premium per dollar of liability.
#' @param insured_acres_elasticity Numeric. Elasticity of insured acres w.r.t. price (percent basis).
#' @param coverage_level_elasticity Numeric. Elasticity of coverage level w.r.t. price (percent basis).
#' @param baseline_coverage_type Character. Baseline coverage type code (\code{"A"} or \code{"C"}).
#' @param baseline_coverage_level Numeric in (0,1]. Baseline coverage level share.
#' @param baseline_insured_acres Numeric. Baseline insured acres.
#' @param baseline_liability_per_acre Numeric. Baseline liability per acre.
#' @param baseline_premium_per_liability Numeric. Baseline premium per dollar of liability.
#' @param baseline_subsidy_per_premium Numeric in (0,1). Subsidy share of total premium.
#' @param baseline_indemnity_per_acre Numeric. Baseline indemnity per acre.
#' @param baseline_subsidy_percent Numeric. Baseline subsidy percent.
#' @param baseline_rate_differential Numeric. Baseline rate differential factor.
#' @param final_revenue_per_acre Numeric. Revenue per acre (used for indemnity adjustment).
#' @param assumption Integer (0,1,2,3). Scenario selector (see above). Default \code{0}.
#' @param premium_subsidy_schedule Optional numeric vector of length 8 corresponding to
#'   coverage levels \code{0.50, 0.55, ..., 0.85}. Multiplicative factors applied to
#'   \code{baseline_subsidy_per_premium} at the scenario coverage. Defaults to 1's.
#' @param rate_differential_schedule Optional numeric vector of length 8 corresponding to
#'   coverage levels \code{0.50, 0.55, ..., 0.85}. Multiplicative factors applied to
#'   \code{alternate_premium_per_liability} at the scenario coverage. Defaults to 1's.
#' 
#' @return A list with elements:
#' \describe{
#'   \item{\code{coverage_type_code}}{Scenario coverage type (\code{"A"} or \code{"C"}).}
#'   \item{\code{coverage_level_percent}}{Scenario coverage level share.}
#'   \item{\code{insured_acres}}{Scenario insured acres.}
#'   \item{\code{liability_amount}}{Total liability = acres * adjusted liability per acre.}
#'   \item{\code{total_premium_amount}}{Total premium = liability * alt premium/liability.}
#'   \item{\code{subsidy_amount}}{Subsidy = total premium * subsidy share.}
#'   \item{\code{indemnity_amount}}{Total indemnity = acres * adjusted indemnity per acre.}
#'   \item{\code{price_change_pct}}{Percent price change used for elasticity adjustments.}
#' }
#'
#' @details
#' Elasticities are applied multiplicatively to baseline quantities. Coverage
#' levels are snapped to the 0.05 grid and truncated. When schedules are
#' supplied, subsidy shares and premium rates are rescaled relative to the
#' baseline coverage before applying to the scenario coverage.
#'
#' @import data.table
#' @export
adjust_agent_outcomes_by_elasticity <- function(
    alternate_premium_per_liability,
    insured_acres_elasticity,
    coverage_level_elasticity,
    baseline_coverage_type,
    baseline_coverage_level,
    baseline_insured_acres,
    baseline_liability_per_acre,
    baseline_premium_per_liability,
    baseline_subsidy_per_premium,
    baseline_indemnity_per_acre,
    baseline_subsidy_percent,
    baseline_rate_differential,
    final_revenue_per_acre,
    assumption = 0,
    premium_subsidy_schedule = NULL,
    rate_differential_schedule  = NULL
){
  
  # Default schedules
  if(is.null(premium_subsidy_schedule)) premium_subsidy_schedule <- rep(1, 8L)
  if(is.null(rate_differential_schedule)) rate_differential_schedule <- rep(1, 8L)
  
  # Validations
  stopifnot(is.numeric(premium_subsidy_schedule), length(premium_subsidy_schedule) == 8)
  stopifnot(is.numeric(rate_differential_schedule), length(rate_differential_schedule) == 8)
  stopifnot(is.finite(alternate_premium_per_liability), alternate_premium_per_liability >= 0)
  stopifnot(is.finite(baseline_premium_per_liability), baseline_premium_per_liability > 0)
  stopifnot(is.finite(baseline_coverage_level), baseline_coverage_level > 0, baseline_coverage_level <= 1)
  stopifnot(length(assumption) == 1L, assumption %in% c(0L,1L,2L,3L))
  if(assumption > 1){
    stopifnot(baseline_coverage_level %in% seq(0.50, 0.85, 0.05))
  }
  
  # Percent change in price used by elasticities
  price_change_pct <- 100 * (alternate_premium_per_liability/baseline_premium_per_liability - 1)

  # Defaults (will be overwritten per assumption)
  coverage_type_code <- baseline_coverage_type
  coverage_level_percent <- baseline_coverage_level
  insured_acres <- baseline_insured_acres

  # Acres move with price
  if(assumption %in% c(1L, 3L)){
    insured_acres <- baseline_insured_acres*(1 + ((insured_acres_elasticity * price_change_pct)/100))
    insured_acres <- pmax(insured_acres, 0)
  }

  # Coverage level moves with price
  if(assumption %in% c(2L, 3L)){
    coverage_level_percent <- baseline_coverage_level*(1 + (coverage_level_elasticity * price_change_pct)/100)
    coverage_level_percent <- normalize_coverage(coverage_level_percent)
    coverage_type_code     <- if(coverage_level_percent == 0) "" else if (coverage_level_percent < 0.50) "C" else "A"
    coverage_level_percent <- if(coverage_level_percent >0 & coverage_level_percent < 0.50) 0.50 else coverage_level_percent
  }
  
  # Reset both variables to zero if either insured_acres or coverage_level_percent is missing (NA)
  if (is.na(insured_acres) || insured_acres == 0 ||
      is.na(coverage_level_percent) || coverage_level_percent == 0) {
    return(list(
      coverage_type_code     = "",
      coverage_level_percent = 0,
      insured_acres          = 0,
      liability_amount       = 0,
      total_premium_amount   = 0,
      subsidy_amount         = 0,
      indemnity_amount       = 0,
      price_change_pct       = price_change_pct
    ))
  }
  
  # Adjust rates/subsidy for the new coverage level
  if (!is.na(coverage_level_percent) &&
      !isTRUE(all.equal(coverage_level_percent, baseline_coverage_level))) {

    schedule <- data.table::data.table(
      coverage = seq(0.50, 0.85, 0.05),
      sp = premium_subsidy_schedule,
      rd = rate_differential_schedule
    )
    
    alternate_subsidy_percent   <- schedule[coverage == coverage_level_percent, sp]
    alternate_rate_differential <- schedule[coverage == coverage_level_percent, rd]
    
    if(length(baseline_subsidy_percent) && length(alternate_subsidy_percent) && length(baseline_rate_differential) && length(alternate_rate_differential)){
      adjust_premium_subsidy   <- alternate_subsidy_percent / baseline_subsidy_percent
      adjust_rate_differential <- alternate_rate_differential / baseline_rate_differential
      
      alternate_premium_per_liability <- alternate_premium_per_liability * adjust_rate_differential
      baseline_subsidy_per_premium    <- baseline_subsidy_per_premium    * adjust_premium_subsidy
    }
  }

  # Reconcile CAT vs. buy-up coverage transitions
  price_election_factor <- get_price_election_factor( coverage_type_code, baseline_coverage_type)
  
  # Adjust per-acre liability and indemnity
  adjust_res <- adjust_indemnity_liability_per_acre(
    coverage_level_percent      = coverage_level_percent*price_election_factor,
    final_revenue_per_acre      = final_revenue_per_acre,
    baseline_coverage_level     = baseline_coverage_level,
    baseline_liability_per_acre = baseline_liability_per_acre,
    baseline_indemnity_per_acre = baseline_indemnity_per_acre
  )

  adj_Liability_per_acre  <- adjust_res$adj_Liability_per_acre
  adj_Indemnity_per_acre  <- adjust_res$adj_Indemnity_per_acre

  # Totals
  liability_amount       <- adj_Liability_per_acre * insured_acres
  total_premium_amount   <- alternate_premium_per_liability * liability_amount
  subsidy_amount         <- baseline_subsidy_per_premium*total_premium_amount
  indemnity_amount       <- adj_Indemnity_per_acre * insured_acres

  list(
    coverage_type_code       = coverage_type_code,
    coverage_level_percent   = coverage_level_percent,
    insured_acres            = insured_acres,
    liability_amount         = liability_amount,
    total_premium_amount     = total_premium_amount,
    subsidy_amount           = subsidy_amount,
    indemnity_amount         = indemnity_amount,
    price_change_pct         = price_change_pct
  )
}


#' Internal helper: Reconcile CAT vs. buy-up coverage transitions
#'
#' Computes the price election factor adjustment when moving between CAT ("C")
#' and buy-up ("A") coverage types. Used to scale per-acre liability and indemnity
#' without mutating the coverage share itself.
#'
#' Rules:
#' \itemize{
#'   \item Scenario = CAT, Baseline = Buy-up -> factor = 1/0.55
#'   \item Scenario = CAT, Baseline = CAT -> factor = 1
#'   \item Scenario = Buy-up, Baseline = CAT -> factor = 0.55
#'   \item All other transitions -> factor = 1
#' }
#'
#' @param curr Character scalar. Scenario coverage type code ("A" or "C").
#' @param base Character scalar. Baseline coverage type code ("A" or "C").
#'
#' @return Numeric scalar, the price election adjustment factor.
#'
#' @keywords internal
#' @noRd
get_price_election_factor <- function(curr, base) {
  if (curr == "C" && base == "A") return(1/0.55)
  if (curr == "C" && base == "C") return(1)
  if (curr == "A" && base == "C") return(0.55)
  1
}

#' Internal helper: Normalize coverage levels
#'
#' Snaps a coverage level to the nearest 0.05 grid, truncates at 0.85,
#' and enforces a minimum of 0.50. Values below 0.50 are set to 0.50.
#'
#' @param x Numeric scalar or vector. Coverage level(s) expressed as shares (0-1).
#'
#' @return Numeric scalar or vector. Adjusted coverage level(s) rounded to the
#'   0.05 grid, truncated to 0.85, and floored at 0.50.
#'
#' @keywords internal
#' @noRd
normalize_coverage <- function(x) {
  x <- round(x / 0.05) * 0.05
  x <- pmin(x, 0.85)
  ifelse(x < 0.50, 0, x)
}
