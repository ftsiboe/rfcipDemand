#' Adjust FCIP outcomes by elasticity
#'
#' Simulate FCIP outcomes (coverage level, insured acres, liability, premium,
#' subsidy, and indemnity) under an alternative premium-per-liability, using
#' elasticities for insured acres and/or coverage level.
#'
#' Assumptions:
#' \itemize{
#'   \item \code{0} - Fixed demand (coverage and acres stay at baseline).
#'   \item \code{1} - Acres respond to price (gamma), coverage fixed.
#'   \item \code{2} - Coverage responds to price (theta), acres fixed.
#'   \item \code{3} - Both acres and coverage respond.
#' }
#'
#' Price response uses percent change in price:
#' \deqn{\% \Delta p = 100 \times \left(\frac{\text{alt}}{\text{base}} - 1\right).}
#'
#' Coverage adjustments are rounded to 0.05 increments and truncated to \[0, 0.85\]
#' (values < 0.5 set to 0). Per-acre liability/indemnity are updated via
#' \code{adjust_indemnity_liability_per_acre()}.
#'
#' @param alternate_premium_per_liability Numeric. Alternative premium per dollar of liability.
#' @param insured_acres_elasticity Numeric. Elasticity of insured acres w.r.t. price (percent basis).
#' @param coverage_level_elasticity Numeric. Elasticity of coverage level w.r.t. price (percent basis).
#' @param baseline_coverage_level Numeric in (0,1]. Baseline coverage level share.
#' @param baseline_insured_acres Numeric. Baseline insured acres.
#' @param baseline_liability_per_acre Numeric. Baseline liability per acre.
#' @param baseline_premium_per_liability Numeric. Baseline premium per dollar of liability.
#' @param baseline_subsidy_per_premium Numeric in (0,1). Subsidy share of total premium.
#' @param baseline_indemnity_per_acre Numeric. Baseline indemnity per acre.
#' @param revenue_per_acre Numeric. Revenue per acre (used by the indemnity adjustment).
#' @param assumption Integer (0,1,2,3). Scenario selector (see above). Default \code{0}.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{coverage_level_percent}}{Scenario coverage level (share).}
#'   \item{\code{insured_acres}}{Scenario insured acres.}
#'   \item{\code{adj_Liability_per_acre}}{Adjusted liability per acre.}
#'   \item{\code{adj_Indemnity_per_acre}}{Adjusted indemnity per acre.}
#'   \item{\code{liability_amount}}{Total liability = acres * adj liability/acre.}
#'   \item{\code{total_premium_amount}}{Total premium = liability * alt premium/liability.}
#'   \item{\code{subsidy_amount}}{Subsidy = total premium * baseline subsidy share.}
#'   \item{\code{indemnity_amount}}{Total indemnity = acres * adj indemnity/acre.}
#'   \item{\code{price_change_pct}}{Percent price change used for elasticities.}
#' }
#'
#' @export
adjust_agent_outcomes_by_elasticity <- function(
    alternate_premium_per_liability,
    insured_acres_elasticity,
    coverage_level_elasticity,
    baseline_coverage_level,
    baseline_insured_acres,
    baseline_liability_per_acre,
    baseline_premium_per_liability,
    baseline_subsidy_per_premium,
    baseline_indemnity_per_acre,
    revenue_per_acre,
    assumption = 0
){
  stopifnot(length(assumption) == 1)

  # Percent change in price used by elasticities
  price_change_pct <- 100 * (alternate_premium_per_liability/baseline_premium_per_liability - 1)

  # Defaults (will be overwritten per assumption)
  coverage_level_percent <- baseline_coverage_level
  insured_acres          <- baseline_insured_acres

  # Acres move with price
  if(assumption == 1L) {
    insured_acres <- baseline_insured_acres * (1 + ((insured_acres_elasticity * price_change_pct)/100))
    insured_acres <- pmax(insured_acres, 0)
  }

  # Coverage level moves with price
  if(assumption == 2L) {
    coverage_level_percent <- baseline_coverage_level * (1 + ((coverage_level_elasticity * price_change_pct)/100))
    coverage_level_percent <- round(coverage_level_percent / 0.05) * 0.05
    coverage_level_percent <- ifelse(coverage_level_percent < 0.5, 0, coverage_level_percent)
    coverage_level_percent <- pmin(coverage_level_percent, 0.85)
  }

  # both Coverage level and Acres move with price
  if(assumption == 3L) {
    coverage_level_percent <- baseline_coverage_level * (1 + ((coverage_level_elasticity * price_change_pct)/100))
    coverage_level_percent <- round(coverage_level_percent / 0.05) * 0.05
    coverage_level_percent <- ifelse(coverage_level_percent < 0.5, 0, coverage_level_percent)
    coverage_level_percent <- pmin(coverage_level_percent, 0.85)

    insured_acres <- baseline_insured_acres * (1 + ((insured_acres_elasticity * price_change_pct)/100))
    insured_acres <- pmax(insured_acres, 0)
  }

  # Reset both variables to zero if either insured_acres or coverage_level_percent is missing (NA) or equals zero. 
  if(is.na(insured_acres) || insured_acres == 0 ||
     is.na(coverage_level_percent) || coverage_level_percent == 0) {
    coverage_level_percent <- 0
    insured_acres <- 0
  }
  
  # Adjust per-acre liability/indemnity for the (possibly) new coverage
  adjust_res <- adjust_indemnity_liability_per_acre(
    coverage_level_percent      = coverage_level_percent,
    revenue_per_acre            = revenue_per_acre,
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
    coverage_level_percent   = coverage_level_percent,
    insured_acres            = insured_acres,
    liability_amount         = liability_amount,
    total_premium_amount     = total_premium_amount,
    subsidy_amount           = subsidy_amount,
    indemnity_amount         = indemnity_amount,
    price_change_pct         = price_change_pct
  )
}


