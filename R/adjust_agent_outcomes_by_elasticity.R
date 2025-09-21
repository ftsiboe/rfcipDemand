#' Adjust FCIP outcomes by price elasticities
#'
#' Simulate FCIP outcomes-coverage level, insured acres, liability, premium,
#' subsidy, and indemnity-under an alternative premium-per-liability using
#' elasticities for insured acres and/or coverage level.
#'
#' Assumptions (set via `assumption`):
#' \itemize{
#'   \item \code{0}: Fixed demand - coverage and acres stay at baseline.
#'   \item \code{1}: Acres respond to price (gamma); coverage fixed.
#'   \item \code{2}: Coverage responds to price (theta); acres fixed.
#'   \item \code{3}: Both acres and coverage respond.
#' }
#'
#' Price response is applied using the percent change in price:
#' \deqn{\%\Delta p = 100 \times \left(\frac{\text{alt}}{\text{base}} - 1\right),}
#' so that, for quantity \code{q} with elasticity \code{e_q},
#' \deqn{q_{\text{new}} = q_{\text{base}} \left[1 + \left(e_q \times \frac{\%\Delta p}{100}\right)\right].}
#'
#' Coverage adjustments are rounded to the nearest 0.05 and truncated to \((0,\,0.85)\).
#' Values less than 0.50 are set to \code{0} (i.e., no coverage). Per-acre liability
#' and indemnity are updated via \code{\link{adjust_indemnity_liability_per_acre}}.
#'
#' Optional schedules allow coverage-specific scaling:
#' \itemize{
#'   \item \code{premium_subsidy_schedule}: length-8 numeric for coverage levels
#'         \code{0.50, 0.55, ..., 0.85}; multiplies \code{baseline_subsidy_per_premium}
#'         at the scenario coverage.
#'   \item \code{rate_differential_schedule}: length-8 numeric for the same grid;
#'         multiplies \code{alternate_premium_per_liability} at the scenario coverage.
#' }
#'
#' Missing or zero coverage/acres: if either \code{insured_acres} or
#' \code{coverage_level_percent} is \code{NA} or \code{0} after adjustment, both are
#' reset to \code{0} and all dollar outcomes become \code{0}.
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
#' @param final_revenue_per_acre Numeric. Revenue per acre (used by the indemnity adjustment).
#' @param assumption Integer (0,1,2,3). Scenario selector (see above). Default \code{0}.
#' @param premium_subsidy_schedule Optional numeric vector of length 8 corresponding to
#'   coverage levels \code{0.50, 0.55, ..., 0.85}. Multiplicative factors applied to
#'   \code{baseline_subsidy_per_premium} at the scenario coverage. Defaults to 1's.
#' @param rate_differential_schedule Optional numeric vector of length 8 corresponding to
#'   coverage levels \code{0.50, 0.55, ..., 0.85}. Multiplicative factors applied to
#'   \code{alternate_premium_per_liability} at the scenario coverage. Defaults to 1's.
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
#' @details
#' Elasticities are applied multiplicatively to the baseline quantities using the
#' computed \code{price_change_pct}. Coverage is then snapped to the 0.05 grid and
#' truncated. When schedules are supplied, the alternative premium-per-liability and/or
#' the baseline subsidy share are scaled at the resulting coverage level.
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
    final_revenue_per_acre,
    assumption = 0,
    premium_subsidy_schedule = NULL,
    rate_differential_schedule  = NULL
){
 
  stopifnot(
    is.finite(baseline_premium_per_liability),
    baseline_premium_per_liability > 0,
    length(assumption) == 1L,
    assumption %in% c(0L,1L,2L,3L)
  )

  # Default schedules
  if (is.null(premium_subsidy_schedule)) {
    premium_subsidy_schedule <- rep(1, length(seq(0.50, 0.85, 0.05)))
  }
  if (is.null(rate_differential_schedule)) {
    rate_differential_schedule <- rep(1, length(seq(0.50, 0.85, 0.05)))
  }
  
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
    return(list(
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
      !isTRUE(all.equal(coverage_level_percent, baseline_coverage_level)) &&
      coverage_level_percent >= 0.50) {
    
    schedule <- data.table::data.table(
      coverage = seq(0.50, 0.85, 0.05),
      sp = premium_subsidy_schedule,
      rd = rate_differential_schedule
    )
    
    sp_base <- schedule[coverage == baseline_coverage_level, sp]
    sp_scn  <- schedule[coverage == coverage_level_percent, sp]
    rd_base <- schedule[coverage == baseline_coverage_level, rd]
    rd_scn  <- schedule[coverage == coverage_level_percent, rd]
    
    if (length(sp_base) && length(sp_scn) && length(rd_base) && length(rd_scn)) {
      adjust_premium_subsidy   <- sp_scn / sp_base
      adjust_rate_differential <- rd_scn / rd_base
      
      alternate_premium_per_liability <- alternate_premium_per_liability * adjust_rate_differential
      baseline_subsidy_per_premium    <- baseline_subsidy_per_premium    * adjust_premium_subsidy
    }
  }

  # Adjust per-acre liability and indemnity
  adjust_res <- adjust_indemnity_liability_per_acre(
    coverage_level_percent      = coverage_level_percent,
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
    coverage_level_percent   = coverage_level_percent,
    insured_acres            = insured_acres,
    liability_amount         = liability_amount,
    total_premium_amount     = total_premium_amount,
    subsidy_amount           = subsidy_amount,
    indemnity_amount         = indemnity_amount,
    price_change_pct         = price_change_pct
  )
}


