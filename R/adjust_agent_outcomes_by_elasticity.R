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
#' The elasticities are estimated with respect to the farmer-paid premium rate,
#' so the price signal is the percent change in the farmer-paid rate,
#' premium per liability times one minus the subsidy share:
#' \deqn{\%\Delta p = 100 \times \left(\frac{r_{alt}(1 - s_{alt})}{r_{base}(1 - s_{base})} - 1\right),}
#' where \eqn{s_{alt}} is \code{alternate_subsidy_per_premium} (defaults to the
#' baseline share, in which case this equals the percent change in the total
#' rate). For any baseline quantity \eqn{q} with elasticity \eqn{e_q}:
#' \deqn{q_{\text{new}} = q_{\text{base}} \left[1 + \left(e_q \times \frac{\%\Delta p}{100}\right)\right].}
#' If the baseline farmer-paid rate is zero (fully subsidized), there is no
#' price signal and \code{price_change_pct} is \code{0}.
#'
#' ## Coverage adjustment
#' Buy-up coverage is rounded to the nearest 0.05 grid and capped at 0.85.
#' A buy-up policy whose coverage falls below 0.50 moves to CAT: coverage type
#' \code{"C"}, coverage level 0.50 at 55\% of the price, with the premium fully
#' subsidized.
#'
#' **CAT policies never leave CAT.** Because CAT premium is fully subsidized,
#' a baseline CAT policy does not respond to the price change: its coverage and
#' insured acres stay at baseline under every assumption.
#'
#' Per-acre liability and indemnity are updated using
#' \code{\link{adjust_indemnity_liability_per_acre}}. Moves between CAT and buy-up
#' rescale liability by the price election: CAT liability is 50\% coverage at
#' 55\% of the price, so buy-up to CAT multiplies by 0.55.
#'
#' ## Optional schedules
#' Coverage-specific scaling is possible via:
#' \itemize{
#'   \item \code{premium_subsidy_schedule}: length-8 numeric for coverage levels
#'         \code{0.50, 0.55, ..., 0.85}. Multiplies the subsidy share at the
#'         scenario coverage, relative to \code{baseline_subsidy_percent}.
#'   \item \code{rate_differential_schedule}: length-8 numeric for the same grid.
#'         Multiplies the \code{alternate_premium_per_liability} at the scenario coverage.
#' }
#'
#' ## Missing or zero inputs
#' If \code{insured_acres} is \code{NA} or \code{0}, or the coverage level is
#' \code{NA}, after adjustment, coverage and acres are reset to \code{0} and all
#' dollar outcomes are set to \code{0}.
#'
#' @param alternate_premium_per_liability Numeric. Alternative premium per dollar of liability.
#' @param insured_acres_elasticity Numeric. Elasticity of insured acres w.r.t. price (percent basis).
#' @param coverage_level_elasticity Numeric. Elasticity of coverage level w.r.t. price (percent basis).
#' @param baseline_coverage_type Character. Baseline coverage type code (\code{"A"} or \code{"C"}).
#' @param baseline_coverage_level Numeric in (0,1]. Baseline coverage level share.
#' @param baseline_insured_acres Numeric. Baseline insured acres.
#' @param baseline_liability_per_acre Numeric. Baseline liability per acre.
#' @param baseline_premium_per_liability Numeric. Baseline premium per dollar of liability.
#' @param baseline_subsidy_per_premium Numeric in \[0,1\]. Subsidy share of total premium.
#' @param baseline_indemnity_per_acre Numeric. Baseline indemnity per acre.
#' @param baseline_subsidy_percent Numeric. Baseline subsidy percent.
#' @param baseline_rate_differential Numeric. Baseline rate differential factor.
#' @param final_revenue_per_acre Numeric. Revenue per acre (used for indemnity adjustment).
#' @param assumption Integer (0,1,2,3). Scenario selector (see above). Default \code{0}.
#' @param premium_subsidy_schedule Optional numeric vector of length 8 corresponding to
#'   coverage levels \code{0.50, 0.55, ..., 0.85}. Multiplicative factors applied to
#'   the subsidy share at the scenario coverage. Defaults to 1's.
#' @param rate_differential_schedule Optional numeric vector of length 8 corresponding to
#'   coverage levels \code{0.50, 0.55, ..., 0.85}. Multiplicative factors applied to
#'   \code{alternate_premium_per_liability} at the scenario coverage. Defaults to 1's.
#' @param alternate_subsidy_per_premium Optional numeric in \[0,1\]. Subsidy share of
#'   total premium in the scenario, at the baseline coverage level. Defaults to
#'   \code{baseline_subsidy_per_premium} (no subsidy change).
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
#'   \item{\code{price_change_pct}}{Percent change in the farmer-paid rate used for elasticity adjustments.}
#' }
#'
#' @details
#' Elasticities are applied multiplicatively to baseline quantities. Coverage
#' levels are snapped to the 0.05 grid and capped. When schedules are
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
    rate_differential_schedule  = NULL,
    alternate_subsidy_per_premium = NULL
){

  # Default schedules
  if(is.null(premium_subsidy_schedule)) premium_subsidy_schedule <- rep(1, 8L)
  if(is.null(rate_differential_schedule)) rate_differential_schedule <- rep(1, 8L)
  if(is.null(alternate_subsidy_per_premium)) alternate_subsidy_per_premium <- baseline_subsidy_per_premium

  # Validations
  stopifnot(is.numeric(premium_subsidy_schedule), length(premium_subsidy_schedule) == 8)
  stopifnot(is.numeric(rate_differential_schedule), length(rate_differential_schedule) == 8)
  stopifnot(is.finite(alternate_premium_per_liability), alternate_premium_per_liability >= 0)
  stopifnot(is.finite(baseline_premium_per_liability), baseline_premium_per_liability > 0)
  stopifnot(is.finite(baseline_coverage_level), baseline_coverage_level > 0, baseline_coverage_level <= 1)
  stopifnot(is.finite(alternate_subsidy_per_premium),
            alternate_subsidy_per_premium >= 0, alternate_subsidy_per_premium <= 1)
  stopifnot(length(assumption) == 1L, assumption %in% c(0L,1L,2L,3L))
  if(assumption > 1){
    stopifnot(round(baseline_coverage_level, 2) %in% round(seq(0.50, 0.85, 0.05), 2))
  }

  # Percent change in the farmer-paid rate used by elasticities
  baseline_farmer_rate  <- baseline_premium_per_liability  * (1 - baseline_subsidy_per_premium)
  alternate_farmer_rate <- alternate_premium_per_liability * (1 - alternate_subsidy_per_premium)
  price_change_pct <- if (isTRUE(baseline_farmer_rate > 0)) {
    100 * (alternate_farmer_rate / baseline_farmer_rate - 1)
  } else {
    0
  }

  # Defaults (will be overwritten per assumption)
  coverage_type_code     <- baseline_coverage_type
  coverage_level_percent <- baseline_coverage_level
  insured_acres          <- baseline_insured_acres
  subsidy_per_premium    <- alternate_subsidy_per_premium

  # CAT never leaves CAT: premium is fully subsidized, so no price response
  responds <- !identical(as.character(baseline_coverage_type), "C")

  # Acres move with price
  if(responds && assumption %in% c(1L, 3L)){
    insured_acres <- baseline_insured_acres*(1 + ((insured_acres_elasticity * price_change_pct)/100))
    insured_acres <- pmax(insured_acres, 0)
  }

  # Coverage level moves with price; buy-up below 0.50 moves to CAT
  if(responds && assumption %in% c(2L, 3L)){
    coverage_level_percent <- baseline_coverage_level*(1 + (coverage_level_elasticity * price_change_pct)/100)
    coverage_level_percent <- normalize_coverage(coverage_level_percent)
    if (!is.na(coverage_level_percent)) {
      if (coverage_level_percent < 0.50) {
        coverage_type_code     <- "C"
        coverage_level_percent <- 0.50
      } else {
        coverage_type_code     <- "A"
      }
    }
  }

  # Reset to zero if acres are missing/zero or coverage is missing
  if (is.na(insured_acres) || insured_acres == 0 || is.na(coverage_level_percent)) {
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
  if (!isTRUE(all.equal(coverage_level_percent, baseline_coverage_level))) {

    schedule <- data.table::data.table(
      coverage = round(seq(0.50, 0.85, 0.05), 2),
      sp = premium_subsidy_schedule,
      rd = rate_differential_schedule
    )

    alternate_subsidy_percent   <- schedule[coverage == round(coverage_level_percent, 2), sp]
    alternate_rate_differential <- schedule[coverage == round(coverage_level_percent, 2), rd]

    if(length(baseline_subsidy_percent) && length(alternate_subsidy_percent) && length(baseline_rate_differential) && length(alternate_rate_differential)){
      adjust_premium_subsidy   <- alternate_subsidy_percent / baseline_subsidy_percent
      adjust_rate_differential <- alternate_rate_differential / baseline_rate_differential

      alternate_premium_per_liability <- alternate_premium_per_liability * adjust_rate_differential
      subsidy_per_premium             <- subsidy_per_premium * adjust_premium_subsidy
    }
  }

  # A move to CAT is fully subsidized
  if (identical(coverage_type_code, "C") && !identical(as.character(baseline_coverage_type), "C")) {
    subsidy_per_premium <- 1
  }

  # Reconcile CAT vs. buy-up coverage transitions
  price_election_factor <- get_price_election_factor(coverage_type_code, baseline_coverage_type)

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
  subsidy_amount         <- subsidy_per_premium * total_premium_amount
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
#' and buy-up ("A") coverage types. CAT liability is 50\% coverage at 55\% of the
#' price, so it is scaled to and from buy-up by 0.55. Used to scale per-acre
#' liability and indemnity without mutating the coverage share itself.
#'
#' Rules:
#' \itemize{
#'   \item Scenario = CAT, Baseline = Buy-up -> factor = 0.55
#'   \item Scenario = Buy-up, Baseline = CAT -> factor = 1/0.55
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
  if (curr == "C" && base == "A") return(0.55)
  if (curr == "A" && base == "C") return(1/0.55)
  1
}

#' Internal helper: Normalize coverage levels
#'
#' Snaps a coverage level to the nearest 0.05 grid and caps it at 0.85.
#' Values below 0.50 are returned as they are; the caller moves them to CAT.
#'
#' @param x Numeric scalar or vector. Coverage level(s) expressed as shares (0-1).
#'
#' @return Numeric scalar or vector, rounded to the 0.05 grid and capped at 0.85.
#'
#' @keywords internal
#' @noRd
normalize_coverage <- function(x) {
  x <- round(x / 0.05) * 0.05
  round(pmin(x, 0.85), 2)
}
