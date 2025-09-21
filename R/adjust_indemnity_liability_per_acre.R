#' Adjust liability and indemnity per acre at an alternative coverage level
#'
#' Compute adjusted liability per acre and adjusted indemnity per acre
#' when moving from a baseline coverage level to a new level. The method
#' floors production-to-count at 0 and resolves five mutually exclusive
#' cases via \code{data.table::fcase()}.
#'
#' @param coverage_level_percent Numeric vector; alternative coverage level (proportion in (0,1)).
#' @param final_revenue_per_acre Numeric vector; revenue per acre at harvest (used when baseline had no indemnity and baseline coverage is lower).
#' @param baseline_coverage_level Numeric vector; baseline coverage level (proportion in (0,1)).
#' @param baseline_liability_per_acre Numeric vector; baseline liability per acre.
#' @param baseline_indemnity_per_acre Numeric vector; baseline indemnity per acre.
#'
#' @details
#' \itemize{
#'   \item \code{production_to_count = pmax(baseline_liability_per_acre - baseline_indemnity_per_acre, 0)}
#'   \item \code{adj_Liability_per_acre = (baseline_liability_per_acre / baseline_coverage_level) * coverage_level_percent}
#'   \item Indemnity cases (I1-I5) selected with \code{data.table::fcase()}.
#' }
#'
#' @return A list with:
#' \describe{
#'   \item{\code{adj_Liability_per_acre}}{Adjusted liability per acre.}
#'   \item{\code{adj_Indemnity_per_acre}}{Adjusted indemnity per acre.}
#' }
#'
#' @export
adjust_indemnity_liability_per_acre <- function(
    coverage_level_percent,
    final_revenue_per_acre,
    baseline_coverage_level,
    baseline_liability_per_acre,
    baseline_indemnity_per_acre
){
  # --- input recycling (allow lengths 1 or n) ---
  lens <- vapply(
    list(coverage_level_percent, final_revenue_per_acre, baseline_coverage_level,
         baseline_liability_per_acre, baseline_indemnity_per_acre),
    length, integer(1)
  )
  n <- max(lens)
  recycle <- function(x) {
    if (length(x) == 1) rep_len(x, n)
    else if (length(x) == n) x
    else stop("All inputs must have length 1 or the same length.", call. = FALSE)
  }
  coverage_level_percent      <- recycle(coverage_level_percent)
  final_revenue_per_acre            <- recycle(final_revenue_per_acre)
  baseline_coverage_level     <- recycle(baseline_coverage_level)
  baseline_liability_per_acre <- recycle(baseline_liability_per_acre)
  baseline_indemnity_per_acre <- recycle(baseline_indemnity_per_acre)

  # Guardrails
  if (any(baseline_coverage_level <= 0, na.rm = TRUE)) {
    warning("baseline_coverage_level contains non-positive values; division may produce Inf/NA.")
  }

  # Production to count (floor at 0)
  production_to_count <- pmax(baseline_liability_per_acre - baseline_indemnity_per_acre, 0)

  # Adjusted liability at alternative coverage
  adj_Liability_per_acre <- (baseline_liability_per_acre / baseline_coverage_level) * coverage_level_percent

  # Adjusted indemnity at alternative coverage level
  # (Cases are mutually exclusive based on baseline vs. alternative coverage
  #  and whether the baseline indemnity was zero or positive.)
  adj_Indemnity_per_acre <- data.table::fcase(
    # Higher baseline coverage; baseline had no indemnity = still no indemnity
    baseline_coverage_level >  coverage_level_percent & baseline_indemnity_per_acre == 0,
    0,

    # Higher baseline coverage; baseline had indemnity = reduce by liability change
    baseline_coverage_level >  coverage_level_percent & baseline_indemnity_per_acre >  0,
    baseline_indemnity_per_acre - (baseline_liability_per_acre - adj_Liability_per_acre),

    # Same coverage level : keep baseline indemnity
    baseline_coverage_level == coverage_level_percent,
    baseline_indemnity_per_acre,

    # Lower baseline coverage; baseline had indemnity = alt liability - production-to-count
    baseline_coverage_level <  coverage_level_percent & baseline_indemnity_per_acre >  0,
    adj_Liability_per_acre - production_to_count,

    # Lower baseline coverage; baseline had no indemnity = alt liability - revenue
    baseline_coverage_level <  coverage_level_percent & baseline_indemnity_per_acre == 0,
    adj_Liability_per_acre - final_revenue_per_acre,

    # Fallback for any unmatched/NA cases
    default = NA_real_
  )

  list(
    adj_Liability_per_acre  = adj_Liability_per_acre,
    adj_Indemnity_per_acre  = adj_Indemnity_per_acre
  )
}
