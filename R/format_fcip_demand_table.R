#' Table: Crop Insurance Demand System for US Federal Crop Insurance Pools (2001/22)
#'
#' Build a two-column, GitHub-safe panel table summarizing a crop insurance
#' demand system. The table is organized into panels for coverage level (Theta),
#' insured acres (Gamma), total protection response, a covariance matrix block,
#' and additional statistics. Coefficients are formatted as
#' \code{estimate (std. error)} with significance stars.
#'
#' Designed for README/output knitted as \code{github_document}; use with
#' \code{knitr::kable(..., format = "pipe")} to avoid HTML-only features.
#'
#' @param df A data frame containing the results with columns:
#'   \itemize{
#'     \item \code{demand} (chr): panel identifier; expected values include
#'           \code{"Theta"}, \code{"Gamma"}, and \code{"Total"}.
#'     \item \code{coef} (chr): raw coefficient/row labels (e.g., \code{"tilda_rate"},
#'           \code{"residCov_11"}, \code{"N"}).
#'     \item \code{Estimate} (dbl): point estimates.
#'     \item \code{StdError} (dbl): standard errors (may be \code{NA} for scalars).
#'     \item \code{Pvalue} (dbl): p-values used to add significance stars.
#'   }
#' @param var_labels A named character vector mapping raw names to display labels,
#' 
#' @return A tibble with two columns, \code{Variables} and \code{Estimates},
#'   where panel headers have empty \code{Estimates} to enable bolding (if rendered
#'   in HTML) and coefficients are formatted as \code{"estimate*** (se)"}.
#'
#' @export
format_fcip_demand_table <- function(df, var_labels) {
  stopifnot(all(c("demand","coef","Estimate","StdError","Pvalue") %in% names(df)))
  
  add_stars <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.01) return("***")
    if (p < 0.05) return("**")
    if (p < 0.10) return("*")
    ""
  }
  
  format_est <- function(est, se, p) {
    if (is.na(est)) return(NA_character_)
    if (is.na(se))  return(sprintf("%.3f", est))
    paste0(sprintf("%.3f", est), add_stars(p), " (", sprintf("%.3f", se), ")")
  }
  
  df_fmt <- df |>
    dplyr::mutate(Estimates = mapply(format_est, Estimate, StdError, Pvalue)) |>
    dplyr::mutate(coef = dplyr::if_else(coef %in% names(var_labels),
                                        var_labels[coef], coef))
  
  panel_labels <- c(
    Theta = "Coverage level",
    Gamma = "Insured acres",
    Total = "Total protection response"
  )
  
  covar_keys_raw <- c("residCov_11","residCov_22","residCov_12")
  other_keys_raw <- c("N","NFE","JTest","FTest")
  
  covar_labels <- unname(ifelse(covar_keys_raw %in% names(var_labels),
                                var_labels[covar_keys_raw], covar_keys_raw))
  other_labels <- unname(ifelse(other_keys_raw %in% names(var_labels),
                                var_labels[other_keys_raw], other_keys_raw))
  
  coverage <- df_fmt |>
    dplyr::filter(demand == "Theta") |>
    dplyr::select(Variables = coef, Estimates)
  
  insured <- df_fmt |>
    dplyr::filter(demand == "Gamma") |>
    dplyr::select(Variables = coef, Estimates)
  
  total <- df_fmt |>
    dplyr::filter(demand == "Total") |>
    dplyr::select(Variables = coef, Estimates)
  
  covar <- df_fmt |>
    dplyr::filter(coef %in% covar_labels) |>
    dplyr::select(Variables = coef, Estimates)
  
  other <- df_fmt |>
    dplyr::filter(coef %in% other_labels) |>
    dplyr::select(Variables = coef, Estimates)
  
  dplyr::bind_rows(
    tibble::tibble(Variables = panel_labels[["Theta"]], Estimates = ""),
    coverage,
    tibble::tibble(Variables = panel_labels[["Gamma"]], Estimates = ""),
    insured,
    tibble::tibble(Variables = panel_labels[["Total"]], Estimates = ""),
    total,
    tibble::tibble(Variables = "Covariance matrix", Estimates = ""),
    covar,
    tibble::tibble(Variables = "Additional statistics", Estimates = ""),
    other
  )
}

