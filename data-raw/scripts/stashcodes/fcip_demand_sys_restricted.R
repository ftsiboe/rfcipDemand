#' Optional restricted NLSUR step (when \code{restrict = TRUE})
#'
#' @description
#' If enabled and feasible, estimates a nonlinear SUR with re-parameterized
#' coefficients (negative exponents) using the optional \pkg{nlsur} package and
#' appends "restricted_" rows to the results. Skips gracefully if \pkg{nlsur}
#' is not installed or the step fails.
#'
#' @param restrict Logical flag; when \code{TRUE} attempt the restricted step.
#' @param res  Coefficient table from the unrestricted system (used to check signs).
#' @param fit  Fitted \code{systemfit} object from the unrestricted system.
#' @param data Estimation data used to fit the restricted NLSUR model.
#' @param outcome Character vector of outcome equation names (length 2 expected).
#' @param tilda_endogenous Character vector of endogenous regressors used in the
#'   tilded system (e.g., \code{"tilda_z1"}).
#' @param tilda_excluded Character vector of excluded instruments (e.g., \code{"instr_z1"}).
#' @param tilda_included Character vector of included regressors (\code{"tilda_x1"}, ...).
#'
#' @return A \code{data.frame} with rows for \code{gamma}, \code{theta}, and \code{total}
#'   labeled \code{restricted_*}, or an empty \code{data.frame} if skipped.
#'
#' @note This step uses \code{nlsur::nlsur()} if available; it is optional and
#'       should be listed under \code{Suggests} in \code{DESCRIPTION}.
#' @importFrom utils head
#' @export
fcip_demand_sys_restricted <- function(restrict, res, fit, data, outcome, tilda_endogenous, tilda_excluded, tilda_included) {
  if (!isTRUE(restrict)) return(data.frame())
  keep <- tryCatch(max(res[paste0(outcome, "_", tilda_endogenous), "Estimate"]) > 0, error = function(e) FALSE)
  if (!keep) return(data.frame())
  
  out <- tryCatch({
    strv <- coef(fit)
    eq1  <- paste0("tilda_", outcome[1], "~a0")
    eq2  <- paste0("tilda_", outcome[2], "~b0")
    
    names(strv)[names(strv) %in% paste0(outcome[1], "_(Intercept)")] <- "a0"
    names(strv)[names(strv) %in% paste0(outcome[2], "_(Intercept)")] <- "b0"
    
    for (x in seq_along(tilda_excluded)) {
      eq1 <- paste0(eq1, "-exp(ae", x, ")*", tilda_excluded[x])
      eq2 <- paste0(eq2, "-exp(be", x, ")*", tilda_excluded[x])
      names(strv)[names(strv) %in% paste0(outcome[1], "_", tilda_endogenous[x])] <- paste0("ae", x)
      names(strv)[names(strv) %in% paste0(outcome[2], "_", tilda_endogenous[x])] <- paste0("be", x)
    }
    for (x in seq_along(tilda_included)) {
      eq1 <- paste0(eq1, "+ai", x, "*", tilda_included[x])
      eq2 <- paste0(eq2, "+bi", x, "*", tilda_included[x])
      names(strv)[names(strv) %in% paste0(outcome[1], "_", tilda_included[x])] <- paste0("ai", x)
      names(strv)[names(strv) %in% paste0(outcome[2], "_", tilda_included[x])] <- paste0("bi", x)
    }
    
    # simple numeric "lag" for start values for ae*/be*
    shift1 <- function(v) c(NA_real_, head(v, -1))
    sv <- strv
    sv[grepl("^ae\\d+$", names(sv))] <- shift1(abs(sv[grepl("^ae\\d+$", names(sv))]))
    sv[grepl("^be\\d+$", names(sv))] <- shift1(abs(sv[grepl("^be\\d+$", names(sv))]))
    
    fitc <- nlsur::nlsur(eqns = list(stats::as.formula(eq1), stats::as.formula(eq2)),
                         data = data, type = "IFGNLS", startvalues = sv, maxiter = 50)
    cf   <- coef(fitc)
    cf   <- cf[grepl("^a[e]\\d+$|^b[e]\\d+$", names(cf))]
    cf   <- -exp(cf); cf[3] <- cf[1] + cf[2] + cf[1] * cf[2]
    
    data.frame(
      demand   = c("gamma","theta","total"),
      coef     = paste0("restricted_", tilda_endogenous),
      Estimate = cf, StdError = NA, Zvalue = NA, Pvalue = NA
    )
  }, error = function(e) data.frame())
  out
}