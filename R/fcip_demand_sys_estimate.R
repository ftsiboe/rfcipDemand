#' Prepare data for analysis 
#'
#' @description
#' Drops incomplete/invalid rows, removes constant partials, and optionally demeans via a fixed-effects helper.
#'
#' @param data Estimation dataset that already contains all columns referenced by `fields`.
#' @param fields Named list: `outcome`, `endogenous`, `included`, `excluded` (opt),
#'   `partial` (opt), `FE` (logical), `disag` (column name).
#' @return A list: `data` (prepped), `NFE` (number of FE), `partial` (possibly reduced).
#' @import data.table
#' @importFrom stats complete.cases sd
#' @export
fcip_demand_sys_prep <- function(data, fields) {
  with(fields, {
    # copy data
    dd <- copy(data)
    
    # Vars we require (unique + only those present)
    need <- unique(c(outcome, endogenous, included, excluded, partial))
    need <- need[need %in% names(dd)]
    if (length(need) == 0L) {
      return(list(data = dd[0, , drop = FALSE], NFE = 0L, partial = partial))
    }
    
    # Complete cases on required variables
    dd <- dd[stats::complete.cases(dd[, need, with = FALSE]), , drop = FALSE]
    
    # Sanity filters for Inf/NA; drop constant partials
    if (!is.null(partial) && length(partial)) {
      for (xx in partial) {
        if (!xx %in% names(dd)) next
        keep <- !dd[[xx]] %in% c(Inf, -Inf, NA, NaN)
        dd <- dd[keep, , drop = FALSE]
        if (length(dd[[xx]]) && isTRUE(all.equal(stats::sd(dd[[xx]], na.rm = TRUE), 0))) {
          partial <- partial[partial != xx]
        }
      }
    }
    for (yy in unique(c(outcome, endogenous, included, excluded))) {
      if (!yy %in% names(dd)) next
      dd <- dd[!dd[[yy]] %in% c(Inf, -Inf, NA, NaN), , drop = FALSE]
    }
    
    # Demean if FE (expects user-supplied helper `fixed_effect_model_data_prep`)
    NFE <- 0L
    if (isTRUE(FE)) {
      dm <- fixed_effect_model_data_prep(
        data   = dd,
        varlist= unique(c(endogenous, included, excluded, partial)),
        panel  = "pool",
        time   = "commodity_year",
        wvar   = NULL,
        output = outcome
      )
      NFE <- dm$NFE
      dd  <- as.data.table(dm$data)
      need_present <- need[need %in% names(dd)]
      if (length(need_present)) {
        dd <- dd[stats::complete.cases(dd[, need_present, with = FALSE]), , drop = FALSE]
      }
    }
    
    list(data = dd, NFE = NFE, partial = partial)
  })
}


#' Residualize ("partial out") and build tilded / instrument variables
#'
#' @description
#' If excluded instruments exist, runs first-stage OLS for each endogenous
#' variable \code{e}: \code{e ~ 1 + partial + included + excluded}, storing the
#' fitted values as \code{instr_e}. If \code{partial} is non-empty, it then
#' regresses \code{instr_e ~ 1 + partial} and replaces
#' \code{instr_e <- instr_e - fitted(instr_e ~ partial)} (i.e., removes the
#' partial component; conceptually \eqn{\widehat{\mathrm{instr}}_e(\text{partial})}).
#'
#' Outcomes, included, and endogenous variables are residualized on
#' \code{partial} to create \code{tilda_<var>} (or copied if \code{partial} is empty).
#'
#' @param data A \code{data.frame}/\code{data.table} with referenced variables.
#' @param fields List with: \code{outcome}, \code{endogenous}, \code{included},
#'   optional \code{excluded}, optional \code{partial}.
#' @param partial_override Optional character vector to override \code{fields$partial}.
#' @return List with \code{data}, \code{tilda_included}, \code{tilda_endogenous},
#'   \code{tilda_excluded}.
#'
#' @details
#' Uses defensive checks so absent columns are ignored (with a warning) rather than erroring.
#' If \code{partial} is empty, residualization is a no-op and \code{tilda_*} simply copy
#' the originals. Formulas are constructed via \code{stats::reformulate()} to avoid
#' paste/quoting pitfalls.
#'
#' @importFrom stats lm reformulate residuals predict as.formula
#' @export
fcip_demand_sys_partial <- function(data, fields, partial_override = NULL) {
  stopifnot(is.data.frame(data))
  
  with(fields, {
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    partial_now <- partial_override %||% partial
    if (!is.null(partial_now) && !length(partial_now)) partial_now <- NULL
    
    present <- function(v) {
      if (is.null(v)) return(character())
      unique(intersect(v, names(data)))
    }
    
    # resolve columns that actually exist
    inc_now <- present(included)
    end_now <- present(endogenous)
    par_now <- present(partial_now)
    exc_now <- present(excluded)
    
    # warn about missing columns (non-fatal)
    missing_cols <- unique(c(
      setdiff(included %||% character(), names(data)),
      setdiff(endogenous %||% character(), names(data)),
      setdiff(excluded %||% character(), names(data)),
      setdiff(partial_now %||% character(), names(data))
    ))
    if (length(missing_cols)) {
      warning("Ignoring missing columns: ", paste(missing_cols, collapse = ", "))
    }
    
    # tiny helpers
    add_col <- function(df, nm, val) { df[[nm]] <- val; df }
    make_formula <- function(lhs, rhs_vars) {
      # use reformulate to avoid paste bugs; NULL/character(0) -> intercept-only
      stats::as.formula(stats::reformulate(rhs_vars, response = lhs))
    }
    
    # -------- First-stage instruments --------
    instr_names <- character(0)
    
    # build RHS once per endogenous loop: partial + included + excluded
    rhs1_vars_base <- unique(c(par_now, inc_now, exc_now))
    
    if (length(exc_now) && length(end_now) && length(rhs1_vars_base)) {
      for (e in end_now) {
        # e ~ 1 + partial + included + excluded
        f1 <- make_formula(e, rhs1_vars_base)
        fit1 <- stats::lm(f1, data = data)
        
        nm_instr <- paste0("instr_", e)
        data <- add_col(data, nm_instr, stats::predict(fit1))
        instr_names <- c(instr_names, nm_instr)
        
        # If partial present, subtract fitted component of instr_e on partial
        if (length(par_now)) {
          f2 <- make_formula(nm_instr, par_now)
          fit2 <- stats::lm(f2, data = data)
          data[[nm_instr]] <- data[[nm_instr]] - stats::predict(fit2)
        }
      }
    }
    
    # -------- Residualize to build tilda_* --------
    targets <- present(unique(c(outcome, inc_now, end_now)))
    
    if (length(par_now)) {
      for (v in targets) {
        f <- make_formula(v, par_now)
        m <- stats::lm(f, data = data)
        data[[paste0("tilda_", v)]] <- stats::residuals(m)
      }
    } else {
      # no partial: copy originals
      for (v in targets) {
        data[[paste0("tilda_", v)]] <- data[[v]]
      }
    }
    
    tilda_included   <- intersect(paste0("tilda_", included %||% character()),   names(data))
    tilda_endogenous <- intersect(paste0("tilda_", endogenous %||% character()), names(data))
    # return only the instrument columns we actually created
    tilda_excluded   <- if (length(instr_names)) instr_names else NULL
    
    list(
      data             = data,
      tilda_included   = tilda_included,
      tilda_endogenous = tilda_endogenous,
      tilda_excluded   = tilda_excluded
    )
  })
}


#' Build systemfit formulas and estimate the system
#'
#' @description
#' Constructs the list of structural equations (\code{g}) and instrument sets (\code{h}),
#' then runs \code{systemfit()} using OLS (when no excluded instruments) or 3SLS-GMM
#' (when excluded instruments are present).
#'
#' @param data Estimation \code{data.frame}/\code{data.table} containing the \code{tilda_*}
#'   and \code{instr_*} variables referenced by the formulas.
#' @param fields Named list with at least \code{outcome}, \code{included},
#'   \code{endogenous}, and optionally \code{excluded}.
#' @param tilda_included Character vector of residualized included regressor names
#'   (e.g., \code{"tilda_x1"}).
#' @param tilda_endogenous Character vector of residualized endogenous regressor names
#'   (e.g., \code{"tilda_z1"}).
#' @param tilda_excluded Character vector of instrument names (e.g., \code{"instr_z1"}),
#'   or \code{NULL} when no excluded instruments are used.
#'
#' @return A list with elements:
#'   \item{fit}{Fitted \code{systemfit} object.}
#'   \item{g}{List of structural formulas.}
#'   \item{h}{List of instrument formulas.}
#'
#' @importFrom systemfit systemfit
#' @importFrom stats as.formula
#' @export
fcip_demand_sys_fit <- function(data, fields, tilda_included, tilda_endogenous, tilda_excluded) {
  with(fields, {
    g <- h <- list()
    for (i in outcome) {
      g[[length(g) + 1]] <- stats::as.formula(
        paste0("tilda_", i, " ~ 1 + ", paste0(c(tilda_endogenous, tilda_included), collapse = "+"))
      )
      if (!is.null(excluded) && length(excluded)) {
        h[[length(h) + 1]] <- stats::as.formula(
          paste0("~ 1 + ", paste0(c(tilda_excluded, tilda_included), collapse = "+"))
        )
      } else {
        h[[length(h) + 1]] <- stats::as.formula(
          paste0("~ 1 + ", paste0(c(tilda_endogenous, tilda_included), collapse = "+"))
        )
      }
    }
    names(g) <- outcome  # preserve equation labels
    
    fit <- systemfit::systemfit(
      formula = g, inst = h, data = data,
      method  = if (is.null(excluded) || !length(excluded)) "OLS" else "3SLS",
      control = list(
        maxiter = 1, tol = 1e-3, methodResidCov = "geomean",
        centerResiduals = FALSE, residCovRestricted = TRUE, residCovWeighted = FALSE,
        method3sls = "GMM", singleEqSigma = NULL, useMatrix = TRUE,
        solvetol = .Machine$double.eps, model = TRUE, x = FALSE, y = FALSE, z = FALSE
      )
    )
    list(fit = fit, g = g, h = h)
  })
}


#' Two-way cluster-robust covariance for FCIP demand models
#'
#' @description
#' Computes a Cameron-Gelbach-Miller two-way cluster-robust covariance matrix
#' using inclusion-exclusion: \eqn{V = V_{pool} + V_{year} - V_{pool\_year}}.
#' Works for both \code{systemfit} (stacked system) and \code{lm} (first-stage).
#'
#' @param object Fitted model: either a \code{systemfit} or \code{lm}.
#' @param data   Estimation data containing pool and year identifiers.
#' @param kind   One of \code{c("systemfit","lm")}. If omitted, auto-detected.
#' @param pool_col Name of the pool/cluster id column in \code{data} (default \code{"pool"}).
#' @param year_col Name of the year/time id column in \code{data} (default \code{"crop_yr"}).
#' @param NFE Integer; number of absorbed fixed effects (for df rescaling).
#' @param n_partial Integer; count of variables partialed out per equation.
#' @param n_eq Integer; number of equations (\code{length(object$eq)} for
#'   \code{systemfit}, \code{1} for \code{lm}). You can override if needed.
#'
#' @details
#' **Rescaling.** Let \eqn{n} be the number of observations (stacked across
#' equations for \code{systemfit}). With \eqn{k_old} the number of coefficients
#' and \eqn{k_new = k_old + NFE + n\_partial * n\_eq}, the returned matrix is
#' scaled by \eqn{(n - k_old - 1) / (n - k_new - 1)}.
#'
#' **Row alignment (lm).** Rows used by \code{lm} are inferred from
#' \code{rownames(model.matrix(object))}. If they cannot be mapped back to
#' \code{data}, the first \code{nobs(object)} rows are used.
#'
#' @return Covariance matrix aligned with \code{coef(object)}.
#' @importFrom sandwich vcovCL
#' @importFrom stats residuals coef nobs model.matrix
#' @export
fcip_demand_sys_vcov <- function(object, data,
                                 kind = c("systemfit","lm"),
                                 pool_col = "pool", year_col = "commodity_year",
                                 NFE = 0L, n_partial = 0L, n_eq = NULL) {
  
  if (missing(kind) || length(kind) == 0L) {
    kind <- if(inherits(object, "systemfit")) "systemfit" else "lm"
  }
  
  if (kind == "systemfit") {
    n_eq <- if (is.null(n_eq)) length(object$eq) else n_eq
    pool <- rep(data[[pool_col]], n_eq)
    year <- rep(data[[year_col]], n_eq)
  } else {
    X   <- stats::model.matrix(object)
    rn  <- suppressWarnings(as.integer(rownames(X)))
    idx <- if (all(is.finite(rn))) rn else seq_len(nrow(X))
    pool <- data[[pool_col]][idx]
    year <- data[[year_col]][idx]
    n_eq <- if (is.null(n_eq)) 1L else n_eq
  }
  both <- interaction(pool, year, drop = TRUE)
  
  Vi <- sandwich::vcovCL(object, cluster = pool, type = "HC1", method = "arellano", sandwich = TRUE)
  Vt <- sandwich::vcovCL(object, cluster = year, type = "HC1", method = "arellano", sandwich = TRUE)
  Vw <- sandwich::vcovCL(object, cluster = both, type = "HC1", method = "arellano", sandwich = TRUE)
  V  <- Vi + Vt - Vw
  
  if (kind == "systemfit") {
    r <- stats::residuals(object)
    n_obs <- if (is.list(r)) sum(vapply(r, length, 1L)) else NROW(r)
  } else {
    n_obs <- stats::nobs(object)
  }
  k_old <- length(stats::coef(object))
  k_new <- k_old + NFE + n_partial * n_eq
  scale <- (n_obs - k_old - 1) / (n_obs - k_new - 1)
  V * scale
}


#' Tidy coefficient table with cluster-robust SEs (from supplied VCOV)
#'
#' @description
#' Builds a clean coefficient table for a `systemfit` model using a **user-supplied
#' covariance matrix** (e.g., two-way clustered from `fcip_demand_sys_vcov()`).
#' Estimates come from `coef(fit)`, standard errors from `diag(vcMat)`, then
#' Z-scores and two-sided normal p-values are computed. The `demand` column is
#' inferred from the equation prefix in the coefficient names:
#'   - `"gamma_*"` to `"gamma"`
#'   - `"theta_*"` to `"theta"`
#' Otherwise the prefix itself is used.
#'
#' @param fit   A fitted `systemfit` object.
#' @param vcMat A covariance matrix conformable with `coef(fit)`. Row/column
#'   names are used to align; if missing, positional alignment is assumed.
#' @param p_digits Integer; number of digits to keep for p-values (default `5`).
#'
#' @return A `data.frame` with columns:
#'   `demand`, `coef`, `Estimate`, `StdError`, `Zvalue`, `Pvalue`.
#'
#' @importFrom stats coef pnorm
#' @export
fcip_demand_sys_coeff_table <- function(fit, vcMat, p_digits = 5) {
  b  <- stats::coef(fit)
  cn <- names(b)
  
  # Align VCOV to coef order
  if (!is.null(dimnames(vcMat)) && all(!is.na(match(cn, rownames(vcMat))))) {
    idx <- match(cn, rownames(vcMat))
    V   <- vcMat[idx, idx, drop = FALSE]
  } else {
    V <- vcMat
    if (!all(dim(V) == c(length(b), length(b)))) {
      stop("vcMat is not conformable with coef(fit).")
    }
  }
  
  var_diag <- diag(V)
  var_diag[var_diag < 0 & var_diag > -1e-12] <- 0
  if (any(var_diag < 0, na.rm = TRUE)) {
    warning("Negative variances detected in vcMat diagonal; producing NA StdError for those.")
  }
  se <- sqrt(pmax(var_diag, 0))
  
  z  <- b / se
  p  <- round(2 * stats::pnorm(abs(z), lower.tail = FALSE), p_digits)
  
  prefix <- sub("_.*$", "", cn)
  demand <- tolower(prefix) #ifelse(prefix == "Gamma", "Gamma", ifelse(grepl("^Theta\\d*$", prefix), "Theta", prefix))
  coef_plain <- sub("^[^_]*_", "", cn)
  
  data.frame(
    demand   = demand,
    coef     = coef_plain,
    Estimate = as.numeric(b),
    StdError = as.numeric(se),
    Zvalue   = as.numeric(z),
    Pvalue   = as.numeric(p),
    row.names = NULL,
    check.names = FALSE
  )
}


#' Delta-method "total protection response"
#'
#' @description
#' Combines equation-specific effects into a single "total" effect for each
#' regressor in \code{c(fields$endogenous, fields$included)} using
#' \code{car::deltaMethod} and a supplied covariance matrix.
#'
#' @param fit   A fitted \code{systemfit} object (the structural system).
#' @param vcMat Covariance matrix conformable with \code{coef(fit)} (e.g., from
#'   \code{fcip_demand_sys_vcov()}).
#' @param fields Named list carrying model fields; must include
#'   \code{outcome}, \code{endogenous}, and \code{included}.
#' @param data  Estimation data used to check variable availability and build
#'   delta-method expressions.
#'
#' @return A \code{data.frame} with rows \code{demand="total"} and columns:
#'   \code{demand}, \code{coef}, \code{Estimate}, \code{StdError}, \code{Zvalue}, \code{Pvalue}.
#'
#' @importFrom car deltaMethod
#' @importFrom plyr ldply
#' @importFrom stats pnorm
#' @export
fcip_demand_sys_effect <- function(fit, vcMat, fields, data) {
  with(fields, {
    efflist <- c(endogenous, included)
    efflist <- efflist[efflist %in% names(data)]
    if (!length(efflist)) return(data.frame())
    
    coefs <- coef(fit)
    forms <- paste0(outcome[1], "_tilda_", efflist, " + ",
                    outcome[2], "_tilda_", efflist, " + ",
                    outcome[1], "_tilda_", efflist, "*",
                    outcome[2], "_tilda_", efflist)
    
    lis <- lapply(forms, function(g) car::deltaMethod(object = coefs, g = g, vcov. = vcMat, level = 0.95))
    val <- plyr::ldply(lis)
    k   <- ncol(val)
    ret <- val[, c(-(k-1), -k)][c("Estimate","SE")]
    names(ret) <- c("Estimate","StdError")
    ret$demand <- "total"
    ret$coef   <- paste0("tilda_", efflist)
    ret$Zvalue <- ret$Estimate / ret$StdError
    ret$Pvalue <- round(2 * stats::pnorm(q = abs(ret$Zvalue), lower.tail = FALSE), 5)
    ret[, c("demand","coef","Estimate","StdError","Zvalue","Pvalue")]
  })
}


#' System diagnostics: two-way robust first-stage F (+ optional approx. J)
#'
#' @description
#' Produces diagnostics **without** re-running GMM:
#' - **FTest**: joint relevance of excluded instruments in each first stage,
#'   using the same two-way (pool by crop year) cluster-robust covariance via
#'   `fcip_demand_sys_vcov()` with \code{kind = "lm"}. Reports the **minimum** F
#'   across endogenous regressors.
#' - **JTest** (optional): an *approximate* over-identification test computed as
#'   the sum of per-equation Sargan statistics
#'   \eqn{J_k \approx n_k R_k^2} from regressions of equation residuals on that
#'   equation's instrument set. This is a quick check (not the system Hansen J).
#'
#' @param g List of system equations (the same formulas passed to `systemfit`).
#' @param h List of instrument formulas (the same formulas passed to `systemfit`).
#' @param data Estimation `data.frame`/`data.table` containing all variables in `g`/`h`
#'   plus clustering columns `pool` and `crop_yr`.
#' @param fit A fitted `systemfit` object (used for `N` and `residCov_*` extraction).
#' @param NFE Integer: number of absorbed fixed effects (for reporting only).
#' @param approx_j Logical, compute the approximate (non-robust) Sargan J as described above.
#'   Default `FALSE` (returns `NA` for `JTest`).
#'
#' @return A \code{data.frame} with rows:
#' \code{N}, \code{NFE}, \code{residCov_11}, \code{residCov_22}, \code{residCov_12},
#' \code{JTest}, \code{FTest}.
#'
#' @importFrom stats residuals coef nobs model.matrix terms pf pchisq lm
#' @export
fcip_demand_sys_tests <- function(g, h, data, fit, NFE, approx_j = FALSE){
  
  # Helpers
  term_labels <- function(fr) {
    tl <- attr(stats::terms(fr), "term.labels")
    if (is.null(tl)) character(0) else tl
  }
  # Wald F for H0: R beta = 0 with robust VCOV V
  wald_F <- function(mod, R_names, V, n, k) {
    b <- stats::coef(mod)
    cn <- names(b)
    keep <- intersect(R_names, cn)
    if (!length(keep)) return(c(F = NA_real_, p = NA_real_))
    R <- matrix(0, nrow = length(keep), ncol = length(cn),
                dimnames = list(NULL, cn))
    for (i in seq_along(keep)) R[i, keep[i]] <- 1
    rb  <- as.numeric(R %*% b)
    RV  <- R %*% V %*% t(R)
    RV_inv <- tryCatch(solve(RV), error = function(e) {
      tryCatch(qr.solve(RV, diag(nrow(RV))), error = function(e) NULL)
    })
    if (is.null(RV_inv)) return(c(F = NA_real_, p = NA_real_))
    W  <- as.numeric(t(rb) %*% RV_inv %*% rb)
    q  <- nrow(R)
    Fv <- W / q
    Pv <- stats::pf(Fv, q, n - k, lower.tail = FALSE)
    c(F = Fv, p = Pv)
  }
  
  rows <- list(
    data.frame(demand="", coef="N",           Estimate = nrow(stats::residuals(fit)), StdError=NA, Zvalue=NA, Pvalue=NA),
    data.frame(demand="", coef="NFE",         Estimate = NFE,               StdError=NA, Zvalue=NA, Pvalue=NA),
    data.frame(demand="", coef="residCov_11", Estimate = fit$residCov[1,1], StdError=NA, Zvalue=NA, Pvalue=NA),
    data.frame(demand="", coef="residCov_22", Estimate = fit$residCov[2,2], StdError=NA, Zvalue=NA, Pvalue=NA),
    data.frame(demand="", coef="residCov_12", Estimate = fit$residCov[1,2], StdError=NA, Zvalue=NA, Pvalue=NA)
  )
  
  all_terms   <- unique(unlist(lapply(h, term_labels)))
  instr_terms <- grep("^instr_", all_terms, value = TRUE)
  inc_terms   <- grep("^tilda_", all_terms, value = TRUE)
  if (!length(instr_terms)) {
    rows <- c(rows, list(
      data.frame(demand="", coef="JTest", Estimate = NA_real_, StdError=NA, Zvalue=NA, Pvalue=NA),
      data.frame(demand="", coef="FTest", Estimate = NA_real_, StdError=NA, Zvalue=NA, Pvalue=NA)
    ))
    return(do.call(rbind, rows))
  }
  
  # robust first-stage F across endogenous regressors
  endogs <- unique(sub("^instr_", "", instr_terms))
  F_vals <- c(); P_vals <- c()
  
  for (evar in endogs) {
    y <- paste0("tilda_", evar)
    if (!y %in% names(data)) next
    
    exc_rhs <- grep(paste0("^instr_", evar, "$"), instr_terms, value = TRUE)
    if (!length(exc_rhs)) next
    
    rhs_full <- c(inc_terms, exc_rhs)
    frm_full <- stats::as.formula(paste0(y, " ~ ", if (length(rhs_full)) paste("1 +", paste(rhs_full, collapse = "+")) else "1"))
    
    mod_full <- tryCatch(stats::lm(frm_full, data = data), error = function(e) NULL)
    if (is.null(mod_full)) next
    
    V <- tryCatch(
      fcip_demand_sys_vcov(object = mod_full, data = data,
                           kind = "lm", NFE = NFE, n_partial = 0L, n_eq = 1L),
      error = function(e) NULL
    )
    if (is.null(V)) next
    
    wf <- wald_F(mod_full, R_names = exc_rhs, V = V,
                 n = stats::nobs(mod_full), k = length(stats::coef(mod_full)))
    F_vals <- c(F_vals, wf["F"]); P_vals <- c(P_vals, wf["p"])
  }
  
  F_stat <- if (length(F_vals)) suppressWarnings(min(as.numeric(F_vals), na.rm = TRUE)) else NA_real_
  F_p    <- if (length(P_vals))  P_vals[which.min(replace(as.numeric(F_vals), !is.finite(as.numeric(F_vals)), Inf))] else NA_real_
  
  # optional approximate J (sum of per-eq Sargan nR^2)
  J_est <- NA_real_; J_p <- NA_real_
  if (isTRUE(approx_j)) {
    Rs <- stats::residuals(fit)  # list by equation
    Xs <- lapply(g, function(fr) stats::model.matrix(fr, data))
    Zs <- lapply(h, function(fr) stats::model.matrix(fr, data))
    Jk <- numeric(length(g)); dfk <- integer(length(g))
    for (k in seq_along(g)) {
      e  <- if (is.list(Rs)) Rs[[k]] else Rs
      Xk <- Xs[[k]]; Zk <- Zs[[k]]
      dfk[k] <- ncol(Zk) - ncol(Xk)
      if (!is.na(dfk[k]) && dfk[k] > 0) {
        fitZ <- tryCatch(stats::lm(e ~ Zk - 1), error = function(e) NULL)
        if (!is.null(fitZ)) {
          n  <- NROW(Zk)
          r2 <- summary(fitZ)$r.squared
          Jk[k] <- n * r2
        } else {
          Jk[k] <- NA_real_
        }
      } else {
        Jk[k] <- NA_real_
      }
    }
    J_est    <- sum(Jk[!is.na(Jk) & dfk > 0], na.rm = TRUE)
    df_total <- sum(dfk[!is.na(dfk) & dfk > 0], na.rm = TRUE)
    if (is.finite(J_est) && df_total > 0) J_p <- 1 - stats::pchisq(J_est, df_total)
  }
  
  rows <- c(rows, list(
    data.frame(demand="", coef="JTest", Estimate = J_est, StdError=NA, Zvalue=NA, Pvalue = J_p),
    data.frame(demand="", coef="FTest", Estimate = F_stat, StdError=NA, Zvalue=NA, Pvalue = F_p)
  ))
  do.call(rbind, rows)
}


#' Orchestrate analysis
#'
#' @description
#' Runs the full pipeline: prep -> partial/tilda creation ->
#' systemfit -> two-way clustered VCOV -> delta-method totals -> optional restricted
#' step -> diagnostics; then returns a tidy coefficient table with metadata.
#'
#' @param data Estimation dataset 
#' @param fields Named list carrying model fields (see \code{fcip_demand_estimation()}),
#'   including \code{disag}, \code{FE}, \code{outcome}, \code{endogenous}, \code{included},
#'   optional \code{excluded}, \code{partial}, \code{restrict}, and \code{name}.
#'
#' @return A \code{data.frame} with columns \code{demand}, \code{coef},
#'   \code{Estimate}, \code{StdError}, \code{Zvalue}, \code{Pvalue} and
#'   meta-columns \code{model}, \code{endogenous}, \code{FE}, \code{name},
#'   \code{disag}
#'
#' @export
fcip_demand_sys_run <- function(data, fields) {
  prep <- fcip_demand_sys_prep(data=data, fields=fields)
  dd   <- prep$data
  NFE  <- prep$NFE
  partial_now <- prep$partial
  
  pt <- fcip_demand_sys_partial(dd, fields, partial_override = partial_now)
  dd <- pt$data
  tI <- pt$tilda_included
  tE <- pt$tilda_endogenous
  tX <- pt$tilda_excluded
  
  fito <- fcip_demand_sys_fit(dd, fields, tilda_included = tI, tilda_endogenous = tE, tilda_excluded  = tX)
  fit  <- fito$fit; g <- fito$g; h <- fito$h

  n_eq      <- length(fit$eq)
  n_partial <- if(is.null(partial_now)) 0L else length(partial_now)
  
  vc <- fcip_demand_sys_vcov(object = fit, data = dd,kind = "systemfit", NFE = NFE, n_partial = n_partial, n_eq = n_eq)
  
  tab <- fcip_demand_sys_coeff_table(fit, vc)
  tot <- fcip_demand_sys_effect(fit, vc, fields, dd)
  tst <- fcip_demand_sys_tests(g = g, h = h, data = dd, fit = fit, NFE = NFE, approx_j = TRUE)
  
  res <- rbind(tab, tot, tst)
  # res <- rbind(res, fcip_demand_sys_restricted(fields$restrict, tab, fit, dd, fields$outcome, tE, tX, tI))
  rownames(res) <- NULL
  res$model      <- fields$name
  res$endogenous <- paste(fields$endogenous, collapse = ",")
  res$FE         <- fields$FE
  res$name       <- fields$name
  res
}

#' Calibrate FCIP demand elasticities via IV-SEM (lavaan)
#'
#' Fits a just-identified IV-style SEM where `instr_rate` instruments `tilda_rate`,
#' and `tilda_rate` enters two outcome equations (`tilda_ghamma`, `tilda_theta`).
#' Endogeneity is encoded via residual correlations between `tilda_rate` and each outcome.
#' The instrument is excluded from the outcome disturbances.
#'
#' Constraints imposed:
#'   1) b1 < 0,  2) b2 < 0,  3) b1 + b2 + b1*b2 < 0
#'
#' @param data data.frame with cols: instr_rate, tilda_rate, tilda_gamma, tilda_theta
#' @param estimator "ML" or "MLR" (default "MLR" = robust SE/tests)
#' @param missing   "fiml" or "listwise" (default "fiml")
#' @param control a list of control parameters. see \code{\link{rfcipDemand_controls}}.
#' @return A data.table of parameter estimates and logical convergence flag
#' @import data.table
#' @export
fcip_demand_elasticities_lavaan <- function(
    data,
    estimator = c("ML", "MLR"),
    missing   = c("fiml", "listwise"),
    control = rfcipDemand_controls()){
  # required columns
  need <- c("instr_rate", "tilda_rate", "tilda_gamma", "tilda_theta")
  miss <- setdiff(need, names(data))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse = ", "))
  
  estimator <- match.arg(estimator)
  missing   <- match.arg(missing)
  
  model <- '
    # first stage (instrument relevance)
    tilda_rate ~ a*instr_rate

    # outcome equations (elasticities of interest)
    tilda_gamma  ~ b1*tilda_rate
    tilda_theta ~ b2*tilda_rate

    # encode endogeneity (residual correlations)
    tilda_rate ~~ r1*tilda_gamma
    tilda_rate ~~ r2*tilda_theta

    # exclusion: instrument uncorrelated with outcome disturbances
    instr_rate ~~ 0*tilda_gamma
    instr_rate ~~ 0*tilda_theta

    # allow correlation between outcome disturbances
    tilda_gamma ~~ tilda_theta

    # means (optional)
    tilda_gamma ~ 1
    tilda_theta ~ 1
  '
  ineq <- paste0("b1 < ",control$elasticity_limits$gamma[1],
                 "\nb2 < ",control$elasticity_limits$theta[2],
                 "\nb1 + b2 + b1*b2 < ",control$elasticity_limits$total[3])
  
  fit <- lavaan::sem(
    model         = model,
    data          = data,
    estimator     = estimator,   # "ML" or "MLR"
    missing       = missing,     # "fiml" or "listwise"
    fixed.x       = FALSE,
    meanstructure = TRUE,
    constraints   = ineq
  )
  
  pe  <- as.data.table(lavaan::parameterEstimates(fit, standardized = TRUE))
  # rsq <- tryCatch(lavaan::inspect(fit, "rsquare"), error = function(e) NULL)
  
  pe <- pe[grepl("tilda_rate",rhs)]
  pe <- pe[op %in% "~"]
  pe <- pe[grepl("gamma|theta",lhs)]
  
  pe[,lhs := gsub("tilda_","",lhs)]
  
  pe <- data.table::rbindlist(
    list(pe, data.table(est=pe[grepl("gamma",lhs)][["est"]] + pe[grepl("theta",lhs)][["est"]] + 
                          pe[grepl("gamma",lhs)][["est"]]*pe[grepl("theta",lhs)][["est"]],
                        lhs="total",
                        coef="tilda_rate")),fill = TRUE)
  
  setnames(pe,
           old = c("lhs","rhs","est","se","z","pvalue"),
           new = c("demand","coef","Estimate_lavaan","StdError","Zvalue","Pvalue"))
  
  # list(
  #   fit       = fit,
  #   pe        = pe,
  #   rsq       = rsq,
  #   converged = isTRUE(lavaan::inspect(fit, "converged"))
  # )
  
  pe
}


#' System estimator (modular wrapper; preserves original outputs)
#'
#' @description
#' Estimates a two-equation system with an endogenous regressor across
#' disaggregation levels. The pipeline is:
#'  (1) per-level sample selection (min n per `commodity_year`),
#'  (2) partialling-out / tilda transforms,
#'  (3) system estimation (e.g., IV/3SLS via internal helpers),
#'  (4) clustered variance estimation,
#'  (5) delta-method totals,
#'  (6) optional constrained re-estimation of elasticities (lavaan),
#'  (7) diagnostics, and
#'  (8) row-binding results across levels.
#'
#' @param model List specifying the system; required elements are:
#'   - `outcome` (character(2)): names of the two outcomes in `data`.
#'   - `endogenous` (character(1)): endogenous regressor name.
#'   - `included` (character): included (exogenous) regressors.
#'   - `disag` (character(1) or `NULL`): disaggregation key column in `data`.
#'   - `FE` (logical): include fixed effects in the internal run (handled by helpers).
#'   Optional elements:
#'   - `excluded` (character or `NULL`): excluded instruments.
#'   - `partial`  (character or `NULL`): variables to partial out (tilda).
#'   - `restrict` (logical): pass-through to internal restricted estimation.
#'   - `name`     (character(1)): label carried to the output.
#' @param data `data.frame`/`data.table` containing all referenced columns in
#'   `model` plus `pool` and `commodity_year`. Columns in `model$outcome`,
#'   `model$endogenous`, `model$included`, and (if used) `model$excluded` and
#'   `model$partial` must exist in `data`.
#' @param constrained_elasticities Logical (default `FALSE`). If `TRUE`, re-estimate the
#'   elasticities via a constrained SEM (lavaan) and, where applicable, replace
#'   positive elasticity estimates with constrained ones. See **Constrained
#'   elasticities (optional)**.
#' @param control a list of control parameters. see \code{\link{rfcipDemand_controls}}.
#' @details
#' **Inputs and preprocessing**
#' - `model$outcome` must be a character vector of length 2 giving the two
#'   outcome column names in `data`. Internally these are mapped to
#'   `gamma` and `theta` for estimation convenience.
#' - `model$disag` is the name of the disaggregation key. If `NULL`, a dummy
#'   `"full_sample"` key is created.
#' - The disaggregation key is coerced to `character`.
#' - For each level of `model$disag`, the function keeps only levels that have
#'   **at least 30 observations per `commodity_year`** (computed via
#'   `doBy::summaryBy`). Levels failing this threshold are dropped.
#'
#' **Per-level estimation**
#' For each retained level, the function calls internal helpers
#' (`fcip_demand_sys_run`, etc.) to (i) partially out controls if requested,
#' and (ii) estimate the system with clustered VCOV and delta-method totals.
#' Errors at the level are caught and the level is skipped (no hard stop).
#'
#' **Constrained elasticities (optional)**
#' When `constrained = TRUE`, elasticities on the endogenous regressor are
#' re-estimated per level via a lavaan SEM with sign restrictions (internal
#' helper `fcip_demand_elasticities_lavaan`, using `estimator = "MLR"`,
#' `missing = "listwise"`). The constrained estimates replace any positive
#' system estimates for the elasticities; a logical flag `constrained` marks
#' levels where a replacement occurred. The returned columns are reduced to
#' `disag`, `level`, `demand`, `constrained`, and `Estimate`.
#'
#' **Returned shape**
#' - If `constrained_elasticities = FALSE` (default), returns the full per-level system
#'   output from `fcip_demand_sys_run` with additional columns:
#'   `disag`, `level`, and rounded `Zvalue`, `Pvalue`.
#' - If `constrained = TRUE`, returns a compact table with
#'   `disag`, `level`, `demand`, `constrained`, `Estimate` after merging the
#'   constrained elasticities.
#'
#' @return A `data.table` aggregating results across all disaggregation levels.
#'   The column set depends on `constrained` (see **Returned shape**).
#' @import data.table
#' @importFrom doBy summaryBy
#' @export
fcip_demand_sys_estimate <- function(
    model, 
    data, 
    constrained_elasticities = FALSE,
    control = rfcipDemand_controls()){
  
  stopifnot(all(c("outcome","endogenous","included","disag","FE") %in% names(model)))
  
  data <- as.data.table(data)
  
  # Residual helper: NULL-coalescing
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  outcome    <- model$outcome
  endogenous <- model$endogenous
  included   <- model$included
  excluded   <- model$excluded %||% NULL
  partial    <- model$partial  %||% NULL
  restrict   <- isTRUE(model$restrict)
  if(is.null(model$disag)){
    data$full_sample <- 1
    model$disag      <- "full_sample"
  }
  disag   <- model$disag
  
  # Ensure outcome names
  data[["gamma"]] <- data[[model$outcome[1]]]
  data[["theta"]] <- data[[model$outcome[2]]]
  outcome <- c("gamma","theta")

  # Ensure disaggregation key is character
  data[[disag]] <- as.character(data[[disag]])
  
  # Build levels with >= 30 obs per commodity_year
  disagMap <- doBy::summaryBy(list("commodity_year", disag), data = data, FUN = length)
  stopifnot("commodity_year.length" %in% names(disagMap))
  disagMap <- disagMap[disagMap[["commodity_year.length"]] >= 30, , drop = FALSE]
  
  data   <- data[data[[disag]] %in% disagMap[[disag]], , drop = FALSE]

  fields <- list(outcome=outcome, endogenous=endogenous, included=included,
                 excluded=excluded, partial=partial, FE=model$FE,
                 restrict=restrict, disag=disag, name=model$name %||% NA_character_)
  
  res <- data.table::rbindlist(
    lapply(unique(data[[disag]]), function(i) {
      tryCatch({
        res <- as.data.frame( fcip_demand_sys_run(data = data[data[[disag]] %in% i, , drop = FALSE], fields = fields))
        res$disag <- fields$disag
        res$level <- i
        res$Zvalue <- round(res$Zvalue,8)
        res$Pvalue <- round(res$Pvalue,8)
        res
      }, error = function(e) { NULL })}),fill = TRUE)
  
  if(isTRUE(constrained_elasticities)) {
    
    res_lavaan <- data.table::rbindlist(
      lapply(unique(data[[disag]]), function(i) {
        tryCatch({
          # i <- "1"
          pd <- fcip_demand_sys_prep(data = data[data[[disag]] %in% i, , drop = FALSE], fields = fields)
          pt <- fcip_demand_sys_partial(data = pd$data, fields = fields, partial_override = pd$partial)$data
          pe <- fcip_demand_elasticities_lavaan(data = pt,estimator = "MLR", missing   = "listwise", control = control)
          pe[,endogenous := fields$endogenous]
          pe[,FE := fields$FE]
          pe[,name := fields$name]
          pe[,disag := fields$disag]
          pe[,level := i]
          pe <- pe[, c("demand","coef","Estimate_lavaan","StdError","Zvalue","Pvalue","endogenous","FE","name","disag","level"), with = FALSE]
          pe
        }, error = function(e) { NULL })}),fill = TRUE)

    res <- res[grepl("tilda_rate",coef), c("disag","level","demand","Estimate"), with = FALSE][
      unique(res_lavaan[, c("disag","level","demand","Estimate_lavaan"), with = FALSE]),
      on = c("disag","level","demand"),nomatch = 0]

    res[, constrained := max(Estimate > 0), by = c("disag","level")]
    
    res[constrained %in% TRUE, Estimate := Estimate_lavaan]
    
    res <- res[, c("disag","level","demand","constrained","Estimate"), with = FALSE]
  }

  return(as.data.table(res))
}
