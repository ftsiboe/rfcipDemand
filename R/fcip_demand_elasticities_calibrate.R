#' Calibrate FCIP demand elasticities via IV-SEM (lavaan)
#'
#' Fits a just-identified IV-style SEM where `instr_rate` instruments `tilda_rate`,
#' and `tilda_rate` enters two outcome equations (`tilda_Gamma`, `tilda_Theta1`).
#' Endogeneity is encoded via residual correlations between `tilda_rate` and each outcome.
#' The instrument is excluded from the outcome disturbances.
#'
#' Constraints imposed:
#'   1) b1 < 0,  2) b2 < 0,  3) b1 + b2 + b1*b2 < 0
#'
#' @param data data.frame with cols: instr_rate, tilda_rate, tilda_Gamma, tilda_Theta1
#' @param estimator "ML" or "MLR" (default "MLR" = robust SE/tests)
#' @param missing   "fiml" or "listwise" (default "fiml")
#' @return A data.table of parameter estimates and logical convergence flag
#' @import data.table
#' @export
fcip_demand_elasticities_lavaan <- function(
    data,
    estimator = c("ML", "MLR"),
    missing   = c("fiml", "listwise")
){
  # required columns
  need <- c("instr_rate", "tilda_rate", "tilda_Gamma", "tilda_Theta1")
  miss <- setdiff(need, names(data))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse = ", "))
  
  estimator <- match.arg(estimator)
  missing   <- match.arg(missing)
  
  model <- '
    # first stage (instrument relevance)
    tilda_rate ~ a*instr_rate

    # outcome equations (elasticities of interest)
    tilda_Gamma  ~ b1*tilda_rate
    tilda_Theta1 ~ b2*tilda_rate

    # encode endogeneity (residual correlations)
    tilda_rate ~~ r1*tilda_Gamma
    tilda_rate ~~ r2*tilda_Theta1

    # exclusion: instrument uncorrelated with outcome disturbances
    instr_rate ~~ 0*tilda_Gamma
    instr_rate ~~ 0*tilda_Theta1

    # allow correlation between outcome disturbances
    tilda_Gamma ~~ tilda_Theta1

    # means (optional)
    tilda_Gamma  ~ 1
    tilda_Theta1 ~ 1
  '
  
  ineq <- "b1 < 0\nb2 < 0\nb1 + b2 + b1*b2 < 0"
  
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
  pe <- pe[grepl("Gamma|Theta",lhs)]
  
  pe[,lhs := gsub("tilda_","",lhs)]
  
  pe <- data.table::rbindlist(
    list(pe, data.table(est=pe[grepl("Gamma",lhs)][["est"]] + pe[grepl("Theta",lhs)][["est"]] + pe[grepl("Gamma",lhs)][["est"]]*pe[grepl("Theta",lhs)][["est"]],
                        lhs="Total")),fill = TRUE)
  
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

