#' Demand Model Control Parameters
#'
#' @description
#' Constructs a control list for demand-model routines in \pkg{rfcipDemand},
#' primarily governing admissible ranges for elasticity parameters used in
#' demand adjustments and simulations.
#'
#' If \code{elasticity_limits} is not supplied, the function defaults to
#' \code{fcip_elasticity_limits}, a package-level object defining
#' program-consistent elasticity bounds.
#'
#' @param elasticity_limits
#' Optional named list specifying lower and upper bounds for elasticity
#' parameters. If \code{NULL}, defaults to \code{fcip_elasticity_limits}.
#' 
#' @return A named list of control parameters, ready to be passed to other simulation functions.
#' @family helpers
#' @export
rfcipDemand_controls <- function(
    elasticity_limits = NULL
) {
  
  if (is.null(elasticity_limits)) {
    elasticity_limits <- fcip_elasticity_limits
  }
  
  list(
    elasticity_limits = elasticity_limits
  )
}

