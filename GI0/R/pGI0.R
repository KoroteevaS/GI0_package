#' Cumulative distribution function
#'
#' This function calculates the CDF from Fisher-Snedekor distribution
#'
#' @param x Numeric vector of input values at which to calculate the CDF.
#' @param p_alpha Numeric, negative value controlling the roughness.
#' @param p_gamma Numeric, positive value controlling the scale.
#' @param p_Looks Numeric, positive value controlling the number of looks.
#' @param log.p Logical; if TRUE, returns the log probability.
#' @return Numeric vector of cumulative distribution values for the rGI0 distribution.
#' @examples

pGI0 <- function(x, p_alpha, p_gamma, p_Looks, log.p = FALSE) {
  if (p_alpha < 0 & p_gamma > 0 & p_Looks >= 1) {
    return(pf(-p_alpha * x / p_gamma, df1 = 2 * p_Looks, df2 = -2 * p_alpha, log.p))
  } else {
    stop("Invalid parameters. The conditions are not met.")
  }
}
