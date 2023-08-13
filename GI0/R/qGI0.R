#' Quantile function
#'
#' @param p Probability values for which to calculate the quantiles
#' @param p_alpha Negative value that controls the roughness
#' @param p_gamma Positive value that controls the scale
#' @param p_Looks Positive value that controls the number of looks
#' @param lower.tail Logical; if TRUE, computes lower tail quantiles, else upper tail quantiles
#' @param log.p Logical; if TRUE, input probabilities are given as log probabilities
#' @return The quantile values for the rGI0 distribution
qGI0 <- function(p, p_alpha, p_gamma, p_Looks, lower.tail = TRUE, log.p = FALSE) {
  if (p_alpha < 0 & p_gamma > 0 & p_Looks >= 1) {
      return(qf(p, df1 = 2 * p_Looks, df2 = -2 * p_alpha, lower.tail, log.p))
  } else {
    stop("Invalid parameters. The conditions are not met.")
  }
}
