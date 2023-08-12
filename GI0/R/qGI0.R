#' Quantile function
#'
#' @param p_alpha Negative value that controls the roughness
#' @param p_gamma Positive value that controls the scale
#' @param p_Looks Positive value that controls the number of looks
#' @param lower.tatil Optional parameter that computes only the lower tail
#' @param log optional parameter that switches logarithm
# quantile function
qGI0 <- function(p, p_alpha, p_gamma, p_Looks, lower.tail = TRUE, log.p = FALSE) {
  return(qf(p, df1=2*p_Looks, df2=-2*p_alpha, lower.tail, log.p))
}
