#' CDF
#'

#' @param p_alpha Negative value that controls the roughness
#' @param p_gamma Positive value that controls the scale
#' @param p_Looks Positive value that controls the number of looks

# cumulative distribution function
pGI0 <- function(x, p_alpha, p_gamma, p_Looks, log.p=FALSE) {
  return(pf(-p_alpha * x / p_gamma, df1 = 2*p_Looks, df2=-2*p_alpha, log.p))
}
