#' Random number generator
#'
#' @param p_alpha Negative value that controls the roughness
#' @param p_gamma Positive value that controls the scale
#' @param p_Looks Positive value that controls the number of looks
#'
rGI0 <- function(n, p_alpha, p_gamma, p_Looks, from.F=FALSE){
  ifelse(from.F==TRUE,
         return(rf(n, df1=2*p_Looks, df2=-2*p_alpha)),
         return(
           rgamma(n, shape=p_Looks, rate=p_Looks) /
             rgamma(n, shape=-p_alpha, rate=p_gamma)
         )
  )
}
