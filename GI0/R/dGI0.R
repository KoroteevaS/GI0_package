#'"PDF"
#'

#' @param p_alpha Negative value that controls the roughness
#' @param p_gamma Positive value that controls the scale
#' @param p_Looks Positive value that controls the number of looks
#'
#' @examples
#' x<- seq(0,5,length.ou=500)
#' plot (x, dGI0(x, -2, 4, 5),type="l")
#'
dGI0 <- function(x, p_alpha, p_gamma, p_Looks) {
  if (p_alpha<0 & p_gamma>0 & p_Looks>=1){
    return(df(-p_alpha*x/p_gamma,df1=2*p_Looks, df2=-2*p_alpha))
  } else{
    cat("Wrong parameters")
  }

  return(df(-p_alpha*x/p_gamma, df1=2*p_Looks, df2=-2*p_alpha))
}
