#'  Logistic forest growth derivative
#'  Calculates the derivative of forest growth with respect to time
#' @param time time since start
#' @param params - as list with values, r, K, g, Cthresh
#' @param r exponential growth rate before canopy closure
#' @param K carrying capacity
#' @param g linear growth rate after canopy closure
#' 
#' @return list containing derivative of forest growth with time


dforestgrowth <- function(time, C, params) {
  with(as.list(c(C, params)), {
    if (C < Cthresh) {
      dCdt <- r * C
    } else {
      dCdt <- g * (1 - C / K)
    }
    return(list(dCdt))
  })
}


