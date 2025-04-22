#' almond_profit
#'
#' Calculate total almond profit based on yield anomaly and water costs
#'
#' @param yield_anomaly the yield anomaly (ton/acre) from the almondyield() function
#' @param acres Number of acres being farmed (default = 500)
#' @param baseline_profit Baseline profit per acre in a normal year (default = 4000)
#' @param price_per_ton Price of almonds per ton (default = 6000)
#'
#' @return Total profit for given inputs
#' @export
#'
#' @authors Taylor Cook and Kelsey Warren


almond_profit <- function(yield_anomaly, acres = 500, baseline_profit = 4000, 
                          price_per_ton = 6000) {
  
  # Calculate profit per acre
  total_profit <- baseline_profit + (yield_anomaly * price_per_ton * acres)
  
  return(total_profit)
}
