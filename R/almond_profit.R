#' almond_profit
#'
#' Calculate total almond profit based on yield anomaly and water costs
#'
#' @param yield_anomaly the yield anomaly (ton/acre) from the almondyield() function
#' @param clim the temperature and precip data frame
#' @param month Numeric value for the target month (e.g. 2 for February)
#' @param acres Number of acres being farmed (default = 500)
#' @param baseline_profit Baseline profit per acre in a normal year (default = 4000)
#' @param price_per_ton Price of almonds per ton (default = 6000)
#' @param base_water_cost Base cost of water per acre (default = 200)
#'
#' @return Total profit for given inputs
#' @export
#'
#' @authors Taylor Cook and Kelsey Warren


almond_profit <- function(yield_anomaly, clim, acres = 500, baseline_profit = 4000, 
                          price_per_ton = 6000, base_water_cost = 200) {
  

  # Calculate profit per acre
  profit_per_acre <- baseline_profit + (yield_anomaly * price_per_ton * acres)
  
  
  # Adjust water cost if P1 is low (if P1 < 100 mm, water cost increases, otherwise, water cost = baseline)
  # first define P1
  jan_data <- clim[clim$month == 1, ]
  P1 <- sum(jan_data$precip, na.rm = TRUE)
  # then calculate water cost based on amount of January precipitation (P1)
  water_cost <- if (P1 < 100) {
    base_water_cost + (100 - P1) * 2
  } else {
    base_water_cost
  }
  
  # Net profit and total
  net_profit_per_acre <- profit_per_acre - water_cost
  total_profit <- acres * net_profit_per_acre
  
  return(list(total_profit))
}
