#Computes profit from almond yield using functions: almondyield() and almond_profit(). 


#' Almond Yield and Profit wrapper function
#'
#' @param year # year of the data (1988-2010)
#' @param precip # precipitation in mm
#' @param min_temp # minimum temperature in degrees Celsius
#' @param acres # farm size in acres
#' @return # data frame with year, yield anomaly, yield, and profit
#'
#'#' @author Taylor Cook and Kelsey Warren

almond_wrapper <- function(year, precip, min_temp, acres){
  
  #coefficients kept the same
  coefficients <- c(-0.015, -0.0046, -0.07, 0.0043, 0.28)
  
  #yield anomaly function
  yield_anomaly <- coefficients[1] * min_temp + coefficients[2] * min_temp^2 + coefficients[3] * precip + coefficients[4] * precip^2 + coefficients[5]
  
  
  baseline_profit <- 4000 
  price_per_ton <- 6000 # $6000 per ton assumption
  
  #profit as a function of total yield, price per ton, profit margin
  profit <-  baseline_profit + (yield_anomaly * price_per_ton * acres) 
 
  # Return data frame with the year, yield anomaly, and profit
  return(data.frame(year = year, yield_anomaly = yield_anomaly, profit = profit))
}






