#Almond Profit Function for Assignment 3
#Computes profit from almond yield using functions: almondyield() and almond_profit(). 


#' Computes almond yield anomaly based on minimum temperature and precipitation.
#' @param year The year for which to calculate the yield anomaly
#' @param clim Dataframe containing climate data with columns: day, month, year, tmax_C, tmin_C, and precip (mm) -- used for determining water cost
#' @return profit ($)
#' @author Taylor Cook and Kelsey Warren
#' 
#' function definition
#' 


almond_wrapper <- function(years, clim) {
  # Create an empty data frame to store results (year(s) studied, yield anomaly, and profit)
  profit_df <- data.frame(year = integer(), yield_anomaly = numeric(), profit = numeric())
  
  # for loop through each year to calculate yield anomaly and profit
  for (year in years) {
    # Step 1: Calculate the yield anomaly using the almondyield function
    yield_anomaly <- almondyield(year = year, clim = clim)
    
    # Step 2: Calculate the profit using the almond_profit function
    profit_result <- almond_profit(
      yield_anomaly = yield_anomaly,
      clim = clim[clim$year == year, ],
      acres = 500,
      baseline_profit = 4000,
      price_per_ton = 6000,
      base_water_cost = 200
    )
    
    # Extract the total profit (which is a list, we want the numeric value)
    total_profit <- profit_result
    
    # Step 3: Store the results for the year in the data frame
    profit_df <- rbind(profit_df, data.frame(year = year, yield_anomaly = yield_anomaly, profit = total_profit))
  }
  
  # Return the data frame with the results
  return(profit_df)
}

















