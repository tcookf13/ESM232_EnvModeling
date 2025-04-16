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
almond_wrapper <- function(year, clim) {

# Step 1: Calculate the yield anomaly using the almondyield function
yield_anomaly <- almondyield(year = unique(clim$year), clim = clim)

# Step 2: Create a dataframe to store results (year studied, yield anomaly, and profit)
profit_df <- data.frame(year = year, yield_anomaly = yield, profit = numeric())

# Step 3: Calculate the profit using the almond_profit function and fill the profit_df data frame
profit_df$profit <- almond_profit(yield_anomaly, clim, acres = 500, baseline_profit = 4000, 
                                  price_per_ton = 6000, base_water_cost = 200)

# Return the profit calculated
return(profit_df)
}

