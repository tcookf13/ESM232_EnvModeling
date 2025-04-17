#Almond yield function for assignment 2
#Computes almond yield anomaly based on minimum temperature and precipitation.The yield anomaly is calculated using a simple model based on Lobell et al. (2006), which takes into account temperature and precipitation data from specific months.


#Y = 0.015Tn₂ – 0.0046T²n₂ – 0.07P₁ + 0.0043P² ₁ + 0.28

#Y represents yield anomaly (ton acre"1). 
#Subscript numbers indicate month of climate variable, with negative values denoting a month from the year prior to harvest. Tn, minimum temperature (8C); Tx, maximum temperature (8C); P, precipitation (mm).



#' Computes almond yield anomaly based on minimum temperature and precipitation.
#' @param year The year for which to calculate the yield anomaly
#' @param clim Dataframe containing climate data with columns: day, month, year, tmax_C, tmin_C, and precip (mm)
#' @param coeffs A vector of model coefficients (e.g., c(0.015, -0.0046, -0.07, 0.0043, 0.28)) from Lobell et al. (2006)
#' @return Yield anomaly (ton/acre)
#' @author Taylor Cook and Kelsey Warren
#' 
#' function definition

almondyield <- function(year, clim) {
  # Define the coefficients for the model
  coeffs <- c(-0.015, -0.0046, -0.07, 0.0043, 0.28)
  
  
  # CLIMATE DATA PREP
  # february data for Tn2 (min temp for Feb)
  feb_data <- clim[clim$month == 2 & clim$year == year, ]
  Tn2 <- mean(feb_data$tmin_c, na.rm = TRUE)

  # january data for P1 (precip for Jan)
  jan_data <- clim[clim$month == 1 & clim$year == year, ]
  P1 <- sum(jan_data$precip, na.rm = TRUE)
  
  
  
  # ERROR CHECKING
  # error checking for the coefficients
  if(length(coeffs) != 5) {
    stop("The 'coeffs' vector must have exactly 5 coefficients.")
  }
  # error checking for Feb minimum temperature (C)
  Tn2 = ifelse( (Tn2 < 50), Tn2, stop("Caution: abnormally high minimum temperature"))
  # error checking for Jan precip (mm)
  P1 = ifelse( (P1 < 15000), P1, stop("Caution: abnormally high precipitation"))
  
  
  
  # Calculate the yield anomaly using the model
  yield_anomaly <- coeffs[1] * Tn2 + coeffs[2] * Tn2^2 + coeffs[3] * P1 + coeffs[4] * P1^2 + coeffs[5]
  
  # Return the yield anomaly
  return(yield_anomaly)
}

