#' almond_profit
#'
#' Calculate total almond profit based on yield anomaly and water costs
#'
#' @param df A dataframe with columns: year, month, tmin_c, precip_mm
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


almond_profit <- function(df, month, acres = 500, baseline_profit = 4000, 
                          price_per_ton = 6000, base_water_cost = 200) {
  # Assign month input to target month and month prior
  month1 <- as.numeric(month)
  if (month1 == 1) {
    month2 = 12
  } else {
    month2 <- month1 - 1
  }
  
  # Filter for target month and compute Tn (min temp)
  df_temp <- df %>%
    filter(month == month1) %>%
    group_by(month, year) %>%
    summarize(Tn = min(tmin_c, na.rm = TRUE), .groups = "drop")
  
  # Filter for previous month and compute P1 (total precip)
  df_precip <- df %>%
    filter(month == month2) %>%
    group_by(year) %>%
    summarize(P1 = sum(precip, na.rm = TRUE), .groups = "drop")
  
  # Join both datasets by year
  df_joined <- inner_join(df_temp, df_precip, by = "year")
  
  # Check for missing data
  if (nrow(df_joined) == 0) return(NA)
  
  # Use first (or only) year's values
  Tn <- df_joined$Tn[1]
  P1 <- df_joined$P1[1]
  
  if (is.na(Tn) || is.na(P1)) return(NA)
  
  # Yield anomaly (Lobell et al. 2006)
  yield_anomaly <- -0.015 * Tn - 0.0046 * Tn^2 - 0.07 * P1 + 0.0043 * P1^2 + 0.28
  
  # Calculate profit per acre
  profit_per_acre <- baseline_profit + (yield_anomaly * price_per_ton)
  
  # Adjust water cost if P1 is low (if P1 < 100 mm, water cost increases, otherwise, water cost = baseline)
  water_cost <- if (P1 < 100) {
    base_water_cost + (100 - P1) * 2
  } else {
    base_water_cost
  }
  
  # Net profit and total
  net_profit_per_acre <- profit_per_acre - water_cost
  total_profit <- acres * net_profit_per_acre
  
  return(total_profit)
}
