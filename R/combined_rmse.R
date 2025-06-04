#' Compute rmse between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year

compute_all_metrics <- function(m, o, month) {
  # bind the input vectors into a dataframe
  flow_data = cbind.data.frame(m, o, month)
  # group by monthly flow and water year and find the sum of flow values
  flow_monthly <- flow_data %>%
    group_by(month) %>%
    summarize(model = sum(m),
              obs = sum(o))
  # calculate absolute errors
  rmse <- sqrt(mean((flow_monthly$obs - flow_monthly$model)^2))
  # Normalize RMSE values between 0 and 1
  rmse_normal <- rmse / max(flow_monthly$obs)
  # calculate correlation coefficient
  cor <- cor(m, o)
  # combine the two metrics by subtraction since they are both on a scale from 0-1, with higher values of correlation indicating better M.P., and higher values of rmse indicating worse M.P.
  combined_metric <- cor - rmse_normal 
  return(list(rmse_normal = rmse_normal, cor = cor, combined_metric = combined_metric))
}
