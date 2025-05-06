# Write function that takes the results of diff1 and returns a summary of the maxconcentration
maxconc <- function(result) {
  # result is a list with conc, qin, and qout
  # get the maximum concentration from the conc matrix
  max_conc <- max(result$conc)
  
  # get the time and position of the maximum concentration
  max_time <- which(result$conc == max_conc, arr.ind = TRUE)[1]
  max_position <- which(result$conc == max_conc, arr.ind = TRUE)[2]
  
  # return a list with the maximum concentration, time, and position
  return(list(max_concentration = max_conc, time = max_time, position = max_position))
}
