# Define the parameters
r <- 0.1 # growth rate for forests below threshold canopy cover
g <- 0.05 # growth rate for forests at or above threshold canopy cover
K <- 100 # carrying capacity (threshold canopy cover)
t <- seq(0, 100, by = 1) # time sequence
C0 <- 10 # initial carbon (C) value
# Define the differential equation
forest_growth <- function(t, C, r, g, K) {
  if (C < K) {
    dCdt <- r * C
  } else {
    dCdt <- g * (1 - C/K)
  }
  return(dCdt)
}