#ESM 232
#Taylor Cook


#$E = A * r * H * PR$

  

#' Energy produced from a photovoltaic system 
#'
#' a function that computes energy produced from a photovoltaic system if you know the average annual solar radiation.
#' @param A is the solar panel area (m2)
#' @param r is panel yield (0-1) (manufacture efficiency - usually around 0.2)
#' @param PR is performance ratio (0-1) (accounting for site factors that impact efficiency usually around 0.75) 
#' @param H is annual average solar radiation (kWh)
#'
#' @return E, the energy produced by the system (kWh)



pv_energy = function(A, r=.2, PR=0.75, H) {
  E = A*r*H*PR
  return(E)
}
