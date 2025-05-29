#' Lotka-Volterra model with hunting
#'
#' @param t # Time for ODE solver 
#' @param pop # Concatenated list of population start parameters for 'prey' and 'pred'
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, par$pmort
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators
#'  \emph{alpha} is a interaction coefficient (higher values greater interaction
#'  \emph{pmort} mortality rate of predictor population
#'  \emph{rhunt} rate of hunting as a percentage of the prey population (E.g. 5% of prey hunted, rhunt=0.05)
#'  \emph{Th} Minimum threshold population of prey to allow hunting  
#'
#' @return
#' @export
#'
#' @examples
lotvmodK_hunt <- function(t, pop, pars) {
  with(as.list(c(pop, pars)), {
    dprey = rprey*(1-prey/K)*prey -  alpha*prey*pred
    if(prey >= Th){
      dprey = rprey*(1-prey/K)*prey -  alpha*prey*pred -  rhunt*prey
    }
    dpred = eff*alpha*prey*pred - pmort*pred
    return(list(c(dprey, dpred)))
  })
}