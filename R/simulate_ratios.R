#' Simulate ratios to be taken from two existing universes.
#'
#' As described in Dube et al., the geometric interpretation of Fieller's theorem can be used to calculate the confidence intervals for a ratio of two values with known confidence intervals.
#'
#' @export
#'
#' @param universe_1 A universe.
#' @param universe_2 A universe.
#' @param n_droplets Number of droplets (or partitions) to analyse per simulation.
#' @param n_simulations Number of simulations to perform.
#' @param alpha Level of significance, default = 0.05.
#'
#' @return Simulation results.
#'
#' @examples
#' #

simulate_ratios = function(universe_1, universe_2, n_droplets, n_simulations, alpha = 0.05) {
  ratios = NULL
  for (simulation in 1:n_simulations) {
    conc_1 = sample_from_universe(universe_1, n_droplets, alpha)
    conc_2 = sample_from_universe(universe_2, n_droplets, alpha)
    ratio = calc_ratio(conc_1, conc_2)
    ratios = rbind(ratios, ratio)
  }
  rownames(ratios) = paste0("simulation-", 1:n_simulations)
  return(ratios)
}
