#' Simulate a sample: return a random selection of droplets.
#'
#' Lorem.
#'
#' @export
#'
#' @param sample_size A table with the results of the simulation.
#' @param universe The true value which was simulated.
#'
#' @return List with statistics.
#'
#' @examples
#' # Create two default universes simulating 50 ng DNA input.

simulate_sample = function (sample_size = 20000, universe = NULL) {

  universe_size = 1:(20000*100)
  if (!is.null(universe)) {
    universe_size = 1:length(universe)
  }

  return(sample(universe_size, sample_size, replace = FALSE))
}
