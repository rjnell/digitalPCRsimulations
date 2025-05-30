#' Calculate statistics from simulation result table.
#' 
#' @importFrom stats sd
#'
#' @export
#'
#' @param results A matrix specifying the (simulated) values with lower and higher limits of the corresponding confidence intervals.
#' @param true_value The true value which was simulated.
#'
#' @return List with statistics.
#'
#' @examples
#' # Create two default universes simulating 50 ng DNA input.
#' universe_target = universe(50)
#' universe_reference = universe(50)
#'
#' # Take two samples
#' sample = sample(1:length(universe_target), 20000)
#' conc_target = sample_from_universe(universe_target, sample)
#' conc_reference = sample_from_universe(universe_target, sample)
#'
#' # Calculate the ratio
#' ratio = calc_ratio(conc_target, conc_reference)
#' ratio

# Function to calculate stats
stats = function(results, true_value) {
  return = list()

  return$not_in_interval = length(which(results[,2] > true_value | results[,3] < true_value))
  return$in_interval = nrow(results) - return$not_in_interval
  return$coverage = return$in_interval / nrow(results)
  return$point_estimate_mean = mean(results[,1])
  return$point_estimate_sd = sd(results[,1])

  return(return)
}

