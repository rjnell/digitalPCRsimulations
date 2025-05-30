#' Take a sample from an existing universe.
#'
#' Function to take a random sample of a given size from of a given universe.
#' A random statistical sample of a created universe is taken and converted to concentration result.
#'
#' @export
#'
#' @param universe An existing universe.
#' @param sample An integer specifying the size of the sample to be taken, or a vector indicating which elements of the universe should be sampled.
#' @param alpha Level of significance of returned confidence interval, default = 0.05.
#'
#' @return Concentration with confidence interval.
#'
#' @examples
#' # Create a default universe simulating 50 ng DNA input.
#' universe_50 = universe(50)
#'
#' # Take a sample of 20,000 droplets from this universe.
#' sample = sample_from_universe(universe_50, 20000)

sample_from_universe = function(universe, sample, alpha=0.95) {

  # If int is given as sample, create sample
  if (length(sample) == 1) {
    sample_selection = sample(1:length(universe), sample, replace = FALSE)
  }
  # Otherwise, use the given sample as sample_selection
  else {
    sample_selection = sample
  }

  # Define the sample
  droplets = length(sample_selection)

  # What is the observed percentage of filled sample droplets?
  p = sum(universe[sample_selection]) / droplets

  # Translate this percentage to concentration...
  conc = -log(1-p)

  # ... and its CI
  s = sqrt((p*(1-p))/droplets)
  z = qnorm(1-alpha/2)
  p_low = p - z * s
  conc_low = -log(1-p_low)
  p_high = p + z * s
  conc_high = -log(1-p_high)

  # Droplet volume 0.001 of 0.00085????
  volume = 0.00085

  # Return the calculated concentration with CI
  conc_ci = c(conc/0.00085, conc_low/0.00085, conc_high/0.00085)
  names(conc_ci) = c("concentration", "concentration_low", "concentration_high")
  return(conc_ci)
}
