#' Calculate ratio with confidence intervals of two values with known confidence intervals.
#'
#' As described in Dube et al., the geometric interpretation of Fieller's theorem can be used to calculate the confidence intervals for a ratio of two values with known confidence intervals.
#'
#' @export
#'
#' @param input_a A vector specifying the value, lower and higher limit of the corresponding confidence interval.
#' @param input_b A vector specifying the value, lower and higher limit of the corresponding confidence interval.
#'
#' @return Ratio with confidence interval.
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

calc_ratio = function(input_a, input_b) {

  # Set a
  a_low = input_a[2]
  a_high = input_a[3]
  a = input_a[1]

  # Set b
  b_low = input_b[2]
  b_high = input_b[3]
  b = input_b[1]

  # Calculate the ratio...
  ratio = a/b

  # ...and its CI
  h_top = a_high - a
  h_bottom = a - a_low
  w_right = b_high - b
  w_left = b - b_low
  ratio_low = (a*b - sqrt(a^2*b^2 - (h_bottom^2 - a^2)*(w_right^2 - b^2))) / (b^2 - w_right^2)
  ratio_high = (a*b + sqrt(a^2*b^2 - (h_top^2 - a^2)*(w_left^2 - b^2))) / (b^2 - w_left^2)

  # Return the calculated ratio with its CI
  ratio_ci = c(ratio, ratio_low, ratio_high)
  names(ratio_ci) = c("ratio", "ratio_low", "ratio_high")
  return(ratio_ci)
}
