#' Create a universe.
#'
#' A droplet digital PCR experiment may be seen as a random statistical sampling of an infinite universe consisting of droplets filled with DNA molecules.
#' To simulate this process, we create a finite, but very large universe with empty droplets.
#' Based on a given CPD (=copies/droplet), droplets in this universe are randomly filled. These tasks are performed by the following function `universe`:
#'
#' Please note that created universes are based on 20,000 droplets per experiment.
#' Taking a smaller sample size (e.g. 15,000 droplets) would simulate a loss of droplets during the experiment, leading to a loss of power.
#'
#' @export
#'
#' @param input Input in ng genomic diploid DNA per 20,000 droplets.
#' @param n_size Integer specifying the finite size of the universe (x 20,000 droplets), default = 100.
#'
#' @return A universe of the given size.
#'
#' @examples
#' # Create a default universe simulating 50 ng DNA input.
#' universe(50)

universe = function(input, n_size = 100) {

  # Define the concentration
  # 1 g of DNA contains \(3.59 * 10^{12}\) alleles.
  # 1 ng of DNA contains \(3.59 * 10^{12} * 10^{9}\) alleles.
  cpd = input * (10^-9) / (3.59 * 10^-12) / 20000

  # Define the size of the universe
  size = 20000 * n_size

  # Create a universe 'u' consisting of (size) empty droplets
  u = NULL
  u[1:(size)] = 0

  # Choose (cpd*size) droplets (not-unique, may be replaced) to fill with the molecules
  selection = sample(1:(size), cpd*size, replace = TRUE)
  u[selection] = 1

  # Return the created universe
  return(u)
}

