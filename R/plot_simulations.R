#' Plot the results of a simulation run.
#'
#' @importFrom graphics abline arrows plot
#' @importFrom stats qnorm
#'
#' @export
#'
#' @param results A matrix specifying the (simulated) values with lower and higher limits of the corresponding confidence intervals.
#' @param xlab A title for the x axis.
#' @param ylab A title for the y axis.
#' @param ylim The limits of the y axis.
#' @param true_value True value 
#' @param error Logical: should the confidence intervals be presented? Default = false.
#' @param reverse Logical: should the (sorted) results be reversed? Default = false.
#' @param sort Logical: should the results be sorted? Default = true.
#' @param main A title for the plot.
#'
#' @return Plot with simulation results.
#'
#' @examples
#' # Create a default universe with 50 ng DNA input.
#' universe(50)

plot_simulations = function(results, xlab="Simulation", ylab="Result", ylim = NULL, true_value = NULL, error = F, reverse = F, sort = T, main = NULL) {

  if (sort) {
    results = results[order(results[,1]),]
  }

  if (is.null(ylim) & error) {
    ylim = c(min(results), max(results))
  }

  plot(x = 1:nrow(results),
       y = results[,1],
       pch = 16,
       xlab = xlab,
       ylab = ylab,
       ylim = ylim,
       bty = "l",
       main = main)

  if (!is.null(true_value)) {
    abline(h = true_value, lty = 2)
  }

  if (error) {
    col = rep("black", nrow(results))
    if (!is.null(true_value)) {
      col[which(results[,2] > true_value | results[,3] < true_value)] = "red"
    }
    if (reverse) {
      col = rep("red", nrow(results))
      if (!is.null(true_value)) {
        col[which(results[,2] > true_value | results[,3] < true_value)] = "black"
      }
    }
    arrows(1:nrow(results), results[,2], 1:nrow(results), results[,3], length = 0.05, angle = 90, code = 3, col = col)
  }
}
