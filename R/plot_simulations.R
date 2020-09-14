#' Plot the results of a simulation run
#'
#' Lorem ipsum
#'
#' @importFrom graphics abline arrows plot
#' @importFrom stats qnorm
#'
#' @export
#'
#' @param results A vector specifying the value, lower and higher limit of the corresponding confidence interval.
#' @param xlab Lorem ipsum.
#' @param ylab Lorem ipsum.
#' @param ylim Lorem ipsum.
#' @param true_value Lorem ipsum.
#' @param error Lorem ipsum.
#' @param sort Lorem ipsum.
#' @param main Lorem ipsum.
#'
#' @return Plot
#'
#' @examples
#' # Create a default universe with 50 ng DNA input.
#' universe(50)

plot_simulations = function(results, xlab="Simulation", ylab="Result", ylim = NULL, true_value=NULL, error=F, sort=T, main=NULL) {

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
    abline(h = true_value, lty=2)
  }

  if (error) {
    col = rep("black", nrow(results))
    if (!is.null(true_value)) {
      col[which(results[,2] > true_value | results[,3] < true_value)] = "red"
    }
    arrows(1:nrow(results), results[,2], 1:nrow(results), results[,3], length=0.05, angle=90, code=3, col=col)
  }
}
