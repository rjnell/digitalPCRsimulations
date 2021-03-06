% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_ratio.R
\name{calc_ratio}
\alias{calc_ratio}
\title{Calculate ratio with confidence intervals of two values with known confidence intervals.}
\usage{
calc_ratio(input_a, input_b)
}
\arguments{
\item{input_a}{A vector specifying the value, lower and higher limit of the corresponding confidence interval.}

\item{input_b}{A vector specifying the value, lower and higher limit of the corresponding confidence interval.}
}
\value{
Ratio with confidence interval.
}
\description{
As described in Dube et al., the geometric interpretation of Fieller's theorem can be used to calculate the confidence intervals for a ratio of two values with known confidence intervals.
}
\examples{
# Create two default universes simulating 50 ng DNA input.
universe_target = universe(50)
universe_reference = universe(50)

# Take two samples
sample = sample(1:length(universe_target), 20000)
conc_target = sample_from_universe(universe_target, sample)
conc_reference = sample_from_universe(universe_target, sample)

# Calculate the ratio
ratio = calc_ratio(conc_target, conc_reference)
ratio
}
