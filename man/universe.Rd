% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/universe.R
\name{universe}
\alias{universe}
\title{Create a universe.}
\usage{
universe(input, n_size = 100)
}
\arguments{
\item{input}{Input in ng genomic diploid DNA per 20,000 droplets.}

\item{n_size}{Integer specifying the finite size of the universe (x 20,000 droplets), default = 100.}
}
\value{
A universe of the given size.
}
\description{
A droplet digital PCR experiment may be seen as a random statistical sampling of an infinite universe consisting of droplets filled with DNA molecules.
To simulate this process, we create a finite, but very large universe with empty droplets.
Based on a given CPD (=copies/droplet), droplets in this universe are randomly filled. These tasks are performed by the following function `universe`:
}
\details{
Terst
}
\examples{
# Create a default universe simulating 50 ng DNA input.
universe(50)
}
