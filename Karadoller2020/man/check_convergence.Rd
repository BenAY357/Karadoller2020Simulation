% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_convergence.R
\name{check_convergence}
\alias{check_convergence}
\title{Check model for convergence}
\usage{
check_convergence(mod)
}
\arguments{
\item{mod}{model output}
}
\value{
One of 3 numeric arguments. 1: no convergence issues. 0: model is singular. -1: Another convergence problem
}
\description{
Check model for convergence
}
\examples{
\dontrun{
sim_mod \%>\% check_convergence()}
}
