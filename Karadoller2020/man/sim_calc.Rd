% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_calc.R
\name{sim_calc}
\alias{sim_calc}
\title{Run Model}
\usage{
sim_calc(df, check_convergence = FALSE)
}
\arguments{
\item{df}{\code{tibble}}

\item{check_convergence}{Logical. Check convergence if TRUE. "check_convergence = FALSE" by default.}
}
\value{
\code{tibble} containing the model outputs of the fixed effect.
}
\description{
Run the model for the simulation
}
\examples{
\dontrun{
# no sensitivity analysis

sim_data \%>\% sim_calc() # will produce the outputs for all of the fixed effect.

# sensitivity_analysis = TRUE

sim_data \%>\% sim_calc(sensitivity_analysis = TRUE, )

}
}
