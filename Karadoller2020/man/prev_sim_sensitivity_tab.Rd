
\name{prev_sim_sensitivity_tab}
\alias{prev_sim_sensitivity_tab}
\title{
Sensitivity table
}
\description{

Power to detect a significant effect of age for various sample and age effect sizes.


}
\usage{
sensitivity_tab <- Karadoller2020::get_csv("prev_sim_sensitivity_tab.csv")
}
\format{A data frame containing 42 rows and 3 variables.

 \tabular{rlll}{
    [,1] \tab sample \tab numeric \tab  sample size. \code{seq(from = 40, to = 140, by = 20)}\cr
    [,2] \tab age_coeff \tab numeric \tab age effect size \code{seq(from  = 0.3, to = 0.9, by = 0.1)}\cr
    [,3] \tab power \tab numeric \tab power to detect a significant effect size\cr
  }
}



\docType{data}


