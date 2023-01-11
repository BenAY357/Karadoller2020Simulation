
\name{prev_sim_data}
\alias{prev_sim_data}
\title{
Simulation of Karadoller et al.'s (2020) relational lexeme data.
}
\description{
Contains a simulation of Karadoller et al.'s (2020) RL data. It was created with \code{sim_df(tau_0 = 0.9)}.

Each row is a trial.
}
\usage{
sim_data <- Karadoller2020::get_csv("prev_sim_data.csv")
}
\format{A data frame containing 626 rows and 20 variables.

 \tabular{rlll}{
    [,1] \tab subject \tab numeric \tab subject number\cr
    [,2] \tab language \tab character \tab late/ native signer\cr
    [,3] \tab age \tab character \tab child/ adult\cr
    [,4] \tab viewpoint \tab character \tab dependent/ independent\cr
    [,5] \tab spatial encoding \tab numeric \tab incorrect (0)/ correct (1)\cr
    [,6] \tab lang_dev\tab numeric \tab late (-0.5)/native (0.5) \cr
    [,7] \tab age_dev     \tab numeric \tab child (-0.5)/ adult (0.5)\cr
    [,8] \tab viewpoint_dev \tab numeric \tab independent (-0.5)/ dependent (0.5)\cr
    [,9] \tab b0     \tab numeric \tab intercept coeff\cr
    [,10] \tab b1     \tab numeric \tab age coeff \cr
    [,11] \tab b2     \tab numeric \tab language coeff \cr
    [,12] \tab b3     \tab numeric \tab viewpoint coeff \cr
    [,13] \tab b4     \tab numeric \tab age:language coeff \cr
    [,14] \tab b5     \tab numeric \tab age:viewpoint coeff \cr
    [,15] \tab b6     \tab numeric \tab language:viewpoint coeff \cr
    [,16] \tab b7     \tab numeric \tab language:viewpoint coeff \cr
    [,17] \tab u0p\tab numeric \tab subject random effect\cr
    [,18] \tab logodds     \tab numeric \tab logg odds of producing a RL\cr
    [,19] \tab probability    \tab numeric \tab probability of producing a RL\cr
    [,20] \tab RL     \tab numeric \tab presence of a RL. 1 = yes, 0 = no
  }
}
\note{RL stands for relational lexeme.}


\docType{data}


