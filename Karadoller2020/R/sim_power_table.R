#' @title Simulate Power Table
#' @description Simulate the power table. Simulates and analyses the data N times.
#' @param n_simulations Numeric argument. Number of simulations. "n_simulations = 100" by default.
#' @param alpha Numeric argument. Alpha value for the power analysis. Acceptable type one error rate. "alpha = 0.05" by default.
#' @return \code{tibble}. Contains the model outputs for N simulations.
#' @export
#' @examples
#' # Simulate and analyse the data twice.
#' sim_power_table(2)
sim_power_table <- function(
                            n_simulations = 100,
                           alpha = 0.05
                           ){

  sim_tab <- map_df(1:n_simulations, ~Karadoller2020::sim_whole())
  sim_tab <- sim_tab %>% mutate(sig=p.value<alpha) # return TRUE if p.value is below the alpha value (0.05 by default)
  sim_tab
}
