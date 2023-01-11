#' @title Simulate data and run model
#'
#' @param sensitivity_analysis Logical. Allows the number of participants and value of one coefficient to be manually overridden if TRUE. "sensitivity_analysis = FALSE" by default.
#' @param n_participants Numeric argument. Only needed if "sensitivity_analysis = TRUE". Specify total number of participants. Must be divisible by the number of groups (4 in this case) without a remainder.
#' @param change_coeff String argument. Only needed if "sensitivity_analysis = TRUE". Specifies the changed coefficient for the sensitivity analysis. E.g. "b0","b1". NOT "age" or "age_dev".
#' @param change_coeff_value Numeric argument. Only needed if "sensitivity_analysis = TRUE". Value of changed coefficient
#' @param check_convergence Logical. Check convergence if TRUE. "check_convergence = FALSE" by default.
#' @param effect_of_interest String argument. Only needed if "sensitivity_analysis = TRUE".
#'
#' @return "sensitivity_analysis = FALSE: \code{tibble} containing the model outputs of the fixed effects. "sensitivity_analysis = TRUE": return power (numeric argument)
#' @export
#'
#' @examples
#' # Normal version
#' sim_whole()
#'
#' # Sensitivity analysis
#'
#' sim_whole(sensitivity_analysis = TRUE,
#' n_participants = 60, # 60 participants in total (15 in each condition)
#' change_coef = "b1", # change "b2" (language coef)
#' change_coef_value = 90 # change "b2" to 90.
#' )
#'
sim_whole <- function(sensitivity_analysis = FALSE,
                      n_participants,
                      change_coeff,
                      change_coeff_value,
                      check_convergence = FALSE,
                      effect_of_interest,
                      tau_0 = 0.9){

  # pass same arguments to sim_df

  if (sensitivity_analysis){ # run sensitivity analysis

    df <-  Karadoller2020::sim_df(sensitivity_analysis = TRUE, # simulate the data with manual overriding of the number of participants and 1 effect size enabled.
                                  n_participants = n_participants,
                                  change_coeff = change_coeff,
                                  change_coeff_value = change_coeff_value,
                                  tau_0 = tau_0)

    model_output <- df %>% Karadoller2020::sim_calc(check_convergence = check_convergence) # get the model outputs of the fixed effects.
    model_output <- model_output %>% filter(term == effect_of_interest) # we only want the effect of interest for the sensitivity analysis
   # model_output <- model_output$p.value # get the p value. future_map2 apparently doesn't like simulating data frames.

  } else { # normal simulation
    df <-  Karadoller2020::sim_df(tau_0 = tau_0)
    model_output <- df %>% Karadoller2020::sim_calc(check_convergence = check_convergence) # get the model outputs of the fixed effects.
  }
  return(model_output)

}
