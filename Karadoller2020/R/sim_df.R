#' @title Simulate the data
#' @description Runs the simulation code for one data frame.
#' @param tau_0 Numeric argument. Subject random effect standard deviation. "tau_0 = 0.9" by default.
#' @param sensitivity_analysis Logical. Allows the number of participants and value of one coefficient to be manually overridden if TRUE. "sensitivity_analysis = FALSE" by default.
#' @param n_participants Numeric argument. Specify total number of participants. Must be divisible by the number of groups (4 in this case) without a remainder. Ensures that the groups are balanced.
#' @param change_coeff String argument. Specifies the changed coefficient for the sensitivity analysis. E.g. "b0","b1". NOT "age" or "age_dev".
#' @param change_coeff_value Numeric argument. Value of changed coefficient
#' @return \code{tibble} containing simulated data.
#' @export
#'
#' @examples
#' # Normal version
#' sim_df() %>% head()
#'
#' # Sensitivity analysis
#'
#' sim_df(sensitivity_analysis = TRUE,
#' n_participants = 60, # 60 participants in total (15 in each condition)
#' change_coef = "b1", # change "b1" (age coef)
#' change_coef_value = 90 # change "b1" to 90
#' ) %>% head()
#'
sim_df<- function(tau_0 = 0.9,
                    sensitivity_analysis = FALSE,
                     n_participants,
                     change_coeff,
                     change_coeff_value){

  # First we need to simulate the likelihood that the participants got the spatial encoding correct.

  # get trial, group, and model parameters.
  trial_params <- Karadoller2020::get_trial_params()
  n_trials_param <- as.numeric(trial_params[1,2])
  group_params <- Karadoller2020::get_group_params() # the likelihood each group gets the correct spatial encoding can be found in Table 1 in the paper.
  mod_params <- Karadoller2020::get_RL_model_estimates() # get model estimates

  if (sensitivity_analysis){ # change total number of participants for the sensitivity analysis.
    group_params <- group_params %>% mutate(n = n_participants/nrow(group_params)) # assume that participants are even split across the conditions.

  }

  # simulate correct spatial encodings
  spatial_encoding <- Karadoller2020::sim_spatial_encoding(group_params, trial_params)

  # filter out all incorrect responses
  all_correct <- spatial_encoding %>% filter(spatial_encoding == 1)

  # add parameters to data frame

  sim_data <- all_correct %>% Karadoller2020::add_estimate_params(mod_params) %>% # add model estimates to data frame
    Karadoller2020::add_subj_rand_eff(tau_0 = tau_0) # add subject random effect to data frame.

  if (sensitivity_analysis){ # change coefficient of a fixed effect.
    sim_data[[change_coeff]] <- change_coeff_value
  }

  # work out log odds
  sim_data <- sim_data %>%
    mutate(logodds = b0 + u0p + # grand intercept and subject random effect
             # coefficients
             b1*age_dev +
             b2*language_dev +
             b3*viewpoint_dev+
             b4*age_dev*language_dev+
             b5*age_dev*viewpoint_dev+
             b6*language_dev*viewpoint_dev+
             b7*age_dev*language_dev*viewpoint_dev) %>%
    mutate(probability = logistic(logodds)) %>%
    mutate(RL = rbinom(n = nrow(sim_data),
                       size = 1,
                       prob = probability))

sim_data
}

