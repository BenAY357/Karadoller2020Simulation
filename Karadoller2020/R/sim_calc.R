#' @title Run Model
#' @description Run the model for the simulation
#'
#' @param check_convergence Logical. Check convergence if TRUE. "check_convergence = FALSE" by default.
#' @param df \code{tibble}
#'
#' @return \code{tibble} containing the model outputs of the fixed effect.
#' @export
#'
#' @examples \dontrun{
#' # no sensitivity analysis
#'
#' sim_data %>% sim_calc() # will produce the outputs for all of the fixed effect.
#'
#' # sensitivity_analysis = TRUE
#'
#' sim_data %>% sim_calc(sensitivity_analysis = TRUE, )
#'
#' }
sim_calc <- function(df, check_convergence = FALSE){

  sim_mod <- glmer(RL ~ age_dev * language_dev * viewpoint_dev + (1|subject),
                   family =  "binomial",
                   data = df,
                   control = glmerControl(optimizer="bobyqa"))

  if(check_convergence){ # add convergence indicator

  conv <- Karadoller2020::check_convergence(sim_mod)
  sim_mod <- sim_mod %>% tidy(effects='fixed') %>%
    mutate(converge=conv) ### this line adds convergence info to the table
  }

  else{
    sim_mod <- sim_mod %>% tidy(effects = "fixed")
  }


  return(sim_mod)
}

