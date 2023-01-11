#' @title Get Trial Parameters
#' @description Creates data frame containing the trial parameters
#' @return Data frame containing the trial parameters (number of trials, number of view dependent trials, number of view independent trials)
#' @export
#'
#' @examples get_trial_params()
get_trial_params <- function(){

  trial_params <- tibble(parameter = c("trials", "view dependent trials","view independent trials"), N = c(15,9,6))

  return(trial_params)


}
