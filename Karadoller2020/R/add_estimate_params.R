#' @title Add Estimate Parameters
#' @description Add estimate parameters to a data frame.
#' @param df Data frame the estimate (B) parameters will be added to.
#' @param parameters Date frame containing the estimate parameters (from "get_RL_model_estimates()")
#'
#' @return A data frame with columns containing the model estimates added to it.
#' @export
#'
#' @examples \dontrun{
#' model_params <- get_RL_model_estimates()
#' all_correct %>% add_estimate_params(model_params)
#' }
add_estimate_params <- function(df, parameters){

  for (i in 1:nrow(parameters)){

    df[parameters$parameter[i]] <- rep(parameters$value[i])
  }

  return(df)
}
