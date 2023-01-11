#' @title Simulate RL from log odds
#'
#' @param df data frame
#' @param logodds_col name of the log odds column."logodds_col = logodds" by default
#'
#' @return A data frame containing the simulated RL based on the log odds column.
#' @export
#'
#' @examples \dontrun{
#' sim_data %>% sim_RL_from_logodds()
#' }
sim_RL_from_logodds <- function(df){

 df <-  df %>% mutate(probability = logistic(logodds)) %>%

    mutate(RL = rbinom(n = nrow(df),
                       size = 1,
                       prob = probability))
  return(df)

}
