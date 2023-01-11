#' @title Get RL Model Estimates
#' @description Generates a data frame containing the model estimates (B) for the fixed effects from the glmer model for frequency of RL use. (Table 5)
#' @return A data frame containing the model estimates for the fixed effects from the glmer model for frequency of RL use.
#' @export
#'
#' @examples get_RL_model_estimates()
get_RL_model_estimates <- function(){

  RL_model_b_df <- tibble(parameter = c("b0","b1","b2","b3","b4","b5", "b6", "b7"),
                          value = c(-1.76,0.785,0.536,-0.084,-1.612,1.136,0.171,0.618),
                          description = c("intercept mean",
                                          "age coeff",
                                          "langauge coeff",
                                          "viewpoint coeff",
                                          "age:language coeff",
                                          "age:viewpoint coeff",
                                          "language:viewpoint coeff",
                                          "age:language:viewpoint coeff"))

  return(RL_model_b_df)
}
