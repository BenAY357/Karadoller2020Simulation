#' @title Mean and SD proportions of relational lexemes across age, language and viewpoint
#' @description Generates a data frame containing the mean and sd proportions of relational lexemes. Taken from table 3.
#' @param kable_fomart. Logical. If TRUE apply kable styling to data frame. "kable_format = FALSE" by default.
#' @return A data frame containing the mean and sd proportions of relational lexemes by participant.
#' @export
#'
#' @examples get_desc_RL()
get_desc_RL <- function(kable_format = FALSE){

  age <- c("adult", "child")
  language <- c("native", "late")
  viewpoint <- c("independent", "dependent")
  RL_mean_sd <- crossing(language, age, viewpoint)
  RL_mean_sd <- RL_mean_sd %>% mutate(mean = c(.28, .29,
                                                          .06,.14,
                                                          .28,.21,
                                                          .18,.33),
                                              sd = c(0.45, 0.45,0.23,0.35, 0.45,0.41,0.39,0.47))
  if (kable_format){

  RL_mean_sd <- RL_mean_sd %>% knitr::kable() %>%
    kable_styling()

}

  return(RL_mean_sd)

}
