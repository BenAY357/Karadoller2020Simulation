#' @title Power analysis
#' @description Conducts a power analysis on a data frame with model outputs
#' @param df \code{tibble} containing model outputs.
#' @param effect_of_interest String argument
#'
#' @return numeric value
#' @export
#'
#' @examples \dontrun{
#' power_table %>% power_analysis(effect_of_interest = "age_dev")}
#'
power_analysis <- function(df, effect_of_interest){

  df <- df %>% filter(term==effect_of_interest) # get area of interest
  power <- df %>% pull(sig)%>% sum()/100 # work out percentage of times a significant effect is observed.
  power
}
