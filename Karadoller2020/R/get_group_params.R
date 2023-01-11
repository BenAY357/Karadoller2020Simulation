#' @title Get Group Parameters
#' @description Creates a data frame containing the group paramters. These can be found in Table 1 in the paper.
#' @return Data frame containing the group parameters. Number of participants by age groups and language status and their probability of getting a correct spatial encoding.
#' @export
#'
#' @examples get_group_params()
get_group_params <- function(){
  group_params <- tibble(language = c("native", "native", "late", "late"),
                             age = c("adult", "child", "adult", "child"),
                             n = c(12,11,12,9), # number of participants in each group
                             p_correct_spatial_encoding = c(.98,.94,.98,.85)) # taken from table 1. p_correct = Mean proportions of encoding a correct spatial relation)
  return(group_params)
}
