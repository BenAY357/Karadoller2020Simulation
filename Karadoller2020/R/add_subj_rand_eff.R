#' @title Add Subject Random Effect
#' @description Add the subject random effect to a data frame
#' @param df Data frame to add the subject random effect sd to.
#' @param tau_0 numeric input. Subject random effect SD
#'
#' @return A data frame with a column containing the subject random effect added to it.
#' @export
#'
#' @examples \dontrun{
#' # If the subject random effect SD = 0.5
#' all_correct %>% add_subj_rand_eff_sd(0.5)
#' }
add_subj_rand_eff <- function(df, tau_0){

  subject_rand_eff <- tibble(subject = rep(1:nrow(df)))

  subject_rand_eff[["u0p"]] <- rnorm(n = nrow(subject_rand_eff),
                                     mean = 0,
                                     sd = tau_0)
  df <- inner_join(df, subject_rand_eff, by = "subject")
  df

}
