#' @title Check RL descriptive statistics
#' @description Creates a table comparing the original and simulated RL descriptive statistics.
#' @param orig \code{tibble} original descriptive statistisc. From "get_desc_RL()"
#' @param sim \code{tibble} simulated data frame
#'
#' @return \code{tibble}
#' @export
#'
#' @examples
#' \dontrun{
#' orig_desc <- get_desc_RL()
#' check_RL_desc(orig_desc, sim_data) # where sim_data is the simulated data set.
#' }
check_RL_desc <- function(orig, sim){

  green = "#71CA97"

  red = "#ff7f7f"

  diff_formatter <- formatter("span", # format diff column.
                              style = x ~ style(font.weight = "bold",
                                                color = ifelse(x > 0, green, ifelse(x < 0, red, "black"))),
                              x ~ icontext(case_when(
                                x>0 ~ "arrow-up", #
                                x<0 ~ "arrow-down",
                                x == 0 ~ "thumbs-up"), x)
  )

  sim_RL_desc <- sim %>% group_by(age, language, viewpoint) %>% summarise(sim_mean = mean(RL) %>% round(2),
                                                                          sim_sd = sd(RL) %>% round(2))

  check_RL_desc <- inner_join(orig, sim_RL_desc, by = c("language", "age", "viewpoint"))

  check_RL_desc <- check_RL_desc %>% mutate(mean_diff = sim_mean - mean, # compute differences
                                            sd_diff = sim_sd - sd)




  format_check_RL <- formattable(check_RL_desc, align =c("l","l","l","r","r", "r", "r", "r", "r", "r", "r"), list(
    mean_diff = diff_formatter,
    sd_diff = diff_formatter

  )
  )
  format_check_RL %>% mutate(mean_diff = mean_diff %>% round(2), # round mean and sd diff. formattable() "unrounds" them for some reason.
                             sd_diff = sd_diff %>% round(2))

}
