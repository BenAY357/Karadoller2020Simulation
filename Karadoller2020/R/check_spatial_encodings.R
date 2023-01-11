#' @title Check spatial encodings
#' @description Check that percentage of correct spatial encodings is expected"
#'
#' @param group_parameters \code{tibble} containing the group parameters (from "get_group_params()").
#' @param n_trials numeric. number of trials per participant. "n_trials = 15" by default.
#' @param sim_df \code{tibble} containing the simulated data frame
#' @param formattable Logical. If TRUE format table. "formattable = TRUE" by default.
#'
#' @return \code{tibble}
#' @export
#'
#' @examples \dontrun{
#' check_spatial_encodings(group_parameters = group_params, sim_df = sim_data)
#' }
check_spatial_encodings <- function(group_parameters, n_trials = 15, sim_df, formattable = TRUE){

  total_trials_by_condition <- group_parameters %>% mutate(total_trials = n * n_trials) # add total number of trials to group parameters.

  sim_data_n_correct <- sim_data %>% group_by(age, language) %>% count() # count the number of trials for each group

  sim_data_n_correct <- sim_data_n_correct %>% rename(n_correct = n) # rename column

  check_spatial_encoding <- inner_join(total_trials_by_condition, sim_data_n_correct, by = c("language", "age")) # join data frames

  check_spatial_encoding <- check_spatial_encoding %>% mutate(sim_p_correct = (n_correct/total_trials)%>% round(2)) # work out the percentage of correct trials

  check_spatial_encoding <-  check_spatial_encoding %>% mutate(difference = sim_p_correct - p_correct_spatial_encoding) # compare simulated percentage to original percentage

  check_spatial_encoding <- check_spatial_encoding %>% select(language, age, p_correct_spatial_encoding, sim_p_correct, difference) # output relevant columns



  if(formattable){
    green = "#71CA97"

    red = "#ff7f7f"

    diff_formatter <- formatter("span", # format diff column.
                                style = x ~ style(font.weight = "bold",
                                                  color = ifelse(x > 0, green, ifelse(x < 0, red, "black"))),
                                x ~ icontext(case_when(
                                  x>0 ~ "arrow-up", #
                                  x<0 ~ "arrow-down",
                                  x == 0 ~ "thumbs-up"), x))


    check_spatial_encoding <- formattable(check_spatial_encoding, align =c("c","c","c"), list(
      difference = diff_formatter))
    check_spatial_encoding <- check_spatial_encoding %>% mutate(difference = difference %>% round(2))


}


  return(check_spatial_encoding)


}












