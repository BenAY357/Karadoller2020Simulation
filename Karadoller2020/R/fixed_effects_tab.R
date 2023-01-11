#' @title Fixed effect table
#' @description Table with the input model's fixed effect
#' @param mod \code{glmerMod}
#'
#' @return \code{formattable}
#' @export
#' @examples
#' \dontrun{
#' fixed_effects_tab(sim_mod)
#' }
fixed_effects_tab <- function(mod){

  sim_mod <- mod %>% tidy(effects = "fixed")
  sim_mod <- sim_mod %>% select(-effect) %>% # remove effect column
    # rename terms
    mutate(term = c("(Intercept)", "Age","Language","Viewpoint",
                    "Age:Language","Age:Viewpoint","Language:Viewpoint",
                    "Age:Languge:Viewpoint")) %>%
    mutate_at(vars(estimate:p.value), funs(round(., 3)))  # round values

  sim_mod <-  sim_mod %>% mutate(p.value = as.numeric(p.value)) %>%
    mutate(p.value = case_when( # add significance indicators.
      p.value < 0.001 ~ "0.000***",
      p.value < 0.01 ~ paste(p.value, "**", sep = ""),
      p.value < 0.05 ~ paste(p.value, "*", sep = ""),
      p.value > 0.05 ~ paste(p.value)

    )) %>% rename("SE" = "std.error", "z" = "statistic", "p value " = "p.value")  # rename columns

  formattable(sim_mod,
              align =c("l","c","c","c","l"),
              list(`term` = formatter(
                "span", style = ~ style(color = "black",font.weight = "bold"))
              ))
}
