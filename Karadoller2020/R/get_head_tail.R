#' @title Get head and tail
#' @description combined head() and tail()
#' @param df \code{tibble}
#' @param n_head numeric argument.
#' @param n_tail numeric argument.
#'
#' @return \code{tibble}. First and last N rows of the input data frame.
#' @export
#'
#' @examples
#' # get the first two and last row of my_df
#' my_df <- tibble(x = c(1,2,3,4),
#'  y = c("a", "b","c","d"))
#' my_df %>% get_head_tail(n_head = 2, n_tail = 1)
get_head_tail <- function(df, n_head, n_tail){

  first_rows <- df %>% head(n_head)
  last_rows <- df %>% tail(n_tail)

  first_last <- rbind(first_rows,last_rows)

  return(first_last)

}
