#' @title Check model for convergence
#'
#' @param mod model output
#'
#' @return One of 3 numeric arguments. 1: no convergence issues. 0: model is singular. -1: Another convergence problem
#' @export
#'
#' @examples \dontrun{
#' sim_mod %>% check_convergence()}
check_convergence <- function(mod) {
  if(is.null(unlist(mod@optinfo$conv$lme4))) {
    retval = 1 #returns one if the convergence info is an empty list
  }
  else {
    if (isSingular(mod)) {
      retval = 0 #returns 0 if the model was singular
    } else {
      retval = -1 #returns -1 if there was another convergence problem
    }
  }
  retval #returns the value (1, 0, or -1)
}
