

#' @title Simulate Spatial Encodings for a Single Group
#' @description Simulates the spatial encoding for a single group (e.g. native children, late adults). spatial_encoding: 1 = correct, 0 = incorrect
#'
#' @param n_participants Number of participants in the group.
#' @param p_correct Probability the participant encodes the correct spatial relation between the figure and ground item.
#' @param language Language status. "native" or "late".
#' @param age Whether the participants is a child or adult. "adult" or "child.
#' @param n_trials Number of trials
#' @param n_view_dep Number of view dependent trials.
#' @param n_view_indep Number of view independent trials.
#' @return \code{tibble} containing the simulated correct spatial encodings for one group.
#' @export
#'
#' @examples
#' # Simulates spatial encodings for native adults.
#' sim_spatial_encoding_single(15,.98,"native","adult",15,9,6)
sim_spatial_encoding_single <- function(n_participants,
                                        p_correct,
                                        language,
                                        age,
                                        n_trials,
                                        n_view_dep,
                                        n_view_indep){

  viewpoint <- c(rep("dependent", n_view_dep), rep("independent",n_view_indep)) # list with 9 view dependents then 6 view independents

  # convert parameters to correct input type. Needed because the parameters in "sim_spatial_encoding" will be type "tibble"

  n_participants <- as.numeric(n_participants)
  p_correct <- as.numeric(p_correct)
  language <- as.character(language)
  age <- as.character(age)
  n_trials <- as.numeric(n_trials)
  n_view_dep <- as.numeric(n_view_dep)
  n_view_indep <- as.numeric(n_view_indep)

  # create dataframe

  data <- tibble(subject = rep(1:n_participants, each = n_trials),
                 language = language,
                 age = age,
                 viewpoint = rep(viewpoint, n_participants),
                 spatial_encoding = rbinom(n = n_participants * n_trials,size = 1, prob = p_correct))

  return(data)
}
