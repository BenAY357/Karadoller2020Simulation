#' @title Simulate the Correct Spatial Encoding Data
#' @description Applies "sim_spatial_encoding_single()" to all the rows in group_params and then combines them into one data frame.
#'
#' @param group_params data frame input. group_params can be accessed using the "get_group_params()" function
#' @param trial_params data frame input. trial_params can be accessed using the "get_trial_params()" function
#' @param age_ref string input. "age_ref = child" by default. Picks the reference category.
#' @param lang_ref string input. "lang_ref = native" by default. Picks the reference category.
#' @param viewpoint_ref string input. "viewpoint_ref = independent" by default. Picks the reference category.
#'
#' @return Returns a data frame containing a simulation of the correct spatial encoding data
#' @export
#' @examples
#' \dontrun{sim_spatial_encoding(group_params, trial_params)}
sim_spatial_encoding <- function(group_params, trial_params,
                                 age_ref = "child",
                                 lang_ref = "late",
                                 viewpoint_ref = "independent"
                                ){

  data_by_group <- c() # list which contains the simulated data frames for each of the groups

  shift_subject <- 0 # need to shift subject up every time "sim_spatial_encoding_single()" is called. E.g. this prevents subject 1-9 from being used in all 4 groups.

  for (i in 1:nrow(group_params)){# iterate over every row in group_params

    output <- sim_spatial_encoding_single(language = group_params[i,1],
                                          age = group_params[i,2],
                                          n_participants =  group_params[i,3],
                                          p_correct = group_params[i,4],
                                          n_trials = trial_params[1,2],
                                          n_view_dep = trial_params[2,2],
                                          n_view_indep = trial_params[3,2]
    )

    output <- output %>% mutate(subject = subject + shift_subject) # shift subject up
    shift_subject <- max(output$subject) # get new number to shift subject up by
    data_by_group[[length(data_by_group) + 1]] <- output # append output to list

  }

  combined_df <- do.call(rbind, data_by_group) # combines all outputs in list into one complete data frame.

    combined_df <- combined_df %>% mutate(language_dev = ifelse(language == lang_ref, -0.5, 0.5),
                            age_dev = ifelse(age == age_ref,-0.5,0.5),
                            viewpoint_dev = ifelse(viewpoint == viewpoint_ref, -0.5, 0.5))



  return(combined_df)

}
