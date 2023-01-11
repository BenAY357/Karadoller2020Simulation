#' @title Create violin plot for RL use
#'
#' @param df \code{tibble}
#'
#' @return A violin plor
#' @export
#'
#' @examples
#' sim_data <- sim_df()
#' RL_violin_plot(sim_data)
RL_violin_plot <- function(df){

  prop_RL_use <- df %>% group_by(subject, viewpoint, age, language) %>% summarise(prop_RL_use = mean(RL)) # get the proportion of RL use by participant

  prop_RL_use  <- prop_RL_use %>% mutate(age = dplyr::recode(age, "adult" = "adults", "child" = "children")) # recode age col

  # Reorder factor levels
  prop_RL_use$age <- factor(prop_RL_use$age, levels=c("children", "adults"))
  prop_RL_use$viewpoint <- factor(prop_RL_use$viewpoint, levels=c("independent", "dependent"))

  # Plot data

  ggplot(prop_RL_use, aes(x = language, y = prop_RL_use))+
    facet_wrap(vars(as.factor(age))) +
    geom_violin(aes(fill = factor(viewpoint), fill = factor(viewpoint))) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) + # set scale for y-axis
    scale_fill_manual(values=c("#fcd582", "#78d4f8")) + # add colour scheme (orange and blue)

    theme(panel.background = element_blank(), # remove background
          strip.background = element_rect(colour = "black",
                                          fill = "white"),
          # add axis lines back in
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +

    labs(x='Language Status', y = "Proportion of RL Use", fill = "viewpoint") # change labels
}
