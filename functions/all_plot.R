plot_action_proportions <- function(data, include_p1 = TRUE) {
  
  # Load necessary libraries
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(patchwork)
  
  # Define all actions
  all_actions <- data.frame(action = factor(1:10))
  
  # Plot for p1
  
  p1 <- data %>%
    ggplot(aes(x = trial, y = p_harm)) +
    geom_rect(aes(xmin = -1, xmax = 1001, ymin = 0, ymax = 0.05, fill = "Safe"), alpha = 1) +
    geom_rect(aes(xmin = -1, xmax = 1001, ymin = 0.05, ymax = 1, fill = "Dangerous"), alpha = 1) +
    geom_line(color = "black", size = 1) +
    scale_fill_manual(values = c("Safe" = "deepskyblue", "Dangerous" = "coral1")) +
    theme_classic() +
    scale_x_continuous(breaks = seq(0, 1000, by = 100))+
    labs(x = "Time step", y = "P(harm)", fill = "State") 
  
  # Plot for p2
  p2 <- data %>%
    filter(state == 2) %>%
    group_by(action) %>%
    summarise(count = n()) %>%
    mutate(proportion = count / sum(count)) %>%
    ggplot(aes(x = factor(action), y = proportion)) +
    geom_bar(stat = "identity", fill = "deepskyblue") +
    theme_classic() +
    labs(x = "Action", y = "Action proportion", title = "Calm state") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = scales::percent, limits = c(0, 1))   # Optional: to display y-axis labels as percentages
  
  # Enhanced Plot 3 with consistent aesthetics
  # Create a data frame with all possible actions (assuming action numbers 1 through 10)
  all_actions <- data.frame(action = factor(1:10))
  
  # Calculate the counts of actions where state == 1
  action_counts_state1 <- data %>%
    filter(state == 1) %>%
    group_by(action) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    # Ensure action is a factor with levels for all possible actions
    mutate(action = factor(action, levels = levels(all_actions$action)))
  
  # Ensure all actions are represented, including those with zero counts
  action_counts_state1 <- full_join(all_actions, action_counts_state1, by = "action") %>%
    # Replace NA with zero where count is missing
    replace_na(list(count = 0))
  
  # Calculate the total number of actions in state == 1
  total_actions_state1 <- sum(action_counts_state1$count)
  
  # Calculate the proportion for each action
  action_counts_state1 <- action_counts_state1 %>%
    mutate(proportion = count / total_actions_state1)
  
  # Create the plot for p3
  p3 <- ggplot(action_counts_state1, aes(x = action, y = proportion)) +
    geom_bar(stat = "identity", fill = "coral1") +
    theme_classic() +
    labs(x = "Action", y = "Action proportion", title = "Anxious state") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = scales::percent, limits = c(0, 1))    # Optional: to display y-axis labels as percentages
  
  # Print the plots
  if(include_p1){
    combined_plot=gridExtra::grid.arrange(p1,p2, p3)
  }
  else{
    combined_plot=gridExtra::grid.arrange(p2, p3)  
  }
  
}
