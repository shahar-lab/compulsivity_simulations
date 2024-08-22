plot_by_period <- function(data) {
  
  # Load necessary libraries
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(patchwork)  # For arranging the plots
  
  # Define all actions
  all_actions <- data.frame(action = factor(1:10))
  
  # Helper function to generate plots for safe and dangerous states
  generate_state_plots <- function(period_data, all_actions) {
    
    # Plot for safe state (state == 2)
    p_safe <- period_data %>%
      filter(state == 2) %>%
      group_by(action) %>%
      summarise(count = n()) %>%
      mutate(proportion = count / sum(count)) %>%
      ggplot(aes(x = factor(action), y = proportion)) +
      geom_bar(stat = "identity", fill = "deepskyblue") +
      theme_classic() +
      labs(x = "Action", y = "Action proportion", title = "Safe state") +
      scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = scales::percent, limits = c(0, 1)) 
    
    # Plot for dangerous state (state == 1)
    action_counts_state1 <- period_data %>%
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
    
    # Plot for dangerous state (state == 1)
    p_danger <- ggplot(action_counts_state1, aes(x = action, y = proportion)) +
      geom_bar(stat = "identity", fill = "coral1") +
      theme_classic() +
      labs(x = "Action", y = "Action proportion", title = "Dangerous state") +
      scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = scales::percent, limits = c(0, 1))   
    
    return(list(p_safe = p_safe, p_danger = p_danger))
  }
  
  # Generate plots for each period (period 1, period 2, period 3)
  period1 <- data %>% filter(period == 1)
  period2 <- data %>% filter(period == 2)
  period3 <- data %>% filter(period == 3)
  
  # Get state and harm plots for each period
  period1_plots <- generate_state_plots(period1, all_actions)
  period2_plots <- generate_state_plots(period2, all_actions)
  period3_plots <- generate_state_plots(period3, all_actions)
  
  # Generate P(harm) plots for each period
  p1_harm <- period1 %>%
    ggplot(aes(x = timestep, y = p_harm)) +
    geom_rect(aes(xmin = 0, xmax = 200, ymin = 0, ymax = 0.05, fill = "Safe"), alpha = 1) +
    geom_rect(aes(xmin = 0, xmax = 200, ymin = 0.05, ymax = 1, fill = "Dangerous"), alpha = 1) +
    geom_line(color = "black", size = 1) +
    scale_fill_manual(values = c("Safe" = "deepskyblue", "Dangerous" = "coral1")) +
    theme_classic() +
    labs(x = "Time step", y = "P(harm)", fill = "State", title = "Before treatment")
  
  p2_harm <- period2 %>%
    ggplot(aes(x = timestep + 200, y = p_harm)) +
    geom_rect(aes(xmin = 201, xmax = 400, ymin = 0, ymax = 0.05, fill = "Safe"), alpha = 1) +
    geom_rect(aes(xmin = 201, xmax = 400, ymin = 0.05, ymax = 1, fill = "Dangerous"), alpha = 1) +
    geom_line(color = "black", size = 1) +
    scale_fill_manual(values = c("Safe" = "deepskyblue", "Dangerous" = "coral1")) +
    theme_classic() +
    labs(x = "Time step", y = "P(harm)", fill = "State", title = "During treatment")
  
  p3_harm <- period3 %>%
    ggplot(aes(x = timestep + 400, y = p_harm)) +
    geom_rect(aes(xmin = 401, xmax = 600, ymin = 0, ymax = 0.05, fill = "Safe"), alpha = 1) +
    geom_rect(aes(xmin = 401, xmax = 600, ymin = 0.05, ymax = 1, fill = "Dangerous"), alpha = 1) +
    geom_line(color = "black", size = 1) +
    scale_fill_manual(values = c("Safe" = "deepskyblue", "Dangerous" = "coral1")) +
    theme_classic() +
    labs(x = "Time step", y = "P(harm)", fill = "State", title = "After treatment")
  
  # Arrange the plots in 3 columns and 3 rows using patchwork
  combined_plot=gridExtra::grid.arrange(p1_harm,p2_harm,p3_harm,period1_plots$p_danger ,period2_plots$p_danger ,period3_plots$p_danger,
                                        period1_plots$p_safe , period2_plots$p_safe , period3_plots$p_safe,ncol=3)
  
  # Return the combined plot
  return(combined_plot)
}

