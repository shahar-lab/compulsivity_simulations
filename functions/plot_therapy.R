library(dplyr)
library(ggplot2)
library(gridExtra)
library(patchwork)
# Function to calculate proportions for a given trial range
calculate_proportions <- function(df, trial_min, trial_max) {
  # Define all actions
  all_actions <- data.frame(action = factor(1:10))
  action_counts <- df %>%
    filter(state == 1, trial >= trial_min, trial < trial_max) %>%
    group_by(action) %>%
    summarise(count = n(), .groups = 'drop') %>%
    # Ensure action is a factor with levels for all possible actions
    mutate(action = factor(action, levels = levels(all_actions$action)))
  
  # Ensure all actions are represented, including those with zero counts
  action_counts <- full_join(all_actions, action_counts, by = "action") %>%
    # Replace NA with zero where count is missing
    replace_na(list(count = 0))
  
  # Calculate the total number of actions
  total_actions <- sum(action_counts$count)
  
  # Calculate the proportion for each action
  action_counts %>%
    mutate(proportion = count / total_actions)
}

# Now let's use this function for each trial range and create plots
p1 <- ggplot(calculate_proportions(df, 1, 1000), aes(x = action, y = proportion,fill = ifelse(action == "7", "Ritual", "Other"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Ritual" = "coral1", "Other" = "palegreen4")) +
  theme_classic() +
  ylim(0,1)+
  labs(x = "Action", y = "Action proportion")+
  theme(legend.title = element_blank())

p2 <- ggplot(calculate_proportions(df, 1000, 2000), aes(x = action, y = proportion,fill = ifelse(action == "7", "Ritual", "Other"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Ritual" = "coral1", "Other" = "palegreen4")) +
  theme_classic() +
  ylim(0,1)+
  labs(x = "Action", y = "Action proportion")+
  theme(legend.title = element_blank())

p3 <- ggplot(calculate_proportions(df, 2000, 3000), aes(x = action, y = proportion,fill = ifelse(action == "7", "Ritual", "Other"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Ritual" = "coral1", "Other" = "palegreen4")) +
  theme_classic() +
  ylim(0,1)+
  labs(x = "Action", y = "Action proportion")+
  theme(legend.title = element_blank())

gridExtra::grid.arrange(p1,p2, p3,ncol=3)
