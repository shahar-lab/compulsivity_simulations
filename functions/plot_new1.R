source('functions/all_plot.R')

# Model1 basic model------------------------------------------------------------------
load("data/df_advantage.rdata")
load("data/df_advantage_low_eta.rdata")
plot_action_proportions(df, include_p1 = TRUE)
# Model 2 - frequency catastrophe -----------------------------------------
load("data/df_example_freq.rdata")
plot_action_proportions(df, include_p1 = TRUE)
ggplot(most_frequent_actions, aes(x = frequency_cata, y = proportion)) +
  geom_point() + # Adds points to the plot
  geom_smooth( method="lm",se = T, color = "blue") + # Adds a linear regression line without confidence intervals
  labs(x = "Frequency_catastrophe", 
       y = "Proportion of Choosing Most Frequent Action") +
  theme_classic() # Uses a minimal theme for the plot
# Model 3 - cost action -----------------------------------------
load("data/df_cost.rdata")
plot_action_proportions(df, include_p1 = T)

ggplot(most_frequent_actions, aes(x = cost_action, y = proportion)) +
  geom_point() + # Adds points to the plot
  geom_smooth( method="lm",se = T, color = "blue") + # Adds a linear regression line without confidence intervals
  labs(x = "Cost of Most Frequent Action", 
       y = "Proportion of Choosing Most Frequent Action", 
       title = "Relationship between Cost of Action and Choice Proportion") +
  theme_classic() # Uses a minimal theme for the plot
# Model 4 - valence catastrophe -----------------------------------------
load("data/df_valence.rdata")
plot_action_proportions(df, include_p1 = T)
load("data/df_high_valence_with_cost.rdata")
plot_action_proportions(df, include_p1 = T)
ggplot(most_frequent_actions, aes(x = valence_cata, y = proportion)) +
  geom_point() + # Adds points to the plot
  geom_smooth( method="lm",se = T, color = "blue") + # Adds a linear regression line without confidence intervals
  labs(x = "Valence Catastrophe", 
       y = "Proportion of Choosing Most Frequent Action") +
  theme_classic() # Uses a minimal theme for the plot

# Model 5 - relaxation -----------------------------------------
load("data/df_value_based_relax.rdata")
plot_action_proportions(df, include_p1 = T)

# Model 6 - treatment -----------------------------------------
load("data/df_treatment.rdata")
plot_action_proportions(df, include_p1 = T)

# Model 7 - treatment_new -----------------------------------------
load("data/df_treatment_new.rdata")
plot_action_proportions(df, include_p1 = T)

# Example1 - omega1 -----------------------------------------
load("data/df_example_omega1.rdata")
plot_action_proportions(df, include_p1 = T)
p4 <- df %>%filter(trial<100)%>%
  ggplot(aes(x = trial, y = reward_function)) +
  geom_point() +
  theme_classic() +
  labs(x = "Trial Number", y = "Outcome") 
p4
