
rm(list = ls())
library(tidyverse)
library(brms)
library(cmdstanr)
load(file = "Exp3/data/ERP.rdata")
# Check treatment success and relapse -------------------------------------

#46 out of 100 agents have acquired the ritualistic action.
#Out of these 46, 28 have relapsed following treatment termination

period_results <- function(data, period_number) {
  results <- data %>%
    filter(period == period_number, state == 1) %>%  # Filter by period and state
    mutate(
      action_group = ifelse(action %in% 1:9, "1-9", "10")  # Classify actions into groups
    ) %>%
    group_by(subject, action_group, action) %>%  # Group by subject, action group, and individual action
    summarise(
      action_count = n(),  # Count occurrences of each action
      .groups = "drop"     # Drop grouping after summarise
    ) %>%
    group_by(subject, action_group) %>%
    mutate(
      total_actions = sum(action_count),  # Total actions per group
      relative_frequency = action_count / total_actions  # Relative frequency for each action within the group
    ) %>%
    ungroup()  # Remove all grouping
  
  return(results)
}

before_results=period_results(df5,1)
treatment_results=period_results(df5,2)
after_results=period_results(df5,3)

# Step 1: Add phase labels and combine dataframes
before_results <- before_results %>% mutate(phase = "before")
treatment_results <- treatment_results %>% mutate(phase = "during")
after_results <- after_results %>% mutate(phase = "after")

# Combine all dataframes
combined_data <- bind_rows(before_results, treatment_results, after_results)

# Step 2: Ensure the 'phase' column is a factor with the correct order
combined_data <- combined_data %>%
  mutate(phase = factor(phase, levels = c("before", "during", "after")))

# Step 3: Compute maximal relative frequency for each subject and phase
max_freq <- combined_data %>%
  group_by(subject, phase) %>%
  summarise(max_relative_frequency = max(relative_frequency), .groups = "drop")

treatment_cost=df5%>%group_by(subject)%>%summarise(treatment_cost=mean(treatment_cost))
exposure_intensity=df5%>%group_by(subject)%>%summarise(exposure_intensity=mean(exposure_intensity))

max_freq <- max_freq %>%
  left_join(treatment_cost, by = "subject") %>%
  left_join(exposure_intensity, by = "subject")

# Define the formula
formula <- bf(max_relative_frequency ~ phase * treatment_cost)

# Fit the model
fit <- brm(
  formula = formula,
  data = max_freq,
  family = gaussian(),  # Adjust the family if `max_relative_frequency` has a different distribution
  chains = 4,
  cores = 4,
  iter = 2000,
  backend="cmdstan"
)
# Step 4: Visualize the change
ggplot(max_freq, aes(x = phase, y = max_relative_frequency, group = subject)) +
  geom_line(aes(color = factor(subject)), alpha = 0.6) + # Line plot for each subject
  geom_point(size = 2) + # Add points for better visibility
  labs(
    title = "Change in Maximal Relative Frequency Across Phases",
    x = "Phase",
    y = "Maximal Relative Frequency",
    color = "Subject"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Check if treatment succeeded (waiting ritual acquired) ------------------

acquired=treatment_results%>%
  group_by(subject) %>%
  filter(relative_frequency == max(relative_frequency)) %>%
  select(subject, action, relative_frequency)%>%
  summarise(waiting=mean(action==10))

treatment_success=mean(acquired$waiting) # percent of agents who acquired the waiting action

acquired_subjects=acquired%>%filter(waiting==1)%>%pull(subject)

# Check if relapsed to previous ritual ------------------------------------

first_ritual=before_results%>%
  group_by(subject) %>%
  mutate(
    total_actions = sum(action_count),  # Calculate total actions for each subject
    relative_frequency = action_count / total_actions  # Compute the relative frequency
  ) %>%
  filter(relative_frequency == max(relative_frequency)) %>%
  select(subject, action, relative_frequency)


ritual=first_ritual%>%select(subject,action)
result = after_results %>%filter(subject%in%acquired_subjects)%>%
  inner_join(ritual%>%filter(subject%in%acquired_subjects), by = c("subject", "action" = "action"))  # Join based on subject and action

relapsed_subjects=result%>%pull(subject)

relapse_chance=length(relapsed_subjects)/length(acquired_subjects)
