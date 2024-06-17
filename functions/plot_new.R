rm(list=ls())
library(tidyverse)
library(patchwork)
library(scales)
# Common y-axis breaks and labels for both plots
y_breaks <- seq(0, 1, by = 0.25)
y_labels <- scales::percent(y_breaks)
# Model1 ------------------------------------------------------------------
load("data/df.rdata")

# Enhanced Plot 1 with annotations
p1 <- df %>%
  ggplot(aes(x = trial, y = p_harm)) +
  geom_rect(aes(xmin = 1, xmax = 1001, ymin = 0, ymax = 0.05, fill = "Calm"), alpha = 1) +
  geom_rect(aes(xmin = 1, xmax = 1001, ymin = 0.05, ymax = 1, fill = "Anxious"), alpha = 1) +
  geom_line(size = 1) +
  scale_fill_manual(values = c("Calm" = "deepskyblue", "Anxious" = "firebrick4")) +
  theme_classic() +
  labs(x = "Trial Number", y = "Probability of Harm", title = "Harm Probability Across Trials", fill = "State") +
  scale_y_continuous(breaks = y_breaks, labels = y_labels)   # Optional: to display y-axis labels as percentages
# Now p1 will display a legend for the color-coded regions.

# Enhanced Plot 2 with consistent aesthetics
p2 <- df %>%
  filter(state == 2) %>%
  group_by(action) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  ggplot(aes(x = factor(action), y = proportion)) +
  geom_bar(stat = "identity", fill = "deepskyblue") +
  theme_classic() +
  labs(x = "Action", y = "Proportion", title = "Action Proportion for State 2") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = scales::percent, limits = c(0, 1))   # Optional: to display y-axis labels as percentages

# Enhanced Plot 3 with consistent aesthetics
# Create a data frame with all possible actions (assuming action numbers 1 through 10)
all_actions <- data.frame(action = factor(1:10))

# Calculate the counts of actions where state == 1
action_counts_state1 <- df %>%
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
  geom_bar(stat = "identity", fill = "firebrick4") +
  theme_classic() +
  labs(x = "Action", y = "Proportion", title = "Action Proportion for State 1") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = scales::percent, limits = c(0, 1))    # Optional: to display y-axis labels as percentages

# Now p3 will display a bar for all actions, including those with zero counts.

# Combining plots with patchwork
combined_plot <- p1 + p2 + p3 + plot_layout(ncol = 1)
print(combined_plot)


# Model 2 - frequency catastrophe -----------------------------------------
load("data/df_freq.rdata")

# Enhanced Plot 1 with annotations
p1 <- df %>%
  ggplot(aes(x = trial, y = p_harm)) +
  geom_rect(aes(xmin = 1, xmax = 1001, ymin = 0, ymax = 0.05, fill = "Calm"), alpha = 1) +
  geom_rect(aes(xmin = 1, xmax = 1001, ymin = 0.05, ymax = 1, fill = "Anxious"), alpha = 1) +
  geom_line() +
  scale_fill_manual(values = c("Calm" = "deepskyblue", "Anxious" = "firebrick4")) +
  theme_classic() +
  labs(x = "Trial Number", y = "Probability of Harm", title = "Harm Probability Across Trials", fill = "State") +
  geom_text(data = data.frame(trial = c(0, seq(100, 1000, 100)), p_harm = rep(min(df$p_harm), length(c(0, seq(100, 1000, 100))))), aes(label = "X"), vjust = -1, fontface = "bold", size = 6)+
  scale_y_continuous(breaks = y_breaks, labels = y_labels)   # Optional: to display y-axis labels as percentages
# Now p1 will display a legend for the color-coded regions.

# Enhanced Plot 2 with consistent aesthetics
p2 <- df %>%
  filter(state == 2) %>%
  group_by(action) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  ggplot(aes(x = factor(action), y = proportion)) +
  geom_bar(stat = "identity", fill = "deepskyblue") +
  theme_classic() +
  labs(x = "Action", y = "Proportion", title = "Action Proportion for State 2") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = scales::percent, limits = c(0, 1))   # Optional: to display y-axis labels as percentages

# Enhanced Plot 3 with consistent aesthetics
# Create a data frame with all possible actions (assuming action numbers 1 through 10)
all_actions <- data.frame(action = factor(1:10))

# Calculate the counts of actions where state == 1
action_counts_state1 <- df %>%
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
  geom_bar(stat = "identity", fill = "firebrick4") +
  theme_classic() +
  labs(x = "Action", y = "Proportion", title = "Action Proportion for State 1") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = scales::percent, limits = c(0, 1))    # Optional: to display y-axis labels as percentages

# Now p3 will display a bar for all actions, including those with zero counts.

# Combining plots with patchwork
combined_plot <- p2 + p3 + plot_layout(ncol = 1)
print(combined_plot)

