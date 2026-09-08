rm(list = ls())
library(tidyverse)
library(brms)
library(cmdstanr)

# Sample 3A: ritual frequency vs. repetition cost (pr) --------------------

load(file = "Exp3/data/3A.rdata")

# For each action, get its pr (shared across subjects)
action_pr <- df3A %>%
  filter(state == 1) %>%
  group_by(action) %>%
  summarise(pr = mean(pr), .groups = "drop")

# For each subject, compute ritual frequency of each action in state=1
action_freq <- df3A %>%
  filter(state == 1) %>%
  group_by(subject, action) %>%
  summarise(action_count = n(), .groups = "drop") %>%
  group_by(subject) %>%
  mutate(ritual_frequency = action_count / sum(action_count)) %>%
  ungroup()

# Average ritual frequency per action across subjects
action_results_cost <- action_freq %>%
  group_by(action) %>%
  summarise(ritual_frequency = mean(ritual_frequency), .groups = "drop") %>%
  left_join(action_pr, by = "action")

# Plot
ggplot(action_results_cost, aes(x = pr, y = ritual_frequency)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "#0072B2") +
  labs(x = "Repetition cost (pr)",
       y = "Ritual frequency") +
  theme_bw()

# Bayesian regression
model_cost <- brm(
  ritual_frequency ~ pr,
  data = action_results_cost,
  backend = "cmdstanr",
  chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 1234
)
save(model_cost, file = "Exp3/data/regression_cost_ritual.rdata")
c_eff_cost <- conditional_effects(model_cost)
plot(c_eff_cost, plot = FALSE)[[1]] + theme_bw()

# Sample 3B: ritual frequency vs. self-control (beta) ---------------------

load(file = "Exp3/data/3B.rdata")

# For each action, get its beta (shared across subjects)
action_beta <- df3B %>%
  filter(state == 1) %>%
  group_by(action) %>%
  summarise(beta = mean(beta), .groups = "drop")

# For each subject, compute ritual frequency of each action in state=1
action_freq <- df3B %>%
  filter(state == 1) %>%
  group_by(subject, action) %>%
  summarise(action_count = n(), .groups = "drop") %>%
  group_by(subject) %>%
  mutate(ritual_frequency = action_count / sum(action_count)) %>%
  ungroup()

# Average ritual frequency per action across subjects
action_results_beta <- action_freq %>%
  group_by(action) %>%
  summarise(ritual_frequency = mean(ritual_frequency), .groups = "drop") %>%
  left_join(action_beta, by = "action")

# Plot
ggplot(action_results_beta, aes(x = beta, y = ritual_frequency)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "#0072B2") +
  labs(x = "Self-control parameter (β)",
       y = "Ritual frequency") +
  theme_bw()

# Bayesian regression
model_beta <- brm(
  ritual_frequency ~ beta,
  data = action_results_beta,
  backend = "cmdstanr",
  chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 1234
)
save(model_beta, file = "Exp3/data/regression_beta_ritual.rdata")
c_eff_beta <- conditional_effects(model_beta)
plot(c_eff_beta, plot = FALSE)[[1]] + theme_bw()
