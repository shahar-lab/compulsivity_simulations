rm(list = ls())
library(tidyverse)
library(brms)
library(cmdstanr)
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
action_results <- action_freq %>%
  group_by(action) %>%
  summarise(ritual_frequency = mean(ritual_frequency), .groups = "drop") %>%
  left_join(action_pr, by = "action")

# Plot
ggplot(action_results, aes(x = pr, y = ritual_frequency)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "#0072B2") +
  labs(x = "Repetition cost (pr)",
       y = "Ritual frequency") +
  theme_bw()

# Bayesian regression
model <- brm(
  ritual_frequency ~ pr,
  data = action_results,
  backend = "cmdstanr",
  chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 1234
)
save(model, file = "Exp3/data/regression_cost_ritual.rdata")
c_eff <- conditional_effects(model)
plot(c_eff, plot = FALSE)[[1]] + theme_bw()
