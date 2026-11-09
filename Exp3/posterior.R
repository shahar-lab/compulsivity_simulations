# Posterior estimates of the association between per-action parameters and
# ritual frequency: repetition cost (pr, Sample 3A) and action associability (beta, Sample 3B).

rm(list = ls())
library(tidyverse)
library(brms)
library(cmdstanr)
library(bayestestR)

# Sample 3A: ritual frequency vs. repetition cost (pr) --------------------

load(file = "Exp3/data/3A.rdata")

# For each action, get its pr (shared across subjects and states)
action_pr <- df3A %>%
  group_by(action) %>%
  summarise(pr = mean(pr), .groups = "drop")

# For each subject, compute ritual frequency of each action in state=1
# (actions a subject never chose in state=1 count as 0)
action_freq <- df3A %>%
  filter(state == 1) %>%
  group_by(subject, action) %>%
  summarise(action_count = n(), .groups = "drop") %>%
  complete(subject, action = 1:10, fill = list(action_count = 0)) %>%
  group_by(subject) %>%
  mutate(ritual_frequency = action_count / sum(action_count)) %>%
  ungroup()

# Average ritual frequency per action across subjects
action_results_cost <- action_freq %>%
  group_by(action) %>%
  summarise(ritual_frequency = mean(ritual_frequency), .groups = "drop") %>%
  left_join(action_pr, by = "action")

model_cost <- brm(
  ritual_frequency ~ pr,
  data = action_results_cost,
  backend = "cmdstanr",
  chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 1234
)
save(model_cost, file = "Exp3/data/regression_cost_ritual.rdata")

describe_posterior(model_cost)

# Sample 3B: ritual frequency vs. action associability (beta) -------------

load(file = "Exp3/data/3B.rdata")

# For each action, get its beta (shared across subjects and states)
action_beta <- df3B %>%
  group_by(action) %>%
  summarise(beta = mean(beta), .groups = "drop")

# For each subject, compute ritual frequency of each action in state=1
# (actions a subject never chose in state=1 count as 0)
action_freq <- df3B %>%
  filter(state == 1) %>%
  group_by(subject, action) %>%
  summarise(action_count = n(), .groups = "drop") %>%
  complete(subject, action = 1:10, fill = list(action_count = 0)) %>%
  group_by(subject) %>%
  mutate(ritual_frequency = action_count / sum(action_count)) %>%
  ungroup()

# Average ritual frequency per action across subjects
action_results_beta <- action_freq %>%
  group_by(action) %>%
  summarise(ritual_frequency = mean(ritual_frequency), .groups = "drop") %>%
  left_join(action_beta, by = "action")

model_beta <- brm(
  ritual_frequency ~ beta,
  data = action_results_beta,
  backend = "cmdstanr",
  chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 1234
)
save(model_beta, file = "Exp3/data/regression_beta_ritual.rdata")

describe_posterior(model_beta)
