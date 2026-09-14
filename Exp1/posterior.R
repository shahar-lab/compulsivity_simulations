# Posterior estimates of average max(p(a)) (mode_share) for each of the
# 4 groups: controllability (Low/High) x state (Calm/Anxious). ------------

rm(list = ls())
library(tidyverse)
library(brms)
library(cmdstanr)
library(bayestestR)

load("Exp1/data/df_high_eta_all.rdata")
load("Exp1/data/df_low_eta_all.rdata")

state_labels = c("1" = "Anxious", "2" = "Calm")

df_high_eta_all$controllability = "High"
df_low_eta_all$controllability  = "Low"
df_all = bind_rows(df_high_eta_all, df_low_eta_all)

agent_mode_share = df_all %>%
  group_by(controllability, subject, state, action) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(controllability, subject, state) %>%
  summarise(mode_share = max(count) / sum(count), .groups = "drop") %>%
  mutate(
    state = factor(state, levels = c(2, 1), labels = state_labels[c("2", "1")]),
    controllability = factor(controllability, levels = c("Low", "High")),
    group = factor(paste(controllability, state, sep = "_"))
  )

# Fit: one coefficient per group = that group's posterior mean directly ---

model_group = brm(
  mode_share ~ 0 + group,
  data = agent_mode_share,
  backend = "cmdstanr",
  chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 1234
)
save(model_group, file = "Exp1/data/regression_group_mode_share.rdata")

describe_posterior(model_group)
