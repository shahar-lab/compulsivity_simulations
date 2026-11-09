rm(list = ls())
library(tidyverse)
library(brms)

# Models (and their action-level data) are fitted in Exp3/posterior.R

# Sample 3A: ritual frequency vs. repetition cost (pr) --------------------

load(file = "Exp3/data/regression_cost_ritual.rdata")

ggplot(model_cost$data, aes(x = pr, y = ritual_frequency)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "#0072B2") +
  labs(x = "Repetition cost (pr)",
       y = "Ritual frequency") +
  theme_bw()

c_eff_cost <- conditional_effects(model_cost)
plot(c_eff_cost, plot = FALSE)[[1]] + theme_bw()

# Sample 3B: ritual frequency vs. action associability (beta) -------------

load(file = "Exp3/data/regression_beta_ritual.rdata")

ggplot(model_beta$data, aes(x = beta, y = ritual_frequency)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "#0072B2") +
  labs(x = "Action associability (β)",
       y = "Ritual frequency") +
  theme_bw()

c_eff_beta <- conditional_effects(model_beta)
plot(c_eff_beta, plot = FALSE)[[1]] + theme_bw()
