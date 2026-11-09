rm(list = ls())
library(tidyverse)
library(brms)

# Models (and their action-level data) are fitted in Exp3/posterior.R

# Sample 3A: ritual frequency vs. repetition cost (pr) --------------------

load(file = "Exp3/data/regression_cost_ritual.rdata")

p_cost <- ggplot(model_cost$data, aes(x = pr, y = ritual_frequency)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "#0072B2") +
  labs(x = "Repetition cost (pr)",
       y = "Ritual frequency") +
  theme_bw()
print(p_cost)
ggsave("Exp3/ritual_vs_cost.png", p_cost, width = 4.5, height = 4, dpi = 150)

c_eff_cost <- conditional_effects(model_cost)
p_ce_cost <- plot(c_eff_cost, plot = FALSE)[[1]] + theme_bw()
print(p_ce_cost)
ggsave("Exp3/ritual_vs_cost_effects.png", p_ce_cost, width = 4.5, height = 4, dpi = 150)

# Sample 3B: ritual frequency vs. action associability (beta) -------------

load(file = "Exp3/data/regression_beta_ritual.rdata")

p_beta <- ggplot(model_beta$data, aes(x = beta, y = ritual_frequency)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "#0072B2") +
  labs(x = "Action associability (β)",
       y = "Ritual frequency") +
  theme_bw()
print(p_beta)
ggsave("Exp3/ritual_vs_beta.png", p_beta, width = 4.5, height = 4, dpi = 150)

c_eff_beta <- conditional_effects(model_beta)
p_ce_beta <- plot(c_eff_beta, plot = FALSE)[[1]] + theme_bw()
print(p_ce_beta)
ggsave("Exp3/ritual_vs_beta_effects.png", p_ce_beta, width = 4.5, height = 4, dpi = 150)
