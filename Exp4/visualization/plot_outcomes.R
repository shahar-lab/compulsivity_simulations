rm(list = ls())
library(tidyverse)
library(brms)
library(cmdstanr)
load(file = "Exp4/data/4.rdata")

# Identify ritual action from period 1 (most frequent in dangerous state)
rituals_p1 <- df4 %>%
  filter(state == 1, period == 1) %>%
  group_by(subject, action) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(subject) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(subject, ritual_action = action)

# Dominant action in periods 2 and 3
dominant <- df4 %>%
  filter(state == 1, period %in% c(2, 3)) %>%
  group_by(subject, period, action) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(subject, period) %>%
  mutate(freq = n / sum(n)) %>%
  slice_max(freq, n = 1, with_ties = FALSE) %>%
  select(subject, period, dominant_action = action, dominant_freq = freq) %>%
  pivot_wider(names_from = period,
              values_from = c(dominant_action, dominant_freq),
              names_glue = "{.value}_p{period}")

# Classify outcomes
results <- dominant %>%
  left_join(rituals_p1, by = "subject") %>%
  ungroup() %>%
  mutate(
    successful_p2  = as.integer(dominant_action_p2 != ritual_action),
    relapse        = as.integer(successful_p2 == 1 & dominant_action_p3 == ritual_action),
    substitution   = as.integer(successful_p2 == 1 & dominant_action_p3 != ritual_action &
                                  dominant_freq_p3 > 0.5)
  )

fit_if_nonzero <- function(outcome, data) {
  if (sum(outcome) == 0 || sum(outcome) == length(outcome)) return(NULL)
  brm(
    y ~ 1,
    data = data.frame(y = outcome),
    family = bernoulli(link = "logit"),
    backend = "cmdstanr",
    chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 1234
  )
}

m_unsuccessful <- fit_if_nonzero(1 - results$successful_p2, results)
m_relapse      <- fit_if_nonzero(results$relapse,            results)
m_sub          <- fit_if_nonzero(results$substitution,       results)

save(m_unsuccessful, m_relapse, m_sub,
     file = "Exp4/data/outcome_models.rdata")

# Extract posterior means and 95% HDI on probability scale
extract_est <- function(model, label) {
  if (is.null(model)) return(NULL)
  draws <- plogis(as_draws_df(model) %>% pull(b_Intercept))
  hdi   <- bayestestR::hdi(draws, ci = 0.95)
  cat(label, "— mean:", round(mean(draws), 3),
      "| 95% HDI: [", round(hdi$CI_low, 3), ",", round(hdi$CI_high, 3), "]\n")
  tibble(category = label, mean = mean(draws),
         lower = hdi$CI_low, upper = hdi$CI_high)
}

lvls <- c("Unsuccessful\nTreatment", "Relapse", "Ritual\nSubstitution")

plot_data <- bind_rows(
  extract_est(m_unsuccessful, "Unsuccessful\nTreatment"),
  extract_est(m_relapse,      "Relapse"),
  extract_est(m_sub,          "Ritual\nSubstitution")
) %>% mutate(category = factor(category, levels = lvls))

if (nrow(plot_data) == 0) stop("No outcomes to plot.")

okabe_ito <- c(
  "Unsuccessful\nTreatment" = "#0072B2",
  "Relapse"                 = "#E69F00",
  "Ritual\nSubstitution"    = "#CC79A7"
)

ggplot(plot_data, aes(x = category, y = mean, fill = category)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  scale_fill_manual(values = okabe_ito) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Proportion of Agents") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(size = 11)
  )
