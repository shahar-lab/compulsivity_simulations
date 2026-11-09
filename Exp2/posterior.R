# Posterior estimates of the effect of each environmental manipulation
# (v_harm, freq_c) on ritual frequency (max action frequency), by state.

rm(list = ls())
library(tidyverse)
library(brms)
library(cmdstanr)
library(bayestestR)

# calculate repetitions by manipulation by state --------------------------

compare_repetitions = function(df_list){
results <- data.frame()

for (i in 1:length(df_list)) {
  df <- df_list[[i]]
  df_name <- names(df_list)[i]
  max_action_freq_df <- df %>%
    group_by(subject, state, action) %>%
    summarise(action_count = n(), .groups = 'drop') %>%
    group_by(subject, state) %>%
    mutate(total_actions = sum(action_count),
           frequency = action_count / total_actions) %>%
    summarise(max_frequency = max(frequency),
              action_number = action[which.max(frequency)],
              .groups = 'drop')

  # Add subject-specific parameters
  max_action_freq_df <- max_action_freq_df %>%
    left_join(df %>%
                select(subject, v_harm, pr, freq_c) %>%
                distinct(),
              by = "subject") %>%
    mutate(manipulation = df_name)

  results <- bind_rows(results, max_action_freq_df)

}
results$state=factor(results$state)
results <- results %>%
  mutate(state = recode(state, '1' = 'Dangerous', '2' = 'Safe'))
save(results,file="Exp2/data/results.rdata")
results
}

# Run --------------------------------------------------------------------

load(file = "Exp2/data/high_freq_c.rdata")
load(file = "Exp2/data/low_v_harm.rdata")
load(file = "Exp2/data/baseline.rdata")

df_list <- list(
  "Baseline" = df1,
  "Low V(harm)" = df2,
  "High freq(c)" = df3)

results <- compare_repetitions(df_list)

# Fit: ritual frequency ~ state x manipulated parameter, per manipulation --

regression <- brm(
  max_frequency ~ state * v_harm,
  data = results %>% filter(manipulation == "Low V(harm)"),
  backend = "cmdstanr",
  chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 1234
)
save(regression, file = "Exp2/data/regression_v_harm.rdata")
describe_posterior(regression)

regression <- brm(
  max_frequency ~ state * freq_c,
  data = results %>% filter(manipulation == "High freq(c)"),
  backend = "cmdstanr",
  chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 1234
)
save(regression, file = "Exp2/data/regression_freq_c.rdata")
describe_posterior(regression)
