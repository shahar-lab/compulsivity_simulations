rm(list = ls())
library(tidyverse)
library(brms)
library(cmdstanr)

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
}

# plot conditional effects of a regression model --------------------------

plot_regression <- function(path, data, path_save = NULL) {
  # Load the regression model from the specified path
  load(path)

  # Generate conditional effects
  conditional_effects_data <- conditional_effects(regression)
  effect_name <- names(conditional_effects_data)[3]
  ce_df <- conditional_effects_data[[effect_name]]

  # Extract the predictor variable name dynamically
  predictor_var <- strsplit(effect_name, ":")[[1]][1]
  # Create the customized plot

  plot_ce=ggplot() +
    # Add the regression line and confidence intervals
    geom_ribbon(data = ce_df, aes_string(x = predictor_var, ymin = "lower__", ymax = "upper__", fill = "state"), alpha = 0.2) +
    geom_line(data = ce_df, aes_string(x = predictor_var, y = "estimate__", color = "state"), size = 1) +
    # Overlay raw data points (correct dynamic referencing of column)
    geom_point(data = data, aes_string(x = predictor_var, y = "max_frequency", color = "state"), alpha = 0.5) +
    # Customize labels and theme
    labs(x = predictor_var, y = "Ritual Frequency", color = "State", fill = "State") +
    theme_bw()+scale_fill_manual(values = c("Dangerous" = "coral1", "Safe" = "deepskyblue")) +
    scale_color_manual(values = c("Dangerous" = "coral1", "Safe" = "deepskyblue"))


  if (!is.null(path_save)) {
    ggsave(filename = path_save, plot = plot_ce, device = Cairo::CairoSVG, width = 3.5, height = 2)
  } else {
    print(plot_ce)
  }
}

# Run --------------------------------------------------------------------

load(file = "Exp2/data/high_freq_c.rdata")
load(file = "Exp2/data/low_v_harm.rdata")
load(file = "Exp2/data/baseline.rdata")

load(file = "Exp2/data/results.rdata")

df_list <- list(
  "Baseline" = df1,
  "Low V(harm)" = df2,
  "High freq(c)" = df3)

compare_repetitions(df_list)
plot_regression("Exp2/data/regression_v_harm.rdata", results %>% filter(manipulation == "Low V(harm)"))
plot_regression("Exp2/data/regression_freq_c.rdata", results %>% filter(manipulation == "High freq(c)"))
