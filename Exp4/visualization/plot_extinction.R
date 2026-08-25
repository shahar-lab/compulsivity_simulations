# P(harm) across the full 3-period sequence, averaged within a condition ----
# Averages p_harm at each timestep across all subjects in a condition, then
# plots that mean trace across the whole Before/During/After sequence in one
# continuous timeline, so the trigger's weakening during treatment and the
# response after treatment are both visible.

plot_extinction_condition_mean <- function(df4, subject_ids, title = NULL) {

  library(ggplot2)
  library(dplyr)

  max_timestep <- max(df4$timestep)

  trace <- df4 %>%
    filter(subject %in% subject_ids) %>%
    group_by(period, timestep) %>%
    summarise(p_harm = mean(p_harm), .groups = "drop") %>%
    arrange(period, timestep) %>%
    mutate(
      global_timestep = (period - 1) * max_timestep + timestep
    )

  period_boundaries <- max_timestep * c(1, 2)
  period_midpoints  <- max_timestep * c(0.5, 1.5, 2.5)

  ggplot(trace, aes(x = global_timestep, y = p_harm)) +
    geom_rect(aes(xmin = -1, xmax = 3 * max_timestep + 1, ymin = 0, ymax = 0.05, fill = "Calm"), alpha = 1) +
    geom_rect(aes(xmin = -1, xmax = 3 * max_timestep + 1, ymin = 0.05, ymax = 1, fill = "Anxious"), alpha = 1) +
    geom_vline(xintercept = period_boundaries, linetype = "dashed", color = "grey30") +
    geom_line(color = "black", size = 0.6) +
    scale_fill_manual(values = c("Calm" = "deepskyblue", "Anxious" = "coral1")) +
    scale_x_continuous(
      breaks = period_midpoints,
      labels = c("Before treatment", "During treatment", "After treatment"),
      expand = c(0, 0)
    ) +
    theme_classic() +
    theme(axis.ticks.x = element_blank()) +
    labs(x = NULL, y = "Mean P(harm)", fill = "State", title = title)
}

# Panel of the 4 conditions sweeping exposure_intensity x treatment_cost ----
# One row per condition, showing the mean P(harm) sequence across that
# condition's subjects, to depict how each parameter shapes whether the
# trigger's threat is learned away during treatment.

plot_extinction_outcome_panel <- function(df4, subject_condition, condition_labels) {

  library(ggplot2)
  library(dplyr)
  library(patchwork)

  rows <- lapply(seq_along(condition_labels), function(i) {
    subject_ids <- subject_condition$subject[subject_condition$condition == i]
    plot_extinction_condition_mean(df4, subject_ids, title = condition_labels[i])
  })

  wrap_plots(rows, ncol = 1)
}

# Mean theta of the original ritual action, high vs low treatment_cost ------
# theta_ritual is the actor's learned value for each subject's period-1
# ritual action, logged every timestep from period 2 onward (see
# model/advantage_actor_critic.R). Averaged across subjects and collapsed
# across exposure_intensity, split by treatment_cost, to show whether a
# higher cost of performing the ritual during treatment suppresses its
# learned value more than a low cost does.

plot_ritual_metric_by_condition <- function(df4, subject_condition, condition_labels,
                                             metric_col, y_label, title, legend_title) {

  library(ggplot2)
  library(dplyr)

  max_timestep <- max(df4$timestep)

  trace <- df4 %>%
    filter(period %in% c(2, 3), !is.na(.data[[metric_col]])) %>%
    inner_join(subject_condition, by = "subject") %>%
    mutate(condition_group = condition_labels[condition]) %>%
    group_by(condition_group, period, timestep) %>%
    summarise(mean_metric = mean(.data[[metric_col]]), .groups = "drop") %>%
    mutate(global_timestep = (period - 2) * max_timestep + timestep)

  period_boundary <- max_timestep
  period_midpoints <- max_timestep * c(0.5, 1.5)

  ggplot(trace, aes(x = global_timestep, y = mean_metric, color = condition_group)) +
    geom_vline(xintercept = period_boundary, linetype = "dashed", color = "grey70") +
    geom_line(size = 0.7) +
    scale_color_manual(values = c("#D55E00", "#0072B2")) +
    scale_x_continuous(
      breaks = period_midpoints,
      labels = c("During treatment", "After treatment"),
      expand = c(0, 0)
    ) +
    theme_classic() +
    theme(axis.ticks.x = element_blank()) +
    labs(x = NULL, y = y_label, color = legend_title, title = title)
}

plot_theta_ritual_by_condition <- function(df4, subject_condition, condition_labels) {
  plot_ritual_metric_by_condition(
    df4, subject_condition, condition_labels,
    metric_col = "theta_ritual",
    y_label = "Mean theta (original ritual action)",
    title = "Learned value of the original ritual action",
    legend_title = "Condition"
  )
}

plot_p_ritual_by_condition <- function(df4, subject_condition, condition_labels) {
  plot_ritual_metric_by_condition(
    df4, subject_condition, condition_labels,
    metric_col = "p_ritual",
    y_label = "Mean P(choose ritual | anxious state)",
    title = "Selection probability of the original ritual action",
    legend_title = "Condition"
  )
}
