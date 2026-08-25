
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
    theme_bw()+scale_fill_manual(values = c("Anxious" = "coral1", "Calm" = "deepskyblue")) +
    scale_color_manual(values = c("Anxious" = "coral1", "Calm" = "deepskyblue"))
  

  if (!is.null(path_save)) {
    ggsave(filename = path_save, plot = plot_ce, device = Cairo::CairoSVG, width = 3.5, height = 2)
  } else {
    print(plot_ce)
  }
}


