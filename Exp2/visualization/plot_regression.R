plot_regression=function(path,path_save){
load(path)
conditional_effects_data <- conditional_effects(regression)
conditional_effects_data=conditional_effects_data$`state:manipulation`

custom_colors <- c('Dangerous' = 'coral1', 'Safe' = 'deepskyblue')
# Set the order of the manipulation factor
conditional_effects_data$manipulation <- factor(conditional_effects_data$manipulation,levels = c("Baseline", "Low V(harm)", "High freq(c)", "High cost"))

# Custom plot with ggplot2
plot=ggplot(conditional_effects_data, aes(x = manipulation, y = estimate__,color=state)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~state) +
  scale_color_manual(values = custom_colors)+
  theme_minimal() +
  labs(x = "Manipulation", y = "P(ritualistic behavior)", color = "State")
plot
ggsave(filename = path_save, plot = plot, width = 799 / 72, height = 307 / 72)
}
