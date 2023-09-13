#plots

df %>% 
  ggplot(aes(x = 1:nrow(df), y = action)) +
  labs(x = "Trial", y = "Anxiety level") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 20),
        axis.title.y = element_text(margin = margin(r = 10), size = 20)) +
  theme(axis.title = element_text(size = 20, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(color = "black", size = 20, face = "bold")) +
  theme(axis.text = element_text(color = "black", size = 16),
        axis.text.x = element_text(face = "italic")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 16)) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14, color = "black")) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 50), fill = "lightgray", alpha = 0.5) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 50, ymax = Inf), fill = "darkgray", alpha = 0.5) +
  geom_line(aes(x = 1:nrow(df), y = anxiety[1:nrow(df)]), color = "steelblue", size = 1.5) +
  geom_hline(yintercept = cfg$cutoff_anxiety, linetype = "dashed", color = "darkorange", size = 1.5) +
  scale_x_continuous(breaks = seq(0, nrow(df), 100)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0, 100)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black", size = 0.5))

ggplot(df, aes(x = trial, y = action)) +
  geom_point(aes(color = repetition), size = 2) +
  labs(x = bquote(bold("Trial")), y = bquote(bold(""))) +
  scale_color_manual(values = c("#87CEFA","#FF6B6B"),
                     labels = c("No", "Yes"),
                     name = bquote(bold("Repetition"))) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  scale_y_continuous(breaks = 1:5, limits = c(1, 5)) +
  theme_minimal() +
  theme(axis.title = element_text(margin = margin(r = 10),size = 20, color = "black", face = "bold"),
        axis.text = element_text(size = 16, color = "black"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 16)) +
  guides(color = guide_legend(override.aes = list(size = 4)))+labs(x = "Trial", y = "Action") +
           theme(axis.title.x = element_text(margin = margin(t = 10), size = 20),
                 axis.title.y = element_text(margin = margin(r = 10), size = 20)) +
           theme(axis.title = element_text(size = 20, color = "black", face = "bold")) +
           theme(axis.title.y = element_text(color = "black", size = 20, face = "bold")) +
           theme(axis.text = element_text(color = "black", size = 16),
                 axis.text.x = element_text(face = "italic")) +
           theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 16))
