rm(list=ls())
library(ggplot2)
library(gridExtra) # for arranging plots
library(dplyr)
library(tidyr)
library(reshape2)

#load data (should also load cfg from compulsive_choice file)
load("data/df_freq.rdata")

#calculate when state shifts had happened
block_change=df%>%mutate(block_change = state != lag(state, default = first(state)))%>%pull(block_change)
block_shifts=data.frame(shifts=which(block_change))

#plot the harm expectancy levels through time with black dashed lines indicating the state shifts
harm_expectancy_plot=df %>% 
  ggplot(aes(x = 1:nrow(df))) +
  labs(x = "Time step", y = "Harm Expectancy Level") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 16),
        axis.title.y = element_text(margin = margin(r = 10), size = 16)) +
  theme(axis.title = element_text(size = 20, color = "black", face = "bold")) +
  theme(axis.title.y = element_text(color = "black", size = 16, face = "bold")) +
  theme(axis.text = element_text(color = "black", size = 14),
        axis.text.x = element_text(face = "italic")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 14)) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14, color = "black")) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = cfg$cutoff_harm_exp), fill = "lightgray", alpha = 0.5) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = Inf, ymax = cfg$cutoff_harm_exp), fill = "darkgray", alpha = 0.5) +
  geom_line(aes(x = 1:nrow(df), y = harm_exp[1:nrow(df)]), color = "steelblue", size = 1.5) +
  geom_hline(yintercept = cfg$cutoff_harm_exp, linetype = "dashed", color = "darkorange", size = 1.5) +
  scale_x_continuous(breaks = seq(0, nrow(df), 100)) +
  scale_y_continuous(breaks = c(-100,-75,-50,-25, 0), limits = c(-100, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black", size = 0.5))+geom_vline(data = block_shifts, aes(xintercept = shifts),size=1, color = "black", linetype = "dashed")
print(harm_expectancy_plot)


# Calculate for each subject and block the action counts and their percentages
action_counts <- df %>%
  group_by(subject) %>%
  mutate(block_change = state != lag(state, default = state[1]),
         block = cumsum(block_change)) %>%
  group_by(subject, block) %>%
  # Add a column for the total number of actions in the block
  mutate(total_actions_per_block = n()) %>%
  group_by(subject, block, action, total_actions_per_block) %>%
  # Calculate the percentage of each action and then divide by total to get the percentage
  summarize(action_count = n(), .groups = 'drop') %>%
  # Now calculate the percentage
  mutate(percentage = (action_count / total_actions_per_block) * 100) %>%
  ungroup()

# define block_widths to be based on state shifts
block_widths <- df %>%
  group_by(subject) %>% # Group by subject first
  mutate(block_change = state != lag(state, default = state[1]),
         block = cumsum(block_change)) %>%
  group_by(subject, block) %>%
  summarize(trials_in_block = n(), .groups = 'drop') %>%
  ungroup()

#change from 0 to 1
action_counts$block=action_counts$block+1
block_widths$block = block_widths$block+1

# Merge the block widths back into the action_counts dataframe
action_counts <- action_counts %>%
  left_join(block_widths, by = c("subject", "block"))

# Convert to wide format
action_counts$action <- factor(action_counts$action, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'))

# Convert actions to a factor if not already
action_counts$w = action_counts$trials_in_block / 1000 * 24

#convert actions to include actions that were not chosen in that block
complete_actions = expand(action_counts, subject, block, action = 1:10)
complete_actions$action=factor(complete_actions$action)
full_df <- complete_actions %>%
  left_join(action_counts, by = c("subject", "block", "action")) %>%
  replace_na(list(action_count = 0, percentage = 0)) %>%
  group_by(subject, block) %>%
  fill(everything(), .direction = "downup") %>%
  ungroup()

# Plot the heatmap with facet_wrap
heatmap_plot <- ggplot(full_df, aes(x = block, y = action, fill = percentage)) +
  geom_tile(aes(width = w), linewidth = 0.5, color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "State shift", y = "Action", fill = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ subject)

# Print the plot
print(heatmap_plot)



# OLD catasrophe_plot
# catastrophe_data <- df %>%
#   filter(catastrophe == 1) %>%
#   select(trial,action)
# 
# catastrophe_plot <- ggplot(catastrophe_data, aes(x = trial, y = factor(action))) +
#   geom_point(shape = 4, size = 2, color = "red") +  # Red X markers
#   labs(x = "Trial", y = "Action") +
#   theme_minimal() +
#   theme(axis.title.y = element_text(size = 12), 
#         axis.text.y = element_text(size = 10),
#         axis.ticks.y = element_line(),
#         panel.grid.major.y = element_line(),
#         panel.grid.minor.y = element_line()) +
#   geom_vline(data = block_shifts, aes(xintercept = shifts), size = 1, color = "white") +
#   scale_x_continuous(breaks = seq(from = 0, to = max(catastrophe_data$trial), by = 50)) +
#   scale_y_discrete()  # Assuming 'action' is a categorical variable
# 
# # Print the plot
# print(catastrophe_plot)


# old action repetition code
# # Your ggplot code with the scale_x_continuous set to go from 0 to 1000
# ggplot(df, aes(x = trial, y = action)) +
#   geom_point(aes(color = repetition), size = 2) +
#   geom_rect(data = df, aes(xmin = trial - 0.5, xmax = trial + 0.5, ymin = -Inf, ymax = Inf, fill = state_name), alpha = 0.5) +
#   scale_color_manual(values = c("#87CEFA","#FF6B6B"), labels = c("No", "Yes"), name = "Repetition") +
#   scale_fill_manual(values = c("darkgrey", "#F0F0F0"), labels = c("High", "Low"), name = "State(harm_exp)") +
#   scale_x_continuous(breaks = seq(0, 1000, 100), limits = c(1, 1000)) +
#   expand_limits(x=c(1,1000))+
#   scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
#   labs(x = "Trial", y = "Action", color = "Repetition") +
#   guides(color = guide_legend(override.aes = list(size = 4))) +
#   theme_classic(base_size = 14)+
#   theme(
#     axis.title.x = element_text(margin = margin(t = 10), size = 16, color = "black", face = "bold"),
#     axis.title.y = element_text(margin = margin(r = 10), size = 16, color = "black", face = "bold"),
#     axis.text = element_text(size = 16, color = "black"),
#     axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 14, face = "italic"),
#     legend.title = element_text(size = 16, face = "bold"),
#     legend.text = element_text(size = 14)
#   )
