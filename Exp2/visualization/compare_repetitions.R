
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
