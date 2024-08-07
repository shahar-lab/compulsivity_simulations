compare_repetitions = function(df_list){
results <- data.frame()

for (i in 1:length(df_list)) {
  df <- df_list[[i]]
  df_name <- names(df_list)[i]
  max_action_freq_df <- df %>%
    group_by(subject,state, action) %>%
    summarise(action_count = n(), .groups = 'drop') %>%
    group_by(subject,state) %>%
    mutate(total_actions = sum(action_count),
           frequency = action_count / total_actions) %>%
    summarise(max_frequency = max(frequency),
              action_number = action[which.max(frequency)],
              .groups = 'drop')
  
  max_action_freq_df <- max_action_freq_df %>%
    mutate(manipulation = df_name)
  
  results <- bind_rows(results, max_action_freq_df)
}
results$state=factor(results$state)
results <- results %>%
  mutate(state = recode(state, '1' = 'Dangerous', '2' = 'Safe'))
regression <-
  brm(
    formula=max_frequency~state*manipulation,
    data = results,
    family = gaussian,
    warmup = 1000,
    iter = 2000,
    chains = 2,
    cores = 2,
    seed = 123,
    backend = "cmdstanr"
  )
save(regression,file='Exp2/data/regression.rdata')
}
