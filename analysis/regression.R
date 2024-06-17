library(brms)
df$repetition=lag(df$action)==df$action
df=df%>%filter(trial!=1)
df_1=df%>%filter(state==1)

# Step 1: Filter the DataFrame where state == 1
filtered_df <- df %>% filter(state == 1)

# Step 2: Count actions for each agent
action_counts <- filtered_df %>% 
  group_by(subject, action) %>% 
  summarise(action_count = n(), .groups = 'drop',cost_action=mean(cost_action),frequency_cata=mean(frequency_catastrophe),valence_cata=mean(valence_catastrophe))%>%
group_by(subject) %>%
  mutate(total_actions = sum(action_count))

# Step 3: Identify the most frequent action for each agent

action_proportions <- action_counts %>%
  mutate(proportion = action_count / total_actions) %>%
  select(subject, action, proportion,cost_action,frequency_cata,valence_cata)

most_frequent_actions <- action_proportions %>% 
  group_by(subject) %>% 
  top_n(1, proportion) %>% 
  select(subject, action, proportion,cost_action,frequency_cata,valence_cata)

model_freq=
  brm(
    formula=repetition~0+Intercept+frequency_catastrophe,
    data = df_1,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 2,
    cores = 2,
    seed = 123,
    backend = "cmdstanr",
    #prior=mypriors
  )

save(model_freq,file=("data/regression/model_freq.rdata"))

model_cost=
  brm(
    formula=repetition~0+Intercept+cost_action,
    data = df_1,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 2,
    cores = 2,
    seed = 123,
    backend = "cmdstanr",
    #prior=mypriors
  )

save(model_cost,file=("data/regression/model_cost.rdata"))
