
rm(list = ls())
library(tidyverse)
library(brms)
library(cmdstanr)
load(file = "Exp3/data/3B.rdata")

results <- tibble()  # Initialize results storage

  
  ritual_freq_df <- df3B %>%
    group_by(subject, state, period, action) %>%
    summarise(action_count = n(), .groups = 'drop') %>%
    group_by(subject, state, period) %>%
    mutate(total_actions = sum(action_count),
           frequency = action_count / total_actions) %>%
    summarise(ritual_frequency = max(frequency),
              action_number = action[which.max(frequency)],
              .groups = 'drop')
  
  # Add subject-specific parameters
  ritual_freq_df <- ritual_freq_df %>%
    left_join(df3B %>%
                select(subject, exposure_intensity, treatment_cost) %>%
                distinct(),
              by = "subject")

  
  # Step 1: Select relevant columns from ritual_freq_df for state = 1 (Dangerous)
  most_frequent_action <- ritual_freq_df %>%
    filter(state == 1) %>%
    select(subject, period, max_frequency = ritual_frequency, max_action = action_number) %>%
    pivot_wider(names_from = period, values_from = c(max_frequency, max_action),
                names_glue = "{.value}_p{period}")
  
  # Step 2: Extract treatment cost & exposure intensity for periods 2 & 3
  treatment_info <- ritual_freq_df %>%
    filter(period %in% c(2, 3), state == 1) %>%
    select(subject, treatment_cost, exposure_intensity) %>%
    distinct()

  # Step 3: Identify repetitive rituals in period 1 (state = 1, "Dangerous")
  rituals_period1 <- most_frequent_action %>%
    select(subject, max_action_p1) %>%
    rename(ritual_action_p1 = max_action_p1)
  
  # Step 4: Identify subjects whose ritual was suppressed in period 2
  successful_therapy_p2 <- most_frequent_action %>%
    left_join(rituals_period1, by = "subject") %>%
    filter(max_action_p2 != ritual_action_p1) %>%
    select(subject) %>%
    mutate(successful_p2 = 1)
  
  # Step 5: Classify period 3 outcomes with correct relapse condition
  results_p3 <- most_frequent_action %>%
    left_join(rituals_period1, by = "subject") %>%
    left_join(successful_therapy_p2, by = "subject") %>%
    left_join(treatment_info, by = "subject") %>%
    mutate(
      successful_p2 = ifelse(is.na(successful_p2), 0, successful_p2),  # Fill missing with 0
      successful_p3 = ifelse(max_frequency_p3 <= 0.5, 1, 0),
      category = case_when(
        successful_p2 == 1 & successful_p3 == 0 ~ "Relapse",
        successful_p2 == 0 & successful_p3 == 0 ~ "Unsuccessful Treatment",
        successful_p3 == 1 ~ "Successful Therapy",
        TRUE ~ "Other"
      )
    ) %>%
    select(subject, category, successful_p2, successful_p3,
           max_action_p1, max_frequency_p1,
           max_action_p2, max_frequency_p2,
           max_action_p3, max_frequency_p3,
           treatment_cost, exposure_intensity)

  # Step 1: Extract the most frequent (ritualistic) action in period 1
  ritual_action_p1 <- df3B %>%
    filter(period == 1, state == 1) %>%  # Only for period 1 & state 1 (dangerous)
    group_by(subject, action) %>%
    summarise(action_count = n(), .groups = "drop") %>%
    group_by(subject) %>%
    mutate(total_actions = sum(action_count),
           frequency = action_count / total_actions) %>%
    slice_max(frequency, n = 1, with_ties = FALSE) %>%
    select(subject, ritual_action_p1 = action, ritual_frequency_p1 = frequency)
  
  # Step 2: Track the frequency of the same action in period 3
  ritual_frequency_p3 <- df3B %>%
    filter(period == 3, state == 1) %>%
    group_by(subject, action) %>%
    summarise(action_count = n(), .groups = "drop") %>%
    group_by(subject) %>%
    mutate(total_actions = sum(action_count),
           frequency = action_count / total_actions) %>%
    right_join(ritual_action_p1, by = c("subject", "action" = "ritual_action_p1")) %>%
    select(subject, ritual_frequency_p3 = frequency)
  
  # Step 3: Merge period 1 & 3 data
  df_ritual <- ritual_action_p1 %>%
    left_join(ritual_frequency_p3, by = "subject")
  
  # Convert to long format for brms
  df_long <- df_ritual %>%
    pivot_longer(cols = c(ritual_frequency_p1, ritual_frequency_p3),
                 names_to = "period", values_to = "max_frequency") %>%
    mutate(period = factor(ifelse(period == "ritual_frequency_p1", 
                                  "Before Treatment", 
                                  "After Treatment"),
                           levels = c("Before Treatment", "After Treatment")),
           ritual_frequency = ifelse(is.na(max_frequency), 0, max_frequency))  # Replace NA with 0

  brm_model_p3_success <- brm(
    successful_p3 ~ 0+Intercept,  # Predictor
    data = results_p3,
    backend="cmdstanr",
    family = bernoulli(link = "logit"),  # Logistic regression
    chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 1234
  )
  
  save(brm_model_p3_success, file = "Exp3/data/3B_regression_baseline.rdata")
  # Fit Bayesian logistic regression model for success in P3
  brm_model_p3 <- brm(
    successful_p3 ~ treatment_cost + exposure_intensity,
    data = results_p3,
    backend="cmdstanr",
    family = bernoulli(link = "logit"),  # Logistic regression
    chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 1234
  )
  save(brm_model_p3, file = "Exp3/data/3B_regression.rdata")
  c_eff <- conditional_effects(brm_model_p3)
  
  #creating plot
  plot(c_eff, plot = FALSE)[[1]]+theme_bw()
  plot(c_eff, plot = FALSE)[[2]]+theme_bw()
  plot(c_eff, plot = FALSE)[[3]]+theme_bw()
  