library(ggplot2)
library(tidyr)
load(file = "Exp3/data/3A.rdata")
results <- tibble()  # Initialize results storage


ritual_freq_df <- df5 %>%
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
  left_join(df6 %>% 
              select(subject, exposure_intensity,treatment_cost) %>%
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
  select(subject, period, treatment_cost, exposure_intensity) %>%
  pivot_wider(names_from = period, values_from = treatment_cost, names_glue = "treatment_cost_p{period}") %>%
  left_join(
    ritual_freq_df %>%
      filter(period %in% c(2, 3), state == 1) %>%
      select(subject, period, exposure_intensity) %>%
      pivot_wider(names_from = period, values_from = exposure_intensity, names_glue = "exposure_intensity_p{period}"),
    by = "subject"
  )

# Step 3: Identify repetitive rituals in period 1 (state = 1, "Dangerous")
rituals_period1 <- most_frequent_action %>%
  select(subject, max_action_p1) %>%
  rename(ritual_action_p1 = max_action_p1)

# Step 4: Identify subjects who frequently did action 10 in period 2
successful_therapy_p2 <- most_frequent_action %>%
  filter(max_action_p2 == 10, max_frequency_p2 > 0.5) %>%
  select(subject) %>%
  mutate(successful_p2 = 1)

# Step 5: Classify period 3 outcomes with correct relapse condition
results_p3 <- most_frequent_action %>%
  left_join(rituals_period1, by = "subject") %>%
  left_join(successful_therapy_p2, by = "subject") %>%
  left_join(treatment_info, by = "subject") %>%
  mutate(
    successful_p2 = ifelse(is.na(successful_p2), 0, successful_p2),  # Fill missing with 0
    successful_p3 = ifelse(max_frequency_p3 <= 0.5 | max_action_p3 == 10, 1, 0),  # Define success in period 3
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
         treatment_cost_p2, exposure_intensity_p2,
         treatment_cost_p3, exposure_intensity_p3)
# Step 1: Plot Ritual Frequency Before and After Treatment
ritual_freq_plot <- results_p3 %>%
  select(subject, max_frequency_p1, max_frequency_p3) %>%
  pivot_longer(cols = c(max_frequency_p1, max_frequency_p3),
               names_to = "period", values_to = "ritual_frequency") %>%
  mutate(period = case_when(
    period == "max_frequency_p1" ~ "Before Treatment",
    period == "max_frequency_p3" ~ "After Treatment"
  ))

ggplot(ritual_freq_plot, aes(x = factor(period, levels = c("Before Treatment", "After Treatment")), 
                             y = ritual_frequency)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) + # Boxplot without outliers
  geom_jitter(width = 0.2, alpha = 0.3, color = "black") +  # Jitter to show individual points
  labs(y = "Ritual Frequency",
       x = "Treatment Period") +
  theme_minimal()

#brms regression of ritual frequency by period
library(brms)
ritual_freq_plot$period <- factor(ritual_freq_plot$period,levels = c("Before Treatment", "After Treatment"))
model <- brm(
  ritual_frequency ~ period,  # Predictor
  data = ritual_freq_plot,
  backend="cmdstanr",
  chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 1234
)
# Step 2: Define Ritual Substitution
results_p3 <- results_p3 %>%
  mutate(
    ritual_substitution = ifelse(
      max_frequency_p3 > 0.5 & max_action_p3 != max_action_p1, 1, 0
    )
  )
c_eff=conditional_effects(model)
plot(c_eff, plot = FALSE)[[1]]+theme_bw()

model_sub <- brm(
  ritual_substitution ~ 0+Intercept,  # Predictor
  data = results_p3,
  backend="cmdstanr",
  chains = 4, iter = 2000, warmup = 1000, cores = 4, seed = 1234
)
c_eff_sub=conditional_effects(model_sub)
plot(c_eff_sub, plot = FALSE)[[1]]+theme_bw()