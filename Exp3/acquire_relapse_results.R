
rm(list = ls())
library(tidyverse)
load(file = "Exp3/data/ERP.rdata")
# Check treatment success and relapse -------------------------------------

#46 out of 100 agents have acquired the ritualistic action.
#Out of these 46, 28 have relapsed following treatment termination

period_results=function(data,period_number){
results=data %>%
  filter(period == period_number, state == 1) %>%
  group_by(subject, action) %>%
  summarise(
    action_count = n(),  # Count the number of times each action was made
  ) %>%
  group_by(subject) %>%
  mutate(
    total_actions = sum(action_count),  # Calculate total actions for each subject
    relative_frequency = action_count / total_actions  # Compute the relative frequency
  ) %>%
  ungroup() 
return(results)
}

before_results=period_results(df5,1)
treatment_results=period_results(df5,2)
after_results=period_results(df5,3)


# Check if treatment succeeded (waiting ritual acquired) ------------------

acquired=treatment_results%>%
  group_by(subject) %>%
  filter(relative_frequency == max(relative_frequency)) %>%
  select(subject, action, relative_frequency)%>%
  summarise(waiting=mean(action==10))

treatment_success=mean(acquired$waiting) # percent of agents who acquired the waiting action

acquired_subjects=acquired%>%filter(waiting==1)%>%pull(subject)

# Check if relapsed to previous ritual ------------------------------------

first_ritual=before_results%>%
  group_by(subject) %>%
  mutate(
    total_actions = sum(action_count),  # Calculate total actions for each subject
    relative_frequency = action_count / total_actions  # Compute the relative frequency
  ) %>%
  filter(relative_frequency == max(relative_frequency)) %>%
  select(subject, action, relative_frequency)


ritual=first_ritual%>%select(subject,action)
result = after_results %>%filter(subject%in%acquired_subjects)%>%
  inner_join(ritual%>%filter(subject%in%acquired_subjects), by = c("subject", "action" = "action"))  # Join based on subject and action

relapsed_subjects=result%>%pull(subject)

relapse_chance=length(relapsed_subjects)/length(acquired_subjects)
