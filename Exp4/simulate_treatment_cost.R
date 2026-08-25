# Sub-experiment B: modulate treatment_cost (cost of performing the ritual
# during treatment) from Low to High, holding exposure_intensity fixed High.
# Depicts how a high treatment_cost suppresses the ritual's selection
# probability during treatment, and whether that suppression persists or
# relapses once the cost is removed after treatment.

rm(list = ls())
library(tidyverse)
source('model/advantage_actor_critic.R')
source('Exp4/simulate_helpers.R')

Nsubjects_per_condition = 50

condition_labels = c("Low treatment cost", "High treatment cost")
condition_exposure_intensity = c(6, 6) #exposure_intensity fixed high
condition_treatment_cost     = c(2, 200)

result = run_exp4_conditions(condition_labels, condition_exposure_intensity,
                              condition_treatment_cost, Nsubjects_per_condition)

df4_treatment_cost = result$df4
subject_condition_treatment_cost = result$subject_condition

save(df4_treatment_cost, file = "Exp4/data/4_treatment_cost.rdata")
save(subject_condition_treatment_cost, condition_labels, file = "Exp4/data/4_treatment_cost_labels.rdata")
