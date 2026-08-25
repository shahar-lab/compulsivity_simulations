# Sub-experiment A: modulate exposure_intensity (trigger frequency during
# treatment) from Low to High, holding treatment_cost fixed High.
# Depicts how more trigger exposures during treatment lead to more thorough
# extinction of the perceived threat (P(harm) drops further and stays low
# after treatment).

rm(list = ls())
library(tidyverse)
source('model/advantage_actor_critic.R')
source('Exp4/simulate_helpers.R')

Nsubjects_per_condition = 50

condition_labels = c("Low exposure", "High exposure")
condition_exposure_intensity = c(2, 6)
condition_treatment_cost     = c(200, 200) #treatment_cost fixed high

result = run_exp4_conditions(condition_labels, condition_exposure_intensity,
                              condition_treatment_cost, Nsubjects_per_condition)

df4_exposure = result$df4
subject_condition_exposure = result$subject_condition

save(df4_exposure, file = "Exp4/data/4_exposure.rdata")
save(subject_condition_exposure, condition_labels, file = "Exp4/data/4_exposure_labels.rdata")
