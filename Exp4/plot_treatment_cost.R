rm(list = ls())
library(tidyverse)
source('Exp4/visualization/plot_extinction.R')
load(file = "Exp4/data/4_treatment_cost.rdata")
load(file = "Exp4/data/4_treatment_cost_labels.rdata")

dir.create("Exp4/figures", showWarnings = FALSE)

fig_p_ritual = plot_p_ritual_by_condition(df4_treatment_cost, subject_condition_treatment_cost, condition_labels)
ggsave("Exp4/figures/fig_p_ritual_by_treatment_cost.pdf", plot = fig_p_ritual, width = 6, height = 4.25)
