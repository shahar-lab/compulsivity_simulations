rm(list = ls())
library(tidyverse)
source('Exp4/visualization/plot_extinction.R')
load(file = "Exp4/data/4_exposure.rdata")
load(file = "Exp4/data/4_exposure_labels.rdata")

dir.create("Exp4/figures", showWarnings = FALSE)

fig_exposure_panel = plot_extinction_outcome_panel(df4_exposure, subject_condition_exposure, condition_labels)
ggsave("Exp4/figures/fig_p_harm_by_exposure.pdf", plot = fig_exposure_panel, width = 6.8, height = 5.1)
