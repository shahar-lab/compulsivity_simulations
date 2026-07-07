rm(list = ls())
library(tidyverse)
library(brms)
library(cmdstanr)
source('Exp2/visualization/compare_repetitions.R')
source('Exp2/visualization/plot_regression.R')
load(file = "Exp2/data/high_freq_c.rdata")
load(file = "Exp2/data/low_v_harm.rdata")
load(file = "Exp2/data/baseline.rdata")

load(file = "Exp2/data/results.rdata")

df_list <- list(
  "Baseline" = df1,
  "Low V(harm)" = df2,
  "High freq(c)" = df3)

compare_repetitions(df_list)
plot_regression("Exp2/data/regression_v_harm.rdata", results %>% filter(manipulation == "Low V(harm)"))
plot_regression("Exp2/data/regression_freq_c.rdata", results %>% filter(manipulation == "High freq(c)"))
