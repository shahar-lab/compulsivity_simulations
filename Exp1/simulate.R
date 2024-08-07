# Simulate two agents, with low/high controllability ----------------------

rm(list = ls())
library(tidyverse)
source('model/advantage_actor_critic.R')
source('Exp1/visualization/plot_action_proportions.R')

# Set parameters ----------------------------------------------------------

cfg = list(
  Nsubjects = 1,
  Ntrials   = 500,
  Nstates   = 2,
  Nactions  = 10,
  v_harm    = -100,
  cutoff    = 0.05,
  natural_relax_rate = 0.95,
  trigger_frequency  = 100,
  trigger_strength   = 5,
  freq_c = 0,
  pr     = 0,
  f_p    = 0,
  betas  = matrix(1, nrow = 10, ncol = 2) #Nactions, Nstates
)

#High eta
cfg$eta = 0.1
df = sim.agent(1, cfg)
plot_action_proportions(df)
save(df, file = "Exp1/data/df_high_eta.rdata")


#Low eta
cfg$eta = 0.01
df = sim.agent(subject, cfg)
plot_action_proportions(df)
save(df, file = "Exp1/data/df_low_eta.rdata")

