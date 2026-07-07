# Simulate agents in 3 periods: before, during and after treatment.
# Exp4: ERP targets only the ritualistic action (ritual_only mode)

rm(list = ls())
library(tidyverse)
source('model/advantage_actor_critic.R')
source('Exp4/visualization/plot_by_period.R')

# Set parameters ----------------------------------------------------------

cfg = list(
  Nsubjects = 100,
  Nperiods  = 3,
  Ntimesteps= 500,
  Nstates   = 2,
  Nactions  = 10,
  cutoff    = 0.05,
  natural_relax_rate = 0.95,
  trigger_frequency  = 100,
  trigger_strength   = 5,
  eta    = 0.1,
  f_p    = 0.01,
  treatment = c("before","during","after")
)

#Exp4: ERP targeting only the ritualistic action
df4=data.frame()
cfg$v_harm =rnorm(cfg$Nsubjects,-100,5)
cfg$freq_c =rbeta(cfg$Nsubjects,1,10000)
cfg$pr     = abs(rnorm(cfg$Nactions, 0.5, 0.5))
cfg$betas  = matrix(1, nrow = cfg$Nstates, ncol = cfg$Nactions)
cfg$exposure_intensity=pmax(rbeta(cfg$Nsubjects,2,10)*100, 1)
cfg$treatment_cost=pmax(rnorm(cfg$Nsubjects,70,20), 1)

for (subject in 1:cfg$Nsubjects){
df4 = rbind(df4,sim.agent(subject, cfg))
}
save(df4, file = "Exp4/data/4.rdata")

