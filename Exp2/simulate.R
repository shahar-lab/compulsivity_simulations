# Simulate 3 populations while varying: v_harm and freq_c. ----------------------

rm(list = ls())
library(tidyverse)
source('model/advantage_actor_critic.R')
source('Exp2/visualization/compare_repetitions.R')

# Set parameters ----------------------------------------------------------

cfg = list(
  Nsubjects = 100,
  Nperiods  = 1,
  Ntimesteps= 500,
  Nstates   = 2,
  Nactions  = 10,
  cutoff    = 0.05,
  natural_relax_rate = 0.95,
  trigger_frequency  = 100,
  trigger_strength   = 5,
  treatment_cost=rep(1,100), #nothing
  exposure_intensity=rep(1,100), #nothing
  eta    = 0.1,
  f_p    = 0.01,
  treatment = "before"
)

#Baseline
df1=data.frame()
cfg$betas  = matrix(1, nrow = cfg$Nstates, ncol = cfg$Nactions)
cfg$v_harm =rnorm(cfg$Nsubjects,-100,5)
cfg$freq_c =rbeta(cfg$Nsubjects,1,1000)
cfg$pr     = 0

for (subject in 1:cfg$Nsubjects){
df1 = rbind(df1,sim.agent(subject, cfg))
}
save(df1, file = "Exp2/data/baseline.rdata")


#Low V(harm)
cfg$v_harm =rnorm(cfg$Nsubjects,-50,30)
df2=data.frame()
for (subject in 1:cfg$Nsubjects){
df2 = rbind(df2,sim.agent(subject, cfg))
}
save(df2, file = "Exp2/data/low_v_harm.rdata")

#High catastrophe frequency
cfg$v_harm =rnorm(cfg$Nsubjects,-100,5)
cfg$freq_c =rbeta(cfg$Nsubjects,1,1)
df3=data.frame()
for (subject in 1:cfg$Nsubjects){
df3 = rbind(df3,sim.agent(subject, cfg))
}
save(df3, file = "Exp2/data/high_freq_c.rdata")
