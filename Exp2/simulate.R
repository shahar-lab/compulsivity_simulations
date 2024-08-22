# Simulate 4 populations while varying: v_harm,freq_c,and cost. ----------------------

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
  eta    = 0.1,
  f_p    = 0.01,
  treatment = "before"
)

#Baseline
df1=data.frame()
cfg$betas  = matrix(1, nrow = cfg$Nstates, ncol = cfg$Nactions)
cfg$beta_wait=rep(1,cfg$Nsubjects)
cfg$v_harm =rnorm(cfg$Nsubjects,-100,5)
cfg$freq_c =rbeta(cfg$Nsubjects,1,1000)
cfg$pr     =rnorm(cfg$Nsubjects,0.02,0.05)

for (subject in 1:cfg$Nsubjects){
df1 = rbind(df1,sim.agent(subject, cfg))
}
save(df1, file = "Exp2/data/baseline.rdata")


#Low V(harm)
cfg$v_harm =rnorm(cfg$Nsubjects,-10,5)
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

#High repetition cost
cfg$freq_c =rbeta(cfg$Nsubjects,1,1000)
cfg$pr     =rnorm(cfg$Nsubjects,2,0.5)
df4=data.frame()
for (subject in 1:cfg$Nsubjects){
df4 = rbind(df4,sim.agent(subject, cfg))
}
save(df4, file = "Exp2/data/high_cost.rdata")


df_list <- list(
  "Baseline" = df1, 
  "Low V(harm)" = df2, 
  "High freq(c)" = df3,
  "High cost" = df4)

compare_repetitions(df_list)
plot_regression(path='Exp2/data/regression.rdata',path_save="C:\\Users\\Ido\\My Drive\\graphics\\compulsivity_simulations\\actor_critic\\Exp2\\plot.eps")
