# Simulate agents in 3 periods: before, during and after treatment.

rm(list = ls())
library(tidyverse)
source('model/advantage_actor_critic.R')
source('Exp3/visualization/plot_by_period.R')

# Set parameters ----------------------------------------------------------

cfg = list(
  Nsubjects = 100,
  Nperiods  = 3,
  Ntimesteps= 200,
  Nstates   = 2,
  Nactions  = 10,
  cutoff    = 0.05,
  natural_relax_rate = 0.95,
  trigger_frequency  = 100,
  trigger_strength   = 5,
  eta    = 0.1,
  f_p    = 0.01,
  treatment = c("before","during","after"), #0 is before, 1 is during and which periods are in treatment
  exposure_intensity = 20,
  treatment_cost = 100
)

#ERP
df5=data.frame()
cfg$v_harm =rnorm(cfg$Nsubjects,-100,5)
cfg$freq_c =rbeta(cfg$Nsubjects,1,1000)
cfg$pr     =rnorm(cfg$Nsubjects,0.02,0.05)
cfg$betas  = matrix(1, nrow = cfg$Nstates, ncol = cfg$Nactions)
cfg$beta_wait=rbeta(cfg$Nsubjects,2,10)
for (subject in 1:cfg$Nsubjects){
df5 = rbind(df5,sim.agent(subject, cfg))
}
save(df5, file = "Exp3/data/ERP.rdata")

#visualize
plot_unsuccesful_treatment=plot_by_period(df5%>%filter(subject==1))

plot_succesful_treatment=plot_by_period(df5%>%filter(subject==6))

plot_relapse=plot_by_period(df5%>%filter(subject==2))
