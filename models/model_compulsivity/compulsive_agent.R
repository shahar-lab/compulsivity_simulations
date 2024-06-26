#rm(list=ls())
library(tidyverse)
Nsubjects=1
Ntrials   = 1000
Nstates = 2
Nactions = 10

cfg=list(
  Nsubjects = Nsubjects,
  Ntrials   = Ntrials,
  Nstates = Nstates,
  Nactions = Nactions,
  v_harm = -100,
  p_harm= 1,
  cutofff=0.05,
  #set parameters
  alpha = 0.3, #updates for state values
  eta = 0.3, #updates for action preferences
  natural_relax_rate = 0.08,
  trigger_strength = 8,
  trigger_frequency = 100
  # betas = matrix(1,Nactions,Nstates) #scaling updates for action preferences by self-controllability
)
#cfg$betas[1,1]=0
source('models/model_compulsivity/advantage_actor_critic.R')
df=data.frame()
for (subject in 1:Nsubjects) {
  df=rbind(df, sim.agent(subject,cfg) )
}
save(df,file="data/df_advantage_new_model.rdata")

