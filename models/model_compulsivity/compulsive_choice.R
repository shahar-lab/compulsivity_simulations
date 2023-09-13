rm(list=ls())
library(tidyverse)
Ntrials   = 1000
Nstates = 2
Nactions = 5
cfg=list(
  Nsubjects = 1,
  Ntrials   = Ntrials,
  Nstates = Nstates,
  Nactions = Nactions,
  anxiety= rep(0,Ntrials), #The agent's anxiety level defines how worried he is that something bad will happen.
  cutoff_anxiety = 50,
  valence           = c(0,1),  #There are 2 possible outcomes, one neutral and one good
  frequency_outcome = c(0.8,0.2), #The neutral outcome is common and the good one is uncommon.
  cost_action       = matrix(0,Nstates,Nactions), #There are two actions that are unrelated to the outcome, but have a different cost

  #set parameters
  alpha = 0.3,
  omega = c(1,0)
  )

cfg$anxiety[1] = 0
source('models/model_compulsivity/sim_agent.R')
source('models/model_compulsivity/plot.R')
df = sim.agent(cfg) #run function
df=df%>%mutate(flag=state==lag(state),repetition=action==lag(action))%>%na.omit()

