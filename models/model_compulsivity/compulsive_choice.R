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
  valence           = c(0,1),  #There are 2 possible outcomes, one bad and one neutral
  frequency_outcome = c(0.8,0.2), #The bad outcome is uncommon and the neutral one is common
  cost_action       = matrix(0,Nstates,Nactions), #There are two actions that are unrelated to the outcome, but have a different cost

  #set parameters
  alpha = 0.3,
  omega = c(1,1)
  )

cfg$anxiety[1] = 100
source('models/model_compulsivity/sim_agent.R')
source('models/model_compulsivity/plot.R')
df = sim.agent(cfg) #run function
df=df%>%mutate(flag=state==lag(state))
plot_df(cfg,df,number_of_trials_to_look_on=1000) #plot
