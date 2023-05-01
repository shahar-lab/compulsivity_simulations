rm(list=ls())
library(tidyverse)
library(RLR)
cfg=list(

  Nsubjects = 1,
  Ntrials   = 1000,
  Nactions = 4,
  
  aspiration_level  = -randomwalk(1,1000,tau=0.3), #This is what the agent expects to get all over the actions.
  valence           = c(-10,0),  #There are 2 possible outcomes, one bad and one good
  frequency_outcome = c(0.001,0.999), #The bad outcome is uncommon and the good one is common
  cost_action       = c(0,0,0,0), #There are four actions that are unrelated to the outcome, but have a different cost

  #set parameters
  alpha = 0.3,
  beta  = 15,
  w1    =0,
  w2    =1
)

source('models/model_compulsivity/sim_agent.R')
source('models/model_compulsivity/plot.R')
df = sim.agent(cfg) #run function
plot_df(df,number_of_trials_to_look_on=500) #plot

