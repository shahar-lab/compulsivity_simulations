rm(list=ls())
library(tidyverse)
cfg=list(

  Nsubjects = 1,
  Ntrials   = 1000,
  Nactions  = 3,
  
  valence           = c(-100,0),  #There are 2 possible outcomes, one bad and one good
  frequency_outcome = c(0.1,0.9), #The bad outcome is uncommon and the good one is common
  cost_action       = c(-10,0,0), #There are three actions that are unrelated to the outcome, but have a different cost

  #set parameters
  alpha = 0.3,
  beta  = 4
)

source('models/model_compulsivity/sim_agent.R')
source('models/model_compulsivity/plot.R')
df = sim.agent(cfg) #run function
plot_df(df,number_of_trials_to_look_on=100) #plot

