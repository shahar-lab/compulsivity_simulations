rm(list=ls())
library(tidyverse)
Nsubjects=1
Ntrials   = 2000
Nstates = 2
Nactions = 10
probs= seq(0.6,0.4,-0.2) #objective outcome probabilities.
cfg=list(
  Nsubjects = Nsubjects,
  Ntrials   = Ntrials,
  Nstates = Nstates,
  Nactions = Nactions,
  eta = matrix(0,Nstates,Nactions),
  p_harm= rep(0,Ntrials),#initialization of the agent's internal probability of harm defined by internal expectancy multiplied by catastrophe valence.
  harm_exp= rep(0,Ntrials),#the agent's harm_exp level defines the internal expectation that something bad will happen.
  cutoff_harm_exp = -5, #this cutoff defines the harm_exp values in which the agent moves between states
  valence_catastrophe=-100, #this defines the how bad (in amplitude) the agent thinks the catastrophe is.
  frequency_catastrophe=0.000001, #this defines the true objective probability by which catastrophes happen.
  valence           = c(1,0),  #these are the possible objective outcomes (other than the catastrophe)
  frequency_outcome = sapply(1:Nactions, function(x) sample(probs, 1)), #this are the probabilities of the objective outcome
  cost_action       = 0.1, # this is a general cost value for doing an action that will be multiplied by the number of repeated actions
  cost_treatment    = 1,
  count_decay       = 0.02, # the action counter decays each time the agent does not choose a certain action
  trigger_frequency = 100, #this determines how often a trigger will happen
  trigger_strength  = 1, # when a trigger occurs, this defines how high would the internal catastrophe expectancy become
  relax_rate        = 0.1, #following a trigger the agent slowly calms down
  learning_rate     = 0.3,
  #set parameters
  alpha = 0.3, #the agent's learning rate
  omega = 0, #the agent's degree of consideration of subjective outcomes in its value updating process (0-1 when 1 means totally objective)
  delta = 0.001
  )

cfg$p_harm[1] = 1 #initialization of p_harm to start at the maximal value
source('models/model_compulsivity/try_adv.R')
df=data.frame()
for (subject in 1:Nsubjects) {
  df=rbind(df, sim.agent(subject,cfg) )
}
save(df,file="data/df_treatment_new.rdata")
#df=df%>%mutate(flag=state==lag(state),repetition=action==lag(action))%>%na.omit()

