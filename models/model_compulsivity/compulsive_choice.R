rm(list=ls())
library(tidyverse)
Nsubjects=1
Ntrials   = 1000
Nstates = 2
Nactions = 10
probs= seq(0.6,0.4,-0.1)
cfg=list(
  Nsubjects = Nsubjects,
  Ntrials   = Ntrials,
  Nstates = Nstates,
  Nactions = Nactions,
  p_harm= rep(0,Ntrials),
  harm_exp= rep(0,Ntrials),#The agent's anxiety level defines how worried he is that something bad will happen.
  cutoff_harm_exp = -5,
  valence_catastrophe=c(-100),
  frequency_catastrophe=0.00001,
  valence           = c(5,0),  #There are 2 possible outcomes, one neutral and one good
  frequency_outcome = sapply(1:Nactions, function(x) sample(probs, 1)),#The neutral outcome is common and the good one is uncommon.
  cost_action       = 0.1, # 0.1 the cost is multiplied by the times the agent does it
  count_decay       = 0.005, #0.005
  relax_rate        = 0.03,
  #set parameters
  alpha = 0.3
  )

cfg$p_harm[1] = 1
source('models/model_compulsivity/sim_agent.R')
df=data.frame()
for (subject in 1:Nsubjects) {
  df=rbind(df, sim.agent(subject,cfg) )
}
save(df,file="data/df_treatment.rdata")
#df=df%>%mutate(flag=state==lag(state),repetition=action==lag(action))%>%na.omit()

