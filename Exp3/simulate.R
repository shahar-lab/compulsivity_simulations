# Simulate two samples examining within-agent variability in action costs and action associability.
# Sample 1: per-action repetition cost varies; estimate association with ritual formation.
# Sample 2: per-action associability (beta) varies; estimate association with ritual formation.

rm(list = ls())
library(tidyverse)
source('model/advantage_actor_critic.R')

# Set parameters (high controllability agent from Exp1) -------------------

Nagents = 200

cfg = list(
  Nsubjects  = Nagents,
  Ntimesteps = 500,
  Nstates    = 2,
  Nactions   = 10,
  cutoff     = 0.05,
  natural_relax_rate = 0.95,
  trigger_frequency  = 100,
  trigger_strength   = 5,
  eta        = 0.1,
  f_p        = 0.01,
  v_harm     = rep(-100, Nagents),
  freq_c     = rep(0, Nagents),
  betas      = matrix(1, nrow = 2, ncol = 10)
)

# Sample 1: per-action repetition cost variability ------------------------
# pr~|N(0.5, 0.5)| sampled once per action, shared across all 200 subjects
cfg$pr = abs(rnorm(cfg$Nactions, 0.5, 0.5))

df3A = data.frame()
for (subject in 1:cfg$Nsubjects){
  df3A = rbind(df3A, sim.agent(subject, cfg))
}
save(df3A, file = "Exp3/data/3A.rdata")

# Sample 2: per-action associability variability --------------------------
# betas sampled once per action, shared across all subjects
cfg$pr = 0
cfg$betas = matrix(rbeta(cfg$Nactions, 1, 1),
                   nrow = cfg$Nstates, ncol = cfg$Nactions, byrow = TRUE)

df3B = data.frame()
for (subject in 1:cfg$Nsubjects){
  df3B = rbind(df3B, sim.agent(subject, cfg))
}
save(df3B, file = "Exp3/data/3B.rdata")
