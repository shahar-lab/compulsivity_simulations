# Simulate two samples examining within-agent variability in action costs and self-control.
# Sample 1: per-action repetition cost varies; estimate association with ritual formation.
# Sample 2: per-action self-control (beta) varies; estimate association with ritual formation.

rm(list = ls())
library(tidyverse)
source('model/advantage_actor_critic.R')

# Set parameters (high controllability agent from Exp1) -------------------

cfg = list(
  Nsubjects  = 200,
  Ntimesteps = 500,
  Nstates    = 2,
  Nactions   = 10,
  cutoff     = 0.05,
  natural_relax_rate = 0.95,
  trigger_frequency  = 100,
  trigger_strength   = 5,
  eta        = 0.1,
  f_p        = 0.01,
  v_harm     = rnorm(200, -100, 5),
  freq_c     = rbeta(200, 1, 1000),
  betas      = matrix(1, nrow = 2, ncol = 10)
)

# Sample 1: per-action repetition cost variability ------------------------
# pr~N(2, 0.5) sampled once per action, shared across all 200 subjects
cfg$pr = abs(rnorm(cfg$Nactions, 0.5, 0.5))

df3A = data.frame()
for (subject in 1:cfg$Nsubjects){
  df3A = rbind(df3A, sim.agent(subject, cfg))
}
save(df3A, file = "Exp3/data/3A.rdata")

# Sample 2: per-action self-control variability ---------------------------
# betas sampled once per action, shared across all subjects
cfg$pr = 0
cfg$betas = matrix(rbeta(cfg$Nactions, 1, 1),
                   nrow = cfg$Nstates, ncol = cfg$Nactions, byrow = TRUE)

df3B = data.frame()
for (subject in 1:cfg$Nsubjects){
  df3B = rbind(df3B, sim.agent(subject, cfg))
}
save(df3B, file = "Exp3/data/3B.rdata")
