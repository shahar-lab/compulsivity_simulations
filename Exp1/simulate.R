# Simulate agents, with low/high controllability ----------------------

rm(list = ls())
library(tidyverse)
source('model/advantage_actor_critic.R')
source('Exp1/plot.R')

# Set parameters ----------------------------------------------------------

Nagents = 100

cfg = list(
  Nsubjects = Nagents,
  Ntimesteps= 500,
  Nstates   = 2,
  Nactions  = 10,
  v_harm    = rep(-100, Nagents),
  cutoff    = 0.05,
  natural_relax_rate = 0.95,
  trigger_frequency  = 100,
  trigger_strength   = 5,
  freq_c = rep(0, Nagents),
  pr     = 0,
  f_p    = 0
)
cfg$betas  = matrix(1, nrow = cfg$Nstates, ncol = cfg$Nactions)

#High eta
cfg$eta = 0.1
df = sim.agent(1, cfg)
plot_action_proportions(df, cfg$Ntimesteps)
save(df, file = "Exp1/data/df_high_eta.rdata")

df_high_eta_all = map_dfr(1:Nagents, function(i) sim.agent(i, cfg))
save(df_high_eta_all, file = "Exp1/data/df_high_eta_all.rdata")


#Low eta
cfg$eta = 0.01
df = sim.agent(1, cfg)
plot_action_proportions(df, cfg$Ntimesteps)
save(df, file = "Exp1/data/df_low_eta.rdata")

df_low_eta_all = map_dfr(1:Nagents, function(i) sim.agent(i, cfg))
save(df_low_eta_all, file = "Exp1/data/df_low_eta_all.rdata")


# Combine controllability levels and plot mode-action-share boxplot -------

df_high_eta_all$controllability = "High"
df_low_eta_all$controllability  = "Low"
df_all = bind_rows(df_high_eta_all, df_low_eta_all)

p_mode_share = plot_mode_action_share(df_all)
ggsave("Exp1/mode_action_share.png", p_mode_share, width = 4.5, height = 4, dpi = 150)
ggsave("Exp1/mode_action_share.pdf", p_mode_share, width = 4.5, height = 4)
