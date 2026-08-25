# Shared setup for Exp4 sub-experiments: simulate N subjects per condition,
# sweeping exposure_intensity and/or treatment_cost, with all other
# parameters fixed like Exp1 (v_harm, freq_c, pr, betas, alpha_T).

run_exp4_conditions <- function(condition_labels, condition_exposure_intensity,
                                 condition_treatment_cost, Nsubjects_per_condition) {

  Nsubjects = Nsubjects_per_condition * length(condition_labels)

  cfg = list(
    Nsubjects = Nsubjects,
    Nperiods  = 3,
    Ntimesteps= 500,
    Nstates   = 2,
    Nactions  = 10,
    cutoff    = 0.05,
    natural_relax_rate = 0.95,
    trigger_frequency  = 100,
    trigger_strength   = 5,
    eta    = 0.1,
    f_p    = 0.01,
    alpha_T = 0.1,
    treatment = c("before","during","after")
  )

  df4=data.frame()
  cfg$v_harm =rep(-100, cfg$Nsubjects)
  cfg$freq_c =rep(0, cfg$Nsubjects)
  cfg$pr     = abs(rnorm(cfg$Nactions, 0.5, 0.5))
  cfg$betas  = matrix(1, nrow = cfg$Nstates, ncol = cfg$Nactions)

  condition = rep(1:length(condition_labels), each = Nsubjects_per_condition)
  cfg$exposure_intensity = condition_exposure_intensity[condition]
  cfg$treatment_cost      = condition_treatment_cost[condition]

  subject_condition = data.frame(subject = 1:Nsubjects, condition = condition,
                                  condition_label = condition_labels[condition])

  for (subject in 1:cfg$Nsubjects){
    df4 = rbind(df4,sim.agent(subject, cfg))
  }

  list(df4 = df4, subject_condition = subject_condition, condition_labels = condition_labels)
}
