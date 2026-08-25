sim.agent <- function(subject, cfg) {
  print(paste('subject', subject))

  # Set parameters ---------------------------------------------------------

  Nperiods  = cfg$Nperiods
  Ntimesteps= cfg$Ntimesteps
  Nstates   = cfg$Nstates
  Nactions  = cfg$Nactions

  cutoff    = cfg$cutoff    #cutoff for internal state transition
  eta       = cfg$eta       #action learning rate environmental controllability estimation
  betas     = cfg$betas     #self-control estimation for each action
  alpha_T   = if (length(cfg$alpha_T) > 1) cfg$alpha_T[subject] else cfg$alpha_T   #learning rate for trigger threat value (T)

  v_harm    = cfg$v_harm[subject]    #valence of the negative event
  freq_c    = cfg$freq_c[subject]    #frequency with which the negative event occurs
  f_p       = cfg$f_p       #forgetting rate for perseveration cost

  # per-action pr (same cost for both states) vs per-subject scalar pr
  if (length(cfg$pr) == Nactions) {
    pr = matrix(rep(cfg$pr, times = Nstates), nrow = Nstates, byrow = TRUE)
  } else {
    pr = matrix(rep(cfg$pr[subject]), Nstates, Nactions)
  }

  trigger_frequency      = cfg$trigger_frequency   #how often triggers occur
  trigger_frequency_base = cfg$trigger_frequency   #saved for reset after treatment
  trigger_strength   = cfg$trigger_strength    #the amplitude of the trigger
  natural_relax_rate = cfg$natural_relax_rate  #decay rate of accumulated triggers

  treatment=cfg$treatment
  exposure_intensity=cfg$exposure_intensity[subject]
  treatment_cost=cfg$treatment_cost[subject]

  # Initialization ----------------------------------------------------------
  h = 0       #trigger accumulator
  T_val = 1   #learned trigger threat value
  p_harm = 0  #probability of how likely the agent thinks a negative event could occur

  state_value = rep(0, Nstates)              #critic
  thetas      = matrix(0, Nstates,Nactions) #actor
  gradient    = 0                            #actor
  cost        = matrix(0, Nstates,Nactions) #costs for actor

  df = data.frame()
  ritual_action = NA #most frequent dangerous-state action in period 1, identified once period 1 ends

  for (period in 1:Nperiods){
    # Treatment period --------------------------------------------------------

    if(treatment[period]=="during"){
      trigger_frequency = round(trigger_frequency / exposure_intensity)
      pr[1,] = pr[1,] * treatment_cost
    }

    else if(treatment[period]=="after"){
      trigger_frequency = trigger_frequency_base
      pr[1,] = pr[1,] / treatment_cost
    }

    if (period > 1 && is.na(ritual_action)) {
      ritual_action = which.max(prior_action_counts)
    }

    action_counts = rep(0, Nactions)

  for (timestep in 1:Ntimesteps) {
    # States transition and state value ----------------------------------------------

    if (p_harm >= cutoff) {
      state = 1 #dangerous
    }
    else{
      state = 2 #safe
    }

    state_value[state] = p_harm * v_harm #internal state value estimation


    # Action selection --------------------------------------------------------

    logits    = thetas[state, ] + cost[state, ]
    p         = exp(logits - max(logits)) / sum(exp(logits - max(logits)))
    action    = sample(1:Nactions, 1, prob = p)

    # Dangerous-state selection probability of the original ritual action,
    # computed every timestep regardless of the current state, so its
    # trajectory is always defined once the ritual is identified.
    logits_dangerous = thetas[1, ] + cost[1, ]
    p_dangerous       = exp(logits_dangerous - max(logits_dangerous)) / sum(exp(logits_dangerous - max(logits_dangerous)))

    # Outcome -----------------------------------------------------------------

    reward = sample(c(0, v_harm), 1, p = c(1 - freq_c, freq_c)) #freq_c is the actual chance for catastrophe in the env

    # Track dangerous-state actions for ritual identification
    if (state == 1) {
      action_counts[action] = action_counts[action] + 1
    }

    # Save timestep's data -------------------------------------------------------

    dfnew = data.frame(
      subject              = subject,
      period               = period,
      timestep             = timestep,
      state                = state,
      action               = action,
      p                    = p[action],
      reward               = reward,
      p_harm               = p_harm,
      v_harm               = v_harm,
      pr                   = pr[1,action],
      freq_c               = freq_c,
      state_value          = state_value[state],
      thetas               = thetas[state,action],
      theta_ritual         = if (is.na(ritual_action)) NA else thetas[1, ritual_action],
      p_ritual             = if (is.na(ritual_action)) NA else p_dangerous[ritual_action],
      eta                  = eta,
      natural_relax_rate   = natural_relax_rate,
      gradient             = gradient,
      cost_action          = cost[state,action],
      beta                 = betas[state,action],
      T_val                = T_val,
      alpha_T              = alpha_T,
      treatment            = treatment[period],
      treatment_cost       = treatment_cost,
      exposure_intensity   = exposure_intensity,
      trigger_frequency = trigger_frequency
    )

    df = rbind(df, dfnew)

    # Action preferences update ----------------------------------------------

    advantage = reward - state_value[state]

    gradient = p[action] * (1 - p[action])

    thetas[state,action] = thetas[state,action] + eta * betas[state,action] *
      advantage * gradient
    #eta is environmental controllability and betas are self-control for the specific action.

    # Perseveration cost ------------------------------------------------------

      cost = (1 - f_p) * cost #forgetting
      cost[state,action] = cost[state,action] - pr[state,action] #updating


    # Threat belief dynamics --------------------------------------------------

    if (timestep %% trigger_frequency == 0 |timestep == 1) {
      trigger = trigger_strength
    }
    else{
      trigger = 0
    }

    catastrophe = as.numeric(reward == v_harm)

    if (trigger > 0) {
      T_val = T_val + alpha_T * (catastrophe - T_val) #update learned trigger threat value
    }

    h = (h + T_val * trigger) * natural_relax_rate * (1 - p[action]) #trigger accumulation

    p_harm = (2 / (1 + exp(-h)) - 1) #goes between 0 and 1 for h values between 0 and +inf


  }

  prior_action_counts = action_counts

  }
  return(df)
}
