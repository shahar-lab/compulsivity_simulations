sim.agent <- function(subject, cfg) {
  print(paste('subject', subject))
  
  # Set parameters ---------------------------------------------------------
  
  Ntrials   = cfg$Ntrials
  Nstates   = cfg$Nstates
  Nactions  = cfg$Nactions
  
  cutoff    = cfg$cutoff    #cutoff for internal state transition
  eta       = cfg$eta       #action learning rate environmental controllability estimation
  betas     = cfg$betas     #self-control estimation for each action
  
  v_harm    = cfg$v_harm[subject]    #valence of the negative event
  freq_c    = cfg$freq_c[subject]    #frequency with which the negative event occurs
  pr        = cfg$pr[subject]        #perseveration cost
  f_p       = cfg$f_p       #forgetting rate for perseveration cost
  
  trigger_frequency  = cfg$trigger_frequency   #how often triggers occur
  trigger_strength   = cfg$trigger_strength    #the amplitude of the trigger
  natural_relax_rate = cfg$natural_relax_rate  #decay rate of accumulated triggers
  
  # Initialization ----------------------------------------------------------
  h = 0       #trigger accumulator
  p_harm = 0  #probability of how likely the agent thinks a negative event could occur
  
  state_value = rep(0, Nstates)              #critic
  thetas      = matrix(0, Nactions, Nstates) #actor
  gradient    = 0                            #actor
  cost        = matrix(0, Nactions, Nstates) #costs for actor
  
  df = data.frame()
  
  for (trial in 1:Ntrials) {
    # States transition and state value ----------------------------------------------
    
    if (p_harm >= cutoff) {
      state = 1 #dangerous
    }
    else{
      state = 2 #safe
    }
    
    state_value[state] = p_harm * v_harm #internal state value estimation
    
    
    # Action selection --------------------------------------------------------
    
    p         = exp(thetas[, state] + cost[, state]) / sum(exp(thetas[, state] + cost[, state]))
    action    = sample(1:Nactions, 1, prob = p)
    
    # Outcome -----------------------------------------------------------------
    
    reward = sample(c(0, v_harm), 1, p = c(1 - freq_c, freq_c)) #freq_c is the actual chance for catastrophe in the env
    
    # Save trial's data -------------------------------------------------------
    
    dfnew = data.frame(
      subject              = subject,
      trial                = trial,
      state                = state,
      action               = action,
      p                    = p[action],
      reward               = reward,
      p_harm               = p_harm,
      v_harm               = v_harm,
      state_value          = state_value[state],
      thetas               = thetas[action, state],
      eta                  = eta,
      natural_relax_rate   = natural_relax_rate,
      gradient             = gradient,
      cost_action          = cost[action, state],
      beta                 = betas[action, state]
    )
    
    df = rbind(df, dfnew)
    
    # Action preferences update ----------------------------------------------
    
    advantage = reward - state_value[state]
    
    gradient = p[action] * (1 - p[action])
    
    thetas[action, state] = thetas[action, state] + eta * betas[action, state] *
      advantage * gradient
    #eta is environmental controllability and betas are self-control for the specific action.
    
    # Threat belief dynamics --------------------------------------------------
    
    if (trial %% trigger_frequency == 0 |trial == 1) { 
      trigger = trigger_strength
    }
    else{
      trigger = 0
    }
    
    h = (h + trigger) * natural_relax_rate * (1 - p[action]) #trigger accumulation
    
    p_harm = (2 / (1 + exp(-h)) - 1) #goes between 0 and 1 for h values between 0 and +inf
    
    # Perseveration cost ------------------------------------------------------
    
    cost = (1 - f_p) * cost #forgetting
    cost[action, state] = cost[action, state] - pr #updating
  }
  return(df)
}
