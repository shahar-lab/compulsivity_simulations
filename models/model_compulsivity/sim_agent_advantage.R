sim.agent<-function(subject,cfg){
  print(paste('subject',subject))
  Nsubjects = cfg$Nsubjects
  Ntrials   = cfg$Ntrials
  Nstates  = cfg$Nstates
  Nactions  = cfg$Nactions
  harm_exp = cfg$harm_exp
  p_harm   = cfg$p_harm
  relax_rate = cfg$relax_rate
  cutoff_harm_exp = cfg$cutoff_harm_exp
  valence           = cfg$valence
  frequency_outcome = cfg$frequency_outcome
  cost_action       = cfg$cost_action
  cost_treatment    = cfg$cost_treatment
  valence_catastrophe=cfg$valence_catastrophe
  frequency_catastrophe = cfg$frequency_catastrophe
  trigger_frequency = cfg$trigger_frequency
  trigger_strength= cfg$trigger_strength
  #set parameters
  alpha = cfg$alpha
  delta = cfg$delta
  count_decay  = cfg$count_decay 
  omega = cfg$omega #this defines whether non-punishment reinforcement will occur.
  
  df=data.frame()
  count_action=matrix(0,Nstates,Nactions) #counter of how many times each action was repeated
  #subject_specific
  condition=subject
  frequency_catastrophe=frequency_catastrophe #study2
  #cost_action=cost_action[condition] #study 3
  valence[3]=valence_catastrophe
  #valence[3]=valence_catastrophe[condition] #study4
  values=matrix(0,Nstates,Nactions) #the agent learns values in an internal-state dependent manner. 
  #PE=0 #initialization for first trial 
  for (trial in 1:Ntrials){
    
    harm_exp[trial]=p_harm[trial]*valence_catastrophe #p_harm is a probability and valence catastrophe is negative, leading to negative harm_exp.
    
    if (harm_exp[trial]<cutoff_harm_exp){ #when the harm_exp is more negative than the cutoff then the state is high anxiety
      state=1 #high anxiety 
    }
    else{
      state=2 #low anxiety
    }
    
    # if(trial>1000&trial<2000&state==1){
    #   treatment=1
    # }
    # else{
    #   treatment=0
    # }
    
    # if((state==1)&((max(count_action[state,]*cost_action>=1)) |(treatment==1))){ #if the agent repeated the same action 10 times or more he will get into treatment and remain in it while he has not transitioned into the calm state.
    #   treatment=1
    # }
    # else {
    #   treatment=0
    # }
    # if(trial==1001){
    #   print("stop")
    # }
    # current_cost_treatment = ifelse(p_harm[trial]<0.2,-cost_treatment,cost_treatment) #the treatment creates an external cost (response prevention) to the repeated action.
    # state_cost = cost_action * count_action[state,]
    # state_cost_treatment=treatment*current_cost_treatment*count_action[state,]
    # 
    #cost magnitude depends on the harm_exp to emphasize the idea that the therapist reduces the cost as the patient relaxes to establish "reversed exposure"
    # total_cost=state_cost+state_cost_treatment #the cost has the basic cumulative factor and the treatment factor.
    #the action counter indicates which actions are repeated and thus directs the cost allocation.
    #p         = exp((values[state,] -total_cost )) / sum(exp((values[state,] - total_cost))) #the cost is externally integrated with action values to adapt action propensities
    
    p         = exp((values[state,])) / sum(exp((values[state,])))
    action    = sample(1:Nactions,1,prob=p)
    
    #change count action to be state-dependent and change relax rate to be based on that
    # count_action[state,action]=count_action[state,action]+1 #we count how many times an action is repeated and it increases it's effective cost.
    # count_action[state,-action]=count_action[state,-action]*exp(-count_decay) #the counter for all unchosen actions are decayed to reduce the cost for them.
    # outcome sampling
    #can be either positive, neutral and catastrophic.
    objective_outcome = sample(valence,1,prob=c(frequency_outcome[action],1-frequency_outcome[action]-frequency_catastrophe,frequency_catastrophe))
    
    #save trial's data
    
    #create data for current trials
    dfnew=data.frame(
      subject              = subject,
      trial                = trial,
      state                = state,
      action               = action,
      cost_action          = state_cost[action],
      treatment            = treatment,
      cost_treatment       = state_cost_treatment[action],
      total_cost_current   = total_cost[action],
      total_cost_previous  = if(previous_action == 0) 0 else total_cost[previous_action], 
      
      
      objective_outcome    = objective_outcome,
      value_ch             = values[state,action],
      value1               = values[state,1],
      value2               = values[state,2],
      value3               = values[state,3],
      value4               = values[state,4],
      value5               = values[state,5],
      value6               = values[state,6],
      value7               = values[state,7],
      value8               = values[state,8],
      value9               = values[state,9],
      value10              = values[state,10],
      cost1               = total_cost[1],
      cost2               = total_cost[2],
      cost3               = total_cost[3],
      cost4               = total_cost[4],
      cost5               = total_cost[5],
      cost6               = total_cost[6],
      cost7               = total_cost[7],
      cost8               = total_cost[8],
      cost9               = total_cost[9],
      cost10              = total_cost[10],
      harm_exp             = harm_exp[trial],
      frequency_catastrophe= frequency_catastrophe,
      valence_catastrophe  = valence_catastrophe,
      p_harm               = p_harm[trial],
      state_name           = if_else(state==1,"High","Low"),
      EV                   = frequency_outcome[action],
      relax_rate           = relax_rate,
      trigger_frequency    = trigger_frequency,
      trigger_strength     = trigger_strength,
      catastrophe = if_else(objective_outcome==valence_catastrophe,1,0),
      omega = omega
    )
    
    df=rbind(df,dfnew)
    
    #p_harm updates
    p_harm[trial+1] = max(p_harm[trial]-relax_rate,0)
    
    #p_harm[trial+1] = min(max(p_harm[trial]-((1-delta)*relax_rate+delta*values[state,action]),0),1) #internal probability gradually decreases according to a agent-specific relaxation rate
    if (trial%%trigger_frequency==0){ #There is a trigger every once in a while according to agent-specific trigger_frequency
      p_harm[trial+1]=trigger_strength #agent-specific trigger strength indicates how high does p_harm go
    }
    
    #update values
    # subjective = objective_outcome-harm_exp[trial] #The agent also calculates a subjective outcome by comparing the objective outcome with its current harm_expectancy.
    # outcome_net = omega*objective_outcome+(1-omega)*subjective #omega determines how much value updating is determined by subjective or objective values.
    # omega=1 meaning totally objective
    
    # PE = outcome_net - values[state,action] #the integration of objective and subjective outcomes are used for prediction_error calculation
    # values[state,action] = values[state,action] + alpha*(PE) #value updating occurs normally
    advantage=(1-p[choice])*outcome-p[!choice]*values[state,!action]
    
  }
  return(df)
}
