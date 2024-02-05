sim.agent<-function(subject,cfg){
  print(paste('subject',subject))
  Nsubjects = cfg$Nsubjects
  Ntrials   = cfg$Ntrials
  Nstates  = cfg$Nstates
  Nactions  = cfg$Nactions
  state=1 #the agent starts at the "calm" state
  treatment=0 #the agent starts out of treatment
  harm_exp = cfg$harm_exp
  p_harm   = cfg$p_harm
  relax_rate = cfg$relax_rate
  cutoff_harm_exp = cfg$cutoff_harm_exp
  valence           = cfg$valence
  frequency_outcome = cfg$frequency_outcome
  cost_action       = cfg$cost_action
  valence_catastrophe=cfg$valence_catastrophe
  frequency_catastrophe = cfg$frequency_catastrophe
  trigger_frequency = cfg$trigger_frequency
  trigger_strength= cfg$trigger_strength
  #set parameters
  alpha = cfg$alpha
  count_decay  = cfg$count_decay 
  omega = cfg$omega #this defines whether non-punishment reinforcement will occur.
  
  df=data.frame()
  count_action=rep(0,Nactions) #counter of how many times each action was repeated
    condition=subject 
    valence[3]=valence_catastrophe[condition] 
    values=matrix(0,Nstates,Nactions) #the agent learns values in an internal-state dependent manner. 
    count_repeat=0 #the number of times the agent repeated a certain action is tracked to know if treatment should start
    
    for (trial in 1:Ntrials){
      harm_exp[trial]=p_harm[trial]*valence_catastrophe[condition] #p_harm is a probability and valence catastrophe is negative, leading to negative harm_exp.
      if (harm_exp[trial]<cutoff_harm_exp){ #when the harm_exp is more negative than the cutoff then the state is high anxiety
        state=1 #high anxiety 
      }
      else{
        state=2 #low anxiety
        count_repeat=0 #each time an agent gets into the calm state count_repeat is initialized so that it will get out of treatment
      }
      
      #players action
      if(count_repeat>=10 |(treatment==1 & state==1)){ #if the agent repeated the same action 10 times or more he will get into treatment and remain in it while he has not transitioned into the calm state.
        treatment=1
      }
      else {
        treatment=0
      }
      cost_treatment = -0.5*harm_exp[trial] #the treatment creates an external cost (response prevention) to the repeated action.
      #cost magnitude depends on the harm_exp to emphasize the idea that the therapist reduces the cost as the patient relaxes to establish "reversed exposure"
      
      cost=count_action*(cost_action+treatment*cost_treatment) #the cost has the basic cumulative factor and the treatment factor.
      #the action counter indicates which actions are repeated and thus directs the cost allocation.
      p         = exp((values[state,] -cost )) / sum(exp((values[state,] - cost))) #the cost is externally integrated with action values to adapt action propensities
      
      if(trial!=1){
      previous_action = action
      }
      else{
      previous_action=0
      }
      action    = sample(1:Nactions,1,prob=p)
      
      #change count action to be state-dependent and change relax rate to be based on that
      count_action[action]=count_action[action]+1 #we count how many times an action is repeated and it increases it's effective cost.
      count_action[-action]=count_action[-action]*exp(-count_decay) #the counter for all unchosen actions are decayed to reduce the cost for them.
      #outcome sampling
      #can be either positive, neutral and catastrophic.
      outcome = sample(valence,1,prob=c(frequency_outcome[action],1-frequency_outcome[action]-frequency_catastrophe,frequency_catastrophe))

      #save trial's data
      
      #create data for current trials
      dfnew=data.frame(
        subject              = subject,
        trial                = trial,
        state                = state,
        action               = action,
        cost_action          = cost_action,
        cost_treatment       = cost_treatment,
        total_cost_previous  = if(previous_action == 0) 0 else cost[previous_action], 
        total_cost_current   = cost[action],
        count_repeat         = count_repeat,
        treatment            = treatment,
        objective            = outcome,
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
        harm_exp             = harm_exp[trial],
        condition            = condition,
        frequency_catastrophe= frequency_catastrophe,
        p_harm               = p_harm[trial],
        state_name           = if_else(state==1,"High","Low"),
        EV                   = frequency_outcome[action],
        relax_rate           = relax_rate,
        trigger_frequency    = trigger_frequency,
        trigger_strength     = trigger_strength,
        catastrophe = if_else(outcome==valence_catastrophe[condition],1,0)
      )
      
      df=rbind(df,dfnew)
      #update count_repeat to indicate treatment initiation
      if(previous_action==action){
        count_repeat=count_repeat+1 
      }
      else{
        count_repeat =1
      }
      #p_harm updates
      p_harm[trial+1] = p_harm[trial]-runif(1,0,relax_rate) #internal probability gradually decreases according to a agent-specific relaxation rate
      p_harm[trial+1]=max(p_harm[trial+1],0) #p_harm is between 0 and 1
      if (trial%%trigger_frequency==0){ #There is a trigger every once in a while according to agent-specific trigger_frequency
        p_harm[trial+1]=trigger_strength #agent-specific trigger strength indicates how high does p_harm go
        }
     
      #update values
      subjective = outcome-harm_exp[trial] #The agent also calculates a subjective outcome by comparing the objective outcome with its current harm_expectancy.
      reward_function = omega*outcome+(1-omega)*subjective #omega determines how much value updating is determined by subjective or objective values.
      #omega=1 meaning totally objective
      
      PE = reward_function - values[state,action] #the integration of objective and subjective outcomes are used for prediction_error calculation
      
      values[state,action] = values[state,action] + alpha*(PE) #value updating occurs normally
      
  }
  return(df)
}
