sim.agent<-function(subject,cfg){
  print(paste('subject',subject))
  Nsubjects = cfg$Nsubjects
  Ntrials   = cfg$Ntrials
  Nstates  = cfg$Nstates
  Nactions  = cfg$Nactions
  state=1
  treatment=0
  harm_exp = cfg$harm_exp
  p_harm   = cfg$p_harm
  relax_rate = cfg$relax_rate
  cutoff_harm_exp = cfg$cutoff_harm_exp
  valence           = cfg$valence
  frequency_outcome = cfg$frequency_outcome
  cost_action       = cfg$cost_action
  valence_catastrophe=cfg$valence_catastrophe
  frequency_catastrophe = cfg$frequency_catastrophe
  #set parameters
  alpha = cfg$alpha
  count_decay  = cfg$count_decay #this defines whether non-punishment reinforcing will occur.
  
  df=data.frame()
  count_action=rep(0,Nactions)
    condition=subject
    valence[3]=valence_catastrophe[condition]
    values=matrix(0,Nstates,Nactions)
    count_repeat=0
    for (trial in 1:Ntrials){
      harm_exp[trial]=p_harm[trial]*valence_catastrophe[condition]
      if (harm_exp[trial]<cutoff_harm_exp){
        state=1 #high 
      }
      else{
        state=2
        count_repeat=0
      }
      
      #players action
      if(count_repeat>=10 |(treatment==1 & state==1)){
        treatment=1
      }
      else {
        treatment=0
      }
      cost_treatment = -0.5*harm_exp[trial]
      cost=count_action*(cost_action+treatment*cost_treatment)
      p         = exp((values[state,] -cost )) / sum(exp((values[state,] - cost)))
      if(trial!=1){
      previous_action = action
      }
      else{
        previous_action=0
      }
      action    = sample(1:Nactions,1,prob=p)

      count_action[action]=count_action[action]+1 #we count how many times an action is repeated and it increases it's effective cost.
      count_action[-action]=count_action[-action]*exp(-count_decay)
      #outcome 
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
        total_cost_previous= if(previous_action == 0) 0 else cost[previous_action],
        total_cost_current = cost[action],
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
        harm_exp              = harm_exp[trial],
        condition            = condition,
        frequency_catastrophe= frequency_catastrophe,
        p_harm               = p_harm[trial],
        state_name           = if_else(state==1,"High","Low"),
        EV                   = frequency_outcome[action],
        relax_rate           = relax_rate,
        catastrophe = if_else(outcome==valence_catastrophe[condition],1,0)
      )
      
      df=rbind(df,dfnew)
      #update count_repeat
      if(previous_action==action){
        count_repeat=count_repeat+1
      }
      else{
        count_repeat =1
      }
      #updates
      p_harm[trial+1] = p_harm[trial]-runif(1,0,relax_rate) #harm_exp just randomly decreases
      p_harm[trial+1]=max(p_harm[trial+1],0) #keep it positive
      if (trial%%100==0){ #There is a trigger every once in a while
        p_harm[trial+1]=1
        }
     
      #update values
      subjective = outcome-harm_exp[trial] #The agent uses a subjective outcome instead of the objective one.
      affective_reward = omega*subjective+(1-omega)*outcome
      PE = affective_reward - values[state,action]
      
      values[state,action] = values[state,action] + alpha*(PE)
      
  }
  return(df)
}
