sim.agent<-function(cfg){
  
  Nsubjects = cfg$Nsubjects
  Ntrials   = cfg$Ntrials
  Nstates  = cfg$Nstates
  Nactions  = cfg$Nactions
  state=1
  anxiety = cfg$anxiety
  cutoff_anxiety = cfg$cutoff_anxiety
  valence           = cfg$valence
  frequency_outcome = cfg$frequency_outcome
  cost_action       = cfg$cost_action
  #set parameters
  alpha = cfg$alpha
  beta  = cfg$beta
  w1 = cfg$w1
  w2  = cfg$w2
  
  df=data.frame()
  count_action=matrix(0,Nstates,Nactions)
  for (subject in 1:Nsubjects){
    values=matrix(0,Nstates,Nactions)
    for (trial in 1:Ntrials){
      #players action
      p         = exp(beta*(values[state,] - cost_action[state,]*count_action[state,])) / sum(exp(beta*(values[state,] - cost_action[state,]*count_action[state,])))
      action    = sample(1:Nactions,1,prob=p)
      count_action[state,action]=count_action[state,action]+1 #we count how many times an action is repeated and it increases it's effective cost.
      #outcome 
      outcome = sample(valence,1,prob=frequency_outcome)

      #save trial's data
      
      #create data for current trials
      dfnew=data.frame(
        subject              = subject,
        trial                = trial,
        state                = state,
        action               = action,
        cost1                = cost_action[state,1]*count_action[state,1],
        cost2                = cost_action[state,2]*count_action[state,2],
        outcome              = outcome,
        value_ch             = values[state,action],
        w1                   = w1,
        w2                   = w2,
        anxiety              = anxiety[trial]
      )
      
      df=rbind(df,dfnew)
      
      #update values
      aspiration_level = anxiety*-1
      if (anxiety[trial]>cutoff_anxiety){
        state=1
        subjective_outcome = outcome-aspiration_level[trial] #The agent uses a subjective outcome instead of the objective one.
        PE = subjective_outcome - values[state,action]
      }
      else{
        PE = outcome - values[state,action]
        state=2
      }
      
      values[state,action] = values[state,action] + alpha*(PE)
      
      #update anxiety
      anxiety[trial+1] = anxiety[trial]-runif(1,0,2) #anxiety just randomly decreases
      anxiety[trial+1]=max(anxiety[trial+1],0)
      if (trial%%100==0){
        anxiety[trial+1]=anxiety[trial+1]+100
      }
    }
  }
  return(df)
}
