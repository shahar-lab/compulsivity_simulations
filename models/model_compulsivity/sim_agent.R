sim.agent<-function(cfg){
  
  Nsubjects = cfg$Nsubjects
  Ntrials   = cfg$Ntrials
  Nactions  = cfg$Nactions
  
  aspiration_level = cfg$aspiration_level
  valence           = cfg$valence
  frequency_outcome = cfg$frequency_outcome
  cost_action       = cfg$cost_action
  #set parameters
  alpha = cfg$alpha
  beta  = cfg$beta
  w1 = cfg$w1
  w2  = cfg$w2
  
  df=data.frame()
  count_action=rep(0,Nactions)
  for (subject in 1:Nsubjects){
    values=rep(0,Nactions)
    for (trial in 1:Ntrials){
      #players choice
      p         = exp(beta*(values - cost_action*count_action)) / sum(exp(beta*(values - cost_action*count_action)))
      choice    = sample(1:Nactions,1,prob=p)
      count_action[choice]=count_action[choice]+1 
      #outcome 
      outcome = sample(valence,1,prob=frequency_outcome)

      #save trial's data
      
      #create data for current trials
      dfnew=data.frame(
        subject              = subject,
        trial                = trial,
        choice               = choice,
        cost1                = cost_action[1]*count_action[1],
        cost2                = cost_action[2]*count_action[2],
        outcome              = outcome,
        value_ch             = values[choice],
        value1               = values[1],
        value2               = values[2],
        w1                   = w1,
        w2                   = w2,
        aspiration_level     = aspiration_level[trial,]
      )
      
      df=rbind(df,dfnew)
      
      #update values
      subjective_outcome = outcome-aspiration_level[trial,]
      PE = subjective_outcome - values[choice]
      values[choice] = values[choice] + alpha*(PE)
    }
  }
  return(df)
}
