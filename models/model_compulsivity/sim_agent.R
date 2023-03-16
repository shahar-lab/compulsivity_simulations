sim.agent<-function(cfg){
  
  Nsubjects = cfg$Nsubjects
  Ntrials   = cfg$Ntrials
  Nactions  = cfg$Nactions
  
  valence           = cfg$valence
  frequency_outcome = cfg$frequency_outcome
  cost_action       = cfg$cost_action
  
  #set parameters
  alpha = cfg$alpha
  beta  = cfg$beta
  
  df=data.frame()
  
  for (subject in 1:Nsubjects){
    values=rep(0,Nactions)
    for (trial in 1:Ntrials){
      
      #players choice
      p         = exp(beta*(values + cost_action)) / sum(exp(beta*(values + cost_action)))
      choice    = sample(1:Nactions,1,prob=p)
      
      #outcome 
      outcome = sample(valence,1,prob=frequency_outcome)

      #save trial's data
      
      #create data for current trials
      dfnew=data.frame(
        subject              = subject,
        trial                = trial,
        choice               = choice,
        cost1                = cost_action[1],
        cost2                = cost_action[2],
        cost3                = cost_action[3],
        outcome              = outcome,
        value_ch             = values[choice],
        value1               = values[1],
        value2               = values[2],
        value3               = values[3]
      )
      
      df=rbind(df,dfnew)
      
      #update values
      values[choice] = values[choice] + alpha*(outcome- values[choice])
    }
  }
  return(df)
}
