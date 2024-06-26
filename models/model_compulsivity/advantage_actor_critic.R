sim.agent<-function(subject,cfg){
  print(paste('subject',subject))
  Ntrials   = cfg$Ntrials
  Nstates  = cfg$Nstates
  Nactions  = cfg$Nactions
  p_harm   = cfg$p_harm
  v_harm   = cfg$v_harm
  cutoff   = cfg$cutoff
  #set parameters
  eta   = cfg$eta   #action learning rate environmental controllability estimation
  natural_relax_rate = cfg$natural_relax_rate
  trigger_frequency = cfg$trigger_frequency
  trigger_strength = cfg$trigger_strength
  #betas  = cfg$betas  #self-control for each action (see Emanuel&Eldar, 2023)
  
  df=data.frame()
  state_value=rep(0,Nstates) #critic
  thetas=matrix(0,Nactions,Nstates) #actor
  h=trigger_strength/2
  
  for (trial in 1:Ntrials){
    
    if(p_harm<=cutoff){
      state=2
    }
    else{
      state=1
    }
    
    state_value[state]=p_harm*v_harm
    p         = exp(thetas[,state]) / sum(exp(thetas[,state]))
    action    = sample(1:Nactions,1,prob=p)
    #outcome
    reward=0

    
    #save trial's data
    dfnew=data.frame(
      subject              = subject,
      trial                = trial,
      state                = state,
      action               = action,
      p                    = p[action],
      reward               = reward,
      p_harm               = p_harm,
      v_harm               = v_harm,
      state_value          = state_value[state],
      thetas               = thetas[action,state],
      eta                  = eta,
      natural_relax_rate   = natural_relax_rate
      #,betas                = betas[action,state]
    )
    
    df=rbind(df,dfnew)
    
    advantage=reward-state_value[state]
    gradient = p[action]*(1-p[action]) #should we add the division by p?
    
    #action preferences update
    thetas[action,state]=thetas[action,state]+eta*gradient*advantage 
    #eta is environmental controllability and betas are self-control for the specific chosen action.
    
    #update p_harm
    if(trial%%trigger_frequency==0){
      trigger=trigger_strength
    }
    else{
      trigger=0
    }
    h=h+trigger-natural_relax_rate*thetas(action,state)
    p_harm=1/(1+exp(-h))
  }
  return(df)
}
