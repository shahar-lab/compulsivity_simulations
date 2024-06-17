sim.agent<-function(subject,cfg){
  print(paste('subject',subject))
  Ntrials   = cfg$Ntrials
  Nstates  = cfg$Nstates
  Nactions  = cfg$Nactions
  p_harm   = cfg$p_harm
  v_harm   = cfg$v_harm
  
  #set parameters
  alpha = cfg$alpha #state learning rate - relax rate
  eta   = cfg$eta   #action learning rate environmental controllability estimation
  betas  = cfg$betas  #self-control for each action (see Emanuel&Eldar, 2023)

  df=data.frame()
  state_value=rep(0,Nstates) #critic
  thetas=matrix(0,Nactions,Nstates) #actor
  state=1
  for (trial in 1:Ntrials){
    
    p       = exp(thetas[,state]) / sum(exp(thetas[,state]))
    action    = sample(1:Nactions,1,prob=p)
    #outcome
    if(action==1){ 
    reward=p_harm*v_harm # i.e., if action is mental simulation of negative event, the reward is negative
    }
    else{
    reward=0 #else, the reward is neutral
    }
    advantage=reward-state_value
    gradient = p[action]*(1-p[action]) #should we add the division by p?
    
    #save trial's data
    dfnew=data.frame(
      subject              = subject,
      trial                = trial,
      state                = state,
      action               = action,
      p                    = p[action],
      reward               = reward,
      advantage            = advantage,
      p_harm               = p_harm,
      v_harm               = v_harm,
      state_value          = state_value,
      thetas               = thetas[action,state],
      alpha                = alpha,
      eta                  = eta,
      betas                = betas[action,state]
    )
    
    df=rbind(df,dfnew)


    #state value update
    state_value=state_value+alpha*advantage
    
    #action preferences update
    thetas[action,state]=thetas[action,state]+eta*betas[action,state]*gradient*advantage 
    #eta is environmental controllability and betas are self-control for the specific chosen action.
    
  }
  return(df)
}
