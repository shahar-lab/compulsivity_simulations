sim.agent<-function(subject,cfg){
  print(paste('subject',subject))
  Nsubjects = cfg$Nsubjects
  Ntrials   = cfg$Ntrials
  Nstates  = cfg$Nstates
  Nactions  = cfg$Nactions
  trigger_frequency=cfg$trigger_frequency
  trigger_strength=cfg$trigger_strength
  value=rep(0,Nstates)
  harm_exp= cfg$harm_exp
  relax_rate=cfg$relax_rate
  learning_rate=cfg$learning_rate
  cutoff_harm_exp=cfg$cutoff_harm_exp
  eta = cfg$eta
  valence = cfg$valence
  valence_catastrophe = cfg$valence_catastrophe
  p_harm = 1
  valence[3]=valence_catastrophe
  frequency_catastrophe = cfg$frequency_catastrophe
  frequency_outcome = cfg$frequency_outcome
  advantage= 0

  for (trial in 1:Ntrials){
    
    if (trial%%trigger_frequency==0|trial==1){ #There is a trigger every once in a while according to agent-specific trigger_frequency
      harm_exp=p_harm*valence_catastrophe #agent-specific trigger strength indicates how high does p_harm go
    }
    else{
      harm_exp = harm_exp +relax_rate*advantage
    }
    #state transition
    if (harm_exp<cutoff_harm_exp){ #when the harm_exp is more negative than the cutoff then the state is high anxiety
      state=1 #high anxiety
    }
    else{
      state=2 #low anxiety
    }

    
    value[state]=harm_exp #the value of the state increases (it's not so bad)
    


    p         = exp((eta[state,])) / sum(exp((eta[state,])))
    action    = sample(1:Nactions,1,prob=p)
    outcome = sample(valence,1,prob=c(frequency_outcome[action],1-frequency_outcome[action]-frequency_catastrophe,frequency_catastrophe))
    
    dfnew=data.frame(
      subject              = subject,
      trial                = trial,
      state                = state,
      action               = action,
      outcome              = outcome,
      harm_exp             = harm_exp,
      frequency_catastrophe= frequency_catastrophe,
      valence_catastrophe  = valence_catastrophe,
      state_name           = if_else(state==1,"High","Low"),
      EV                   = frequency_outcome[action],
      relax_rate           = relax_rate,
      trigger_frequency    = trigger_frequency,
      trigger_strength     = trigger_strength,
      catastrophe          = if_else(outcome==valence_catastrophe,1,0)
    )
    
    df=rbind(df,dfnew)

    advantage=outcome-value[state] #very bad expectation didn't occur
    gradient=p[action]*(1-p[action])
    eta[state,action]=eta[state,action]+learning_rate*advantage*gradient
  }
  return(df)
}
