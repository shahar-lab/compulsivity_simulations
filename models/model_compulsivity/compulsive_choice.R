Nsubjects = 1
Ntrials = 10000
Nactions = 3

valence = c(-100,0) #There are 2 possible outcomes, one bad and one good
frequency_outcome = c(0.1,0.9) #The bad outcome is uncommon and the good one is common
cost_action=c(1,10,10) #There are three actions that are unrelated to the outcome, but have a different cost

#set parameters
alpha = 0.3
beta  = 4

df=data.frame()

for (subject in 1:Nsubjects){
  values=rep(0,Nactions)
  for (trial in 1:Ntrials){
    #players choice
    p         = exp(beta*values) / sum(exp(beta*values))
    choice    = sample(1:Nactions,1,prob=p)
    
    #cost
    cost=cost_action[choice]
    #outcome 
    outcome = sample(valence,1,prob=frequency_outcome)
    total_outcome=outcome-cost
    
    #save trial's data
    
    #create data for current trials
    dfnew=data.frame(
      subject              = subject,
      trial                = trial,
      choice               = choice,
      cost                 = cost,
      outcome              = outcome,
      value_ch             = values[choice],
      value1               = values[1],
      value2               = values[2],
      value3               = values[3]
    )
    
    df=rbind(df,dfnew)
    
    #update values
    values[choice] = values[choice] + alpha*(total_outcome- values[choice])
  }
}