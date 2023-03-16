plot_df=function(df,number_of_trials_to_look_on)
df%>%filter(trial<=number_of_trials_to_look_on)%>%ggplot(aes(x=1:number_of_trials_to_look_on,y=choice))+geom_point(color = "firebrick", shape = "x", size = 3)+
  geom_line(color = "firebrick", linetype = "dotted", linewidth = .3)+labs(x = "Trial", y = "Outcome|Value|Choice")+
 theme(axis.title.x = element_text(margin = margin(t = 10), size = 15),
axis.title.y = element_text(margin = margin(r = 10), size = 15))+
theme(axis.title = element_text(size = 15, color = "firebrick",
face = "italic"))+
theme(axis.title = element_text(color = "sienna", size = 15, face = "bold"),
axis.title.y = element_text(face = "bold.italic"))+
theme(axis.text = element_text(color = "black", size = 12),
axis.text.x = element_text(face = "italic"))+
theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12))+
#+ggtitle('Value of lower cost action by trial')
theme(plot.title.position="plot",plot.title = element_text(face = "bold",margin = margin(10, 0, 10, 0),size = 14,color = "sienna"))+
  geom_point(aes(x=trial,y=outcome/100),size=0.2,color = 'blue')+
  geom_line(aes(x=trial,y=value1/100+1),size=1,color = 'pink')+
  geom_line(aes(x=trial,y=value2/100+1),size=1,color = 'green')+
  geom_line(aes(x=trial,y=value3/100+1),size=1,color = 'yellow')
