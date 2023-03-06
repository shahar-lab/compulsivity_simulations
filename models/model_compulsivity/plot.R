df%>%filter(trial<=1000)%>%ggplot(aes(x=1:1000,y=value1))+geom_point(color = "firebrick", shape = "x", size = 3)+geom_line(color = "firebrick", linetype = "dotted", linewidth = .3)+labs(x = "Trial", y = "Value")+
 theme(axis.title.x = element_text(margin = margin(t = 10), size = 15),
axis.title.y = element_text(margin = margin(r = 10), size = 15))+
theme(axis.title = element_text(size = 15, color = "firebrick",
face = "italic"))+
theme(axis.title = element_text(color = "sienna", size = 15, face = "bold"),
axis.title.y = element_text(face = "bold.italic"))+
theme(axis.text = element_text(color = "black", size = 12),
axis.text.x = element_text(face = "italic"))+
theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12))
+ggtitle('Value of lower cost action by trial')
+theme(plot.title.position="plot",plot.title = element_text(face = "bold",margin = margin(10, 0, 10, 0),size = 14,color = "sienna"))