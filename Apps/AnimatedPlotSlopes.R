
X=c(0.5, 1.9, 2.4, 3.5, 5) #hours studying
Y=c(0.7, 1.2, 2.6, 4,4.7)       #grades

MeanX=mean(X)
MeanY=mean(Y)

ValsX=X-MeanX
ValsY=Y-MeanY



Data=data.frame(X=as.numeric(),Y=as.numeric(),Predicted=as.numeric(),Residuals=as.numeric(),SSE=as.numeric(),Slope=as.numeric(),Names=as.character(),X1=as.numeric(),Y1=as.numeric(),X2=as.numeric(),Y2=as.numeric())
Names=c("Laura", "Peter", "Tom", "Chip","John")

for (Slope in seq(2.5, 0, by=-.1)){

  Predicted=Slope*ValsX
  Residuals=round (ValsY-Predicted,3)
  #now let's do the plot
  SSE=paste("SSE = ", round(sum(Residuals^2),4),sep="")

Results=data.frame(X=ValsX,Y=ValsY,Predicted=Predicted,Residuals=Residuals,SSE=SSE,Slope=Slope,Names=Names,X1=ValsX[1],Y1=Predicted[1],X2=ValsX[5],Y2=Predicted[5])
Data=rbind(Data,Results)

}

theme_set(theme_bw())
sa=   Data %>%  ggplot( aes(x=X, y=Y)) +
    geom_point(colour = "black", size = 4.5) +
  geom_segment(x=Data$X1,y=Data$Y1, xend=Data$X2, yend=Data$Y2,col="blue",lwd=1)+
   geom_segment(x=Data$X,y=Data$Y, xend=Data$X, yend=Data$Predicted, col="orange", lty=1,lwd=2)+
   geom_text(x=-2,y=3,label=Data$SSE,size=12)+
  geom_text(x=Data$X-.5,y=Data$Y,label=Data$Names)+
  geom_text(x=Data$X+.5,y=(Data$Y-Data$Residuals/2),label=Data$Residuals)+

    xlim(-5,5)+
    ylim(-5,5)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))


  sa +transition_states(Slope, wrap = FALSE)
