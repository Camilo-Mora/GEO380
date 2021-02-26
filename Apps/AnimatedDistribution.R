library(animation)


Average_USMaleHeigh=177.8 # height of average male in the USA
SD_USMaleHeigh=7.62       # Standard deviation of male population in the USA
Iteractions= 100
breaks = seq(140, 220, length.out = 80)


#Set delay between frames when replaying
ani.options(interval=.01)



ResultSamples=c() #Empty vector to store results cumulative
saveGIF({

  # For the most part, it’s safest to start with graphical settings in
  # the animation loop, as the loop adds a layer of complexity to
  # manipulating the graphs. For example, the layout specification needs to
  # be within animation loop to work properly.
  layout(matrix(c(1, rep(2, 5)), 6, 1))

  # Adjust the margins a little
  par(mar=c(4,4,2,1) + 0.5)

  # Begin the loop that creates the 150 individual graphs


  for (i in 1:Iteractions) {

    # Pull 100 observations from a normal distribution
    # and add a constant based on the iteration to move the distribution
    Sample1<- rnorm(1500,Average_USMaleHeigh,SD_USMaleHeigh) #we take a random sample of 100 individual of a US population distribution


    # Set up the top chart that keeps track of the current frame/iteration
    # Dress it up a little just for fun
    plot(-5, xlim = c(1,Iteractions), ylim = c(0, .3), axes = F, xlab = "", ylab = "", main = "Sample number")
    abline(v=i, lwd=5)


    # Bring back the X axis
    axis(1)

    # Set the color of the bottom chart based on the distance of the distribution’s mean from 0
    # par(fg = col.range[mean(chunk)+3])

    # Set up the bottom chart
    ResultSamples=c(ResultSamples,Sample1)
    #plot(density(chunk), main = "", xlab = "X Value", xlim = c(-5, 15), ylim = c(0, .6))
    hist(ResultSamples,main=NA,xlim=c(120, 230),ylim=c(0, 10000),breaks = breaks, xlab = "US men heigh (cm)",ylab = "Number of people",cex.lab=2,cex.axis=1.3)

    #plot mean and test
    abline(v=Average_USMaleHeigh, lwd=2, col="blue")
    legend(x=Average_USMaleHeigh-15,y=10500, legend="Mean", text.col="blue", bg="white",cex=2.5,bty = "o",box.col="white")


    Minus2SD=Average_USMaleHeigh-(SD_USMaleHeigh*2)
    Plus2SD=Average_USMaleHeigh+(SD_USMaleHeigh*2)

    abline(v=Minus2SD, lwd=2, col="red")
    abline(v=Plus2SD, lwd=2, col="red")

    # text:
    text("+2 SDs",x=Plus2SD,y=9000, col="red", pos=4,cex=1.5)
    text("-2 SDs",x=Minus2SD,y=9000, col="red", pos=2,cex=1.5)

    #Add fraction of people to the right and lefft of of 2sd
    FracLessthan2Sd= paste(round(sum(ResultSamples <= Minus2SD, na.rm=TRUE) / length (ResultSamples) *100,2), "%")
    FracMorethan2Sd= paste(round(sum(ResultSamples >= Plus2SD, na.rm=TRUE) / length (ResultSamples) *100,2), "%")

    text(FracMorethan2Sd,x=Plus2SD+5,y=5000, col="red", pos=4,cex=5)
    text(FracLessthan2Sd,x=Minus2SD-40,y=5000, col="red", pos=4,cex=5)

  }},
  ani.width = 500,
  ani.height = 500,
)
