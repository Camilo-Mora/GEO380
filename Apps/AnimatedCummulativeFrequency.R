library(animation)
ani.options(
  convert = shQuote('C:/Program Files (x86)/ImageMagick-6.8.1-Q16/convert.exe')
)



DinamicPlot = function(frames) {
  mean=0    # height of average male in the USA
  sd=1       # Standard deviation of male population in the USA

  # create a normal distribution
  x <- seq(-4,4,length=frames)*sd + mean  # data up to four SD from the mean
  y <- dnorm(x,mean,sd)


  CumFractions=c()
  for (i in 1:frames) {

    dev.hold()
    options(digits=2)
    mfrow = c(1, 2)
    par(mfrow = mfrow)
    #Distribution plot
    plot(x, y, type="l", xlab="Standard deviations",ylab = "Fraction of individuals", ylim=c(0, 0.45)) #
    Cum <-  round(pnorm(x[i],mean,sd),3)
    CumFractions=c(CumFractions, Cum) #store fractions for next plot
    CumFrac=paste(Cum*100, "%")
    text(x[i],0.41,labels=CumFrac,pos=4, col="red")

    #lets add a label to mean label
    text(mean,0.45,labels="Mean",pos=4, col="blue")

    NegativeAlpha =mean+x[i]

    #plot the critical thresholds set by alpha


    polygon(c(x[x<=NegativeAlpha], NegativeAlpha), c(y[x<=NegativeAlpha], y[x==max(x)]), col="red") #right hand tail

    abline(v=NegativeAlpha, lwd=2, col="red", lty=2)

    #plot the mean height of men
    abline(v=mean, lwd=2, col="blue", lty=2)

    #Relationship plot

    plot(x[1:i], CumFractions,pch="*", xlab="Standard deviations",ylab = "Cummulative fraction of population", ylim=c(0, 1), xlim=c(-4, 4)) #

    #plot data
    label="At "
    label2=round(x[i],2)
    label3=" SDs"

    label4="are "
    label5=Cum*100
    label6=" % "
    label7="of the population"



    text(-4,0.95,labels=label,pos=4, col="blue", cex=1.5)
    text(-4,0.88,labels=label2,pos=4, col="red", cex=2.5)
    text(-2.6,0.87,labels=label3,pos=4, col="blue", cex=1.5)

    text(-4,.8,labels=label4,pos=4, col="blue", cex=1.5)
    text(-4,.73,labels=label5,pos=4, col="red", cex=2.5)
    text(-2.8,.72,labels=label6,pos=4, col="blue", cex=1.5)
    text(-4,.65,labels=label7,pos=4, col="blue", cex=1.5)

    text(3,0,labels="by Camilo Mora, Ph.D.", col="black", cex=0.9)
   # ani.pause()
  }
}



saveGIF(
  {
    DinamicPlot(80)
  },
  movie.name = "CummulativeNormalDistribution.gif",
  interval = 0.2,
  ani.width = 1000,
  ani.height = 500,
  outdir = getwd()
)
