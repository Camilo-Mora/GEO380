library(animation)
library(TeachingDemos)

ani.options(
  convert = shQuote('C:/Program Files (x86)/ImageMagick-6.8.1-Q16/convert.exe')
)



DinamicPlot = function(frames) {


  CumFractions=c()
  for (i in seq(0, 4.9, length.out=frames)) {

    dev.hold()
    options(digits=2)
    mfrow = c(1, 2)
    par(mfrow = mfrow)

#Distribution plot
        Data = data.frame(X=seq(-5,10, length.out = 100))#bins for the distribution

            Y1=0
            Y2=5-i

            sd1=1
            sd2=1

            Data$Y1<- dnorm(Data$X,Y1,sd1)
            Data$Y2<- dnorm(Data$X,Y2,sd2)

            # calculate intersection densities
            Data$w <- pmin(Data$Y1, Data$Y2)

            # integrate areas under curves

            total <- integrate.xy(Data$X, Data$Y1) + integrate.xy(Data$X, Data$Y2)
            intersection <- integrate.xy(Data$X, Data$w)

            # compute overlap coefficient
            overlap <- round(2 * intersection / total,3)*100



            plot(-10, xlim=c(-5,10), ylim=c(0,0.5),xlab = 'Data values', ylab = 'Frequency',xaxs="i",yaxs="i")


            #because you are plotting three distributions, it will be nice to use different colors. and because they likely overlap, you should use semitransparent colors
            LightBlue <- rgb(173,216,230,max = 255, alpha = 100) #the function rgb lets your select one color, and the alpha gives you how tranparent
            DarkRed <- rgb(255,192,203, max = 255, alpha = 95)

            polygon(c(1, Data$X, 10), c(0, Data$Y1, 0),  col = LightBlue)
            polygon(c(1, Data$X, 10), c(0, Data$Y2, 0),  col = DarkRed)

            abline(v=Y1, lwd=2, col="blue", lty=2)
            abline(v=Y2, lwd=2, col="red", lty=2)

            #Check for significance
            Z=zsum.test(mean.x=Y1, sigma.x = sd1, n.x = 5,
                        mean.y = Y2, sigma.y = sd2, n.y = 5,
                        alternative = "two.sided", mu = 0,conf.level = 0.95)


            PValue=round(Z$p.value,3)

            Result=c(PValue,overlap)
            CumFractions=rbind(CumFractions, Result) #store fractions for next plot

            shadowtext(-4.9,0.43,labels="P-value =",pos=4, col="red",cex=1.5)
            shadowtext(-2,0.43,labels=PValue,pos=4, col="red",cex=1.5)

            shadowtext(-4.9,0.47,labels="Overlap =",pos=4, col="red",cex=1.5)
            shadowtext(-2,0.47,labels=overlap,pos=4, col="red",cex=1.5)










    #Relationship plot

    plot(CumFractions[,1], CumFractions[,2],pch="*", xlab="P-Value",ylab = "Overlap (Percent)", ylim=c(0, 100), xlim=c(0, 1)) #
    abline(v=0.05, col="red", lty=2)
    text(0.09,95,labels="0.05", col="red", cex=0.9)

    text(.8,0,labels="by Camilo Mora, Ph.D.", col="black", cex=0.9)
    # ani.pause()
  }
}



saveGIF(
  {
    DinamicPlot(80)
  },
  movie.name = "OverlapAndPvalue.gif",
  interval = 0.5,
  ani.width = 1000,
  ani.height = 500,
  outdir = getwd()
)
