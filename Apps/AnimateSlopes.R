#modified from here: https://yihui.org/animation/example/least-squares/
#https://github.com/yihui/animation/blob/master/R/least.squares.R

library(animation)
ani.options(
  convert = shQuote('C:/Program Files (x86)/ImageMagick-6.8.1-Q16/convert.exe')
)

X=c(0.5, 1.9, 2.4, 3.5, 5) #hours studying
Y=c(0.7, 1.2, 2.8, 4,4.7)       #grades

MeanX=mean(X)
MeanY=mean(Y)

ValsX=X-MeanX # center to zero
ValsY=Y-MeanY

MeanX=mean(ValsX)
MeanY=mean(ValsY)



least.squares = function(
  x, y, n = 5, ani.type = c('slope', 'intercept'), a, b, a.range, b.range,
  ab.col = c('gray', 'black'), est.pch = 19, v.col = 'red', v.lty = 2,
  rss.pch = 19, rss.type = 'o', mfrow = c(1, 2), ...
) {
  nmax = ani.options('nmax')
  if (missing(x)) x = 1:n
  if (missing(y)) y = x + rnorm(n)
  ani.type = match.arg(ani.type)
  fit = coef(lm(y ~ x))
  if (missing(a)) a = fit[1]
  if (missing(b)) b = fit[2]
  rss = rep(NA, nmax)
  par(mfrow = mfrow)
  if (ani.type == 'slope') {
    if (missing(b.range))
      bseq = tan(seq(pi/10, 3.5 * pi/10, length = nmax))
    else bseq = seq(b.range[1], b.range[2], length = nmax)


    RSSs=data.frame(Line=as.numeric(),RSS=as.numeric())
    for (i in 1:nmax) {
      dev.hold()
      plot(x, y, xaxt = 'n',yaxt = 'n', col="blue",pch="*", cex=2, ann=FALSE,xlim=c(-4,4),ylim=c(-4,4.5),...)

#Plot means as x and y axis


      abline(h=mean(y),lwd=2, col="grey",lty=2)  #Ymean
      legend(2.8,mean(y),"Y-mean",box.col = "lightblue", bg = "lightblue",xjust=0, adj = 0.2)

      abline(v=mean(x),lwd=2, col="grey",lty=2)  #Xmean
      legend(-1.5,4.8,"X-mean",box.col = "lightblue", bg = "lightblue",xjust=0, adj = 0.2)

      points(mean(x),mean(y), col="black",pch=16, cex=2,bg = "red")

      #adding the sse...
      RSS=paste ("SSE = ",round(sum((y - bseq[i] * x - a)^2) ,1),sep="")
      text(x=-2,y=3.5,label=RSS ,cex=1.5)

      LineNum=paste ("Line ",i,sep="")
      text(x=-2,y=3,label=LineNum ,cex=1.5)

      Names=c("Laura", "Peter", "Tom", "Chip","John")
      text(x,y,labels=Names,pos=2, col="orange")

#collect results
Results=c(Line=i,RSS=sum((y - bseq[i] * x - a)^2))
RSSs=rbind(Results,RSSs)

       # abline(fit, col = ab.col[1])
#Slope llines
      abline(a, bseq[i], col = "blue")
    #  points(x, bseq[i] * x + a, pch = est.pch)


      segments(x, bseq[i] * x + a, x, y, col = v.col, lty = 2)
      rss[i] = sum((y - bseq[i] * x - a)^2)
      plot(
        1:nmax, rss, xlab = "Line Number",
        ylab = 'Residual Sum of Squares', ylim=c(0,12),pch = rss.pch, type = rss.type
      )
      ani.pause()
    }
write.csv(RSSs,"RSS.csv")
    return(invisible(list(lmfit = fit, anifit = c(x = bseq[which.min(rss)]))))
  } else if (ani.type == 'intercept') {
    aseq = if (missing(a.range))
      seq(-5, 5, length = nmax) else seq(a.range[1], a.range[2], length = nmax)
    for (i in 1:nmax) {
      dev.hold()
      plot(x, y, ...)
      abline(fit, col = ab.col[1])
      abline(aseq[i], b, col = ab.col[2])
      points(x, b * x + aseq[i], pch = est.pch)
      segments(x, b * x + aseq[i], x, y, col = v.col, lty = v.lty)
      rss[i] = sum((y - b * x - aseq[i])^2)
      plot(
        1:nmax, rss, xlab = "Line Number",
        ylab = 'Residual Sum of Squares', pch = rss.pch, type = rss.type
      )
      ani.pause()
    }
    return(invisible(list(lmfit = fit, anifit = c('(Intercept)' = aseq[which.min(rss)]))))
  } else warning('Incorrect animation type!')

  }

#least.squares(ValsX,ValsY)






saveGIF(
  {
    least.squares(ValsX,ValsY)
  },
  movie.name = "test.gif",
  interval = 0.2,
  ani.width = 1000,
  ani.height = 500,
  outdir = getwd()
)

