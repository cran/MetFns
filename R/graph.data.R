graph.data<-function(x,y,width,ytitle,xlim1,xlim2,xinc,ylim1,ylim2,yinc)
{
  if(!is.numeric(c(x,y,width,xlim1,xlim2,xinc,ylim1,ylim2,yinc)))
      stop("Invalid input parameter(s) specification: check values of x/y/width/xlim1/xlim2/xinc/ylim1/ylim2/yinc")
  
  if(xlim2<xlim1)
      stop("Invalid input parameter specification:xlim2 should be greater than xlim1")
  
  if(ylim2<ylim1)
      stop("Invalid input parameter specification:ylim2 should be greater than ylim1")
  
  if(xinc>xlim2-xlim1 || xinc<0)
      stop("Invalid input parameter specification:check value of xinc")
  
  if(yinc>ylim2-ylim1 || yinc<0)
      stop("Invalid input parameter specification:check value of yinc")
  
  
  if(!is.character(ytitle))
      stop("Invalid input parameter specification: check ytitle")

  par(mfrow=c(1,1))
  plotCI(x,y,width, pch=19, xlab="Solar longitude (J2000.0)",ylab=ytitle, main="",
         xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2),lwd=1.4,xaxt="n",yaxt="n", cex=1.3, cex.lab=1.2)
  xlabels<-seq(xlim1,xlim2,by=xinc)
  ylabels<-seq(ylim1,ylim2,by=yinc)
  axis(1,xlabels,labels=F,tcl=0.4)
  axis(1,xlabels,tcl=0.4)
  #axis(3,xlabels,labels=F,tcl=0.4)
  axis(2,ylabels,tcl=0.4)
  axis(4,ylabels,labels=F,tcl=0.4)
  abline(v=xlabels,lty=3,col="gray")
  abline(h=ylabels,lty=3,col="gray")

}
