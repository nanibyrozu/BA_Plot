library(ggplot2)
library(BlandAltmanLeh)


#####################NANI BA Plot function################################

baplot<-function(datax, datay){
  par(mfrow=c(2,2))
  bmean<-(datax + datay)/2.0
  dif<-datax-datay
  
  m<-mean(dif)
  s<-sd(dif)
  l=m-1.96*s
  u=m+1.96*s
  line=c(l,m,u)
  
  plot(bmean, dif, ylim =c(-5.0, 5.0), pch=18, main="BA plot", xlab="Average", ylab="Difference")
  abline(h = line, lty=c(2,1,2), col=c("Red","blue","Red"), 
         lwd=c(3,2,3))
  abline(h=0,lty=3,col=c("black"))
  text(x=16, y=0.5+line[1],label=paste("LL", round(line[1], digits=3)))
  text(x=16, y=0.5+line[2],label=paste("CL", round(line[2], digits=3)))
  text(x=16, y=0.5+line[3],label=paste("UL", round(line[3], digits=3)))
  
  
  difpc<-((datax- datay) / bmean) * 100
  dm<- mean(difpc)
  dsd<-sd(difpc)
  ll<-dm-1.96*dsd
  ul<-dm+1.96*dsd
  lines<-c(ll,dm, ul)
  plot(bmean, difpc, ylim =c(-50, 50), pch=18, main="BA plot with differnce as % of average", xlab="Average", ylab="Difference (% of average)")
  abline(h = lines, lty=c(2,1,2), col=c("Red","blue","Red"), 
         lwd=c(3,2,3))
  abline(h=0,lty=3,col=c("black"))
  text(x=16, y=2.5+lines[1],label=paste("LL", round(lines[1], digits=3)))
  text(x=16, y=2.5+lines[2],label=paste("CL", round(lines[2], digits=3)))
  text(x=16, y=2.5+lines[3],label=paste("UL", round(lines[3], digits=3)))
  
  dmd<-median(dif)
  d5<-quantile(dif,0.05)
  d95<-quantile(dif,0.95)
  linesmd<-c(d5, dmd, d95)
  
  plot(bmean, dif, ylim =c(-5.0, 5.0), pch=18, main=" Non Parametric-BA plot", xlab="Average", ylab="Difference")
  abline(h = linesmd, lty=c(2,1,2), col=c("Red","blue","Red"), 
         lwd=c(3,2,3))
  abline(h=0,lty=3,col=c("black"))
  text(x=16, y=0.5+linesmd[1],label=paste("LL", round(linesmd[1], digits=3)))
  text(x=16, y=0.5+linesmd[2],label=paste("Median", round(linesmd[2], digits=3)))
  text(x=16, y=0.5+linesmd[3],label=paste("UL", round(linesmd[3], digits=3)))
  
  
  dataxl<-log(datax)
  datayl<-log(datay)
  lmean<-(dataxl + datayl)/2.0
  ldif<-dataxl-datayl
  
  lgm<-mean(ldif)
  lgs<-sd(ldif)
  lgl=lgm-1.96*lgs
  lgu=lgm+1.96*lgs
  lgline=c(lgl,lgm,lgu)
  
  plot(lmean, ldif, ylim =c(-1.0, 1.0), pch=18, main="Logarithmic BA plot", xlab="log Average", ylab="log Difference")
  abline(h = lgline, lty=c(2,1,2), col=c("Red","blue","Red"), 
         lwd=c(3,2,3))
  abline(h=0,lty=3,col=c("black"))
  text(x=2.8, y=0.05+lgline[1],label=paste("LL", round(exp(lgline[1]), digits=3)))
  text(x=2.8, y=0.05+lgline[2],label=paste("CL", round(exp(lgline[2]), digits=3)))
  text(x=2.8, y=0.05+lgline[3],label=paste("UL", round(exp(lgline[3]), digits=3)))
  
}

##################################