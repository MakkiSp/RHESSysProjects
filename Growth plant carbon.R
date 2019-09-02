library(dplyr)
library(lubridate)


first10year=function(a,b,c,d,e,an0){
  str1=readin_rhessys_output(a)
  str2=readin_rhessys_output(b)
  str3=readin_rhessys_output(c)
  str4=readin_rhessys_output(d)
  str5=readin_rhessys_output(e)
  tmp.1=subset(str1$bd,str1$bd$wy>=an0 & str1$bd$wy<=an0+10)
  tmp.2=subset(str2$bd,str2$bd$wy>=an0 & str2$bd$wy<=an0+10)
  tmp.3=subset(str3$bd,str3$bd$wy>=an0 & str3$bd$wy<=an0+10)
  tmp.4=subset(str4$bd,str4$bd$wy>=an0 & str4$bd$wy<=an0+10)
  tmp.5=subset(str5$bd,str5$bd$wy>=an0 & str5$bd$wy<=an0+10)
  stream1=aggregate(tmp.1$plantc,by=list(tmp.1$wy),FUN=mean)
  stream2=aggregate(tmp.2$plantc,by=list(tmp.2$wy),FUN=mean)
  stream3=aggregate(tmp.3$plantc,by=list(tmp.3$wy),FUN=mean)
  stream4=aggregate(tmp.4$plantc,by=list(tmp.4$wy),FUN=mean)
  stream5=aggregate(tmp.5$plantc,by=list(tmp.5$wy),FUN=mean)
  pcp=aggregate(tmp.5$precip,by=list(tmp.5$wy),FUN=sum)
  kk=cbind(pcp,stream1$x,stream5$x,stream2$x,stream3$x,stream4$x)
  colnames(kk)=c("year","pcp","base","thin95","thin75","thin50","thin25")
  kk
}

diff=function(data){
for (i in 2:nrow(data)) {
  data[i,3:7]=data[i,3:7]-data[1,3:7]
}
  data
}

setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=as.data.frame(first10year("sen3","sen7","sen11","sen15","sen19",2002))
thin02=diff(thin02)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=as.data.frame(first10year("sen3","sen7","sen11","sen15","sen19",2003))
thin03=diff(thin03)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=as.data.frame(first10year("sen3","sen7","sen11","sen15","sen19",2004))
thin04=diff(thin04)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=as.data.frame(first10year("sen3","sen7","sen11","sen15","sen19",2005))
thin05=diff(thin05)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=as.data.frame(first10year("sen3","sen7","sen11","sen15","sen19",2006))
thin06=diff(thin06)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=as.data.frame(first10year("sen3","sen7","sen11","sen15","sen19",2007))
thin07=diff(thin07)

base=cbind(thin02$base,thin03$base,thin04$base,thin05$base,thin06$base,thin07$base)
thin95=cbind(thin02$thin95,thin03$thin95,thin04$thin95,thin05$thin95,thin06$thin95,thin07$thin95)
thin75=cbind(thin02$thin75,thin03$thin75,thin04$thin75,thin05$thin75,thin06$thin75,thin07$thin75)
thin50=cbind(thin02$thin50,thin03$thin50,thin04$thin50,thin05$thin50,thin06$thin50,thin07$thin50)
thin25=cbind(thin02$thin25,thin03$thin25,thin04$thin25,thin05$thin25,thin06$thin25,thin07$thin25)

thin=cbind(base,thin95,thin75,thin50,thin25)

setwd("D:/arnas/out/Totales/")
write.table(thin,"10years_plantc.csv",sep=";",dec=",",col.names = T,row.names = F)


#########################################warm
setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=as.data.frame(first10year("sen4","sen8","sen12","sen16","sen20",2002))
thin02=diff(thin02)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=as.data.frame(first10year("sen4","sen8","sen12","sen16","sen20",2003))
thin03=diff(thin03)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=as.data.frame(first10year("sen4","sen8","sen12","sen16","sen20",2004))
thin04=diff(thin04)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=as.data.frame(first10year("sen4","sen8","sen12","sen16","sen20",2005))
thin05=diff(thin05)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=as.data.frame(first10year("sen4","sen8","sen12","sen16","sen20",2006))
thin06=diff(thin06)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=as.data.frame(first10year("sen4","sen8","sen12","sen16","sen20",2007))
thin07=diff(thin07)

base=cbind(thin02$base,thin03$base,thin04$base,thin05$base,thin06$base,thin07$base)
thin95=cbind(thin02$thin95,thin03$thin95,thin04$thin95,thin05$thin95,thin06$thin95,thin07$thin95)
thin75=cbind(thin02$thin75,thin03$thin75,thin04$thin75,thin05$thin75,thin06$thin75,thin07$thin75)
thin50=cbind(thin02$thin50,thin03$thin50,thin04$thin50,thin05$thin50,thin06$thin50,thin07$thin50)
thin25=cbind(thin02$thin25,thin03$thin25,thin04$thin25,thin05$thin25,thin06$thin25,thin07$thin25)

thin.warm=cbind(base,thin95,thin75,thin50,thin25)
colnames(thin.warm)=c("base","base","base","base","base","base","thin95","thin95","thin95","thin95","thin95","thin95","thin75","thin75","thin75","thin75","thin75","thin75","thin50","thin50","thin50","thin50","thin50","thin50","thin25","thin25","thin25","thin25","thin25","thin25")

setwd("D:/arnas/out/Totales/")
write.table(thin.warm,"10years_plantc.warm.csv",sep=";",dec=",",col.names = T,row.names = F)


