library(dplyr)
library(lubridate)


first10year=function(a,b,c,d,e,an1){
  str1=readin_rhessys_output(a)
  str2=readin_rhessys_output(b)
  str3=readin_rhessys_output(c)
  str4=readin_rhessys_output(d)
  str5=readin_rhessys_output(e)
  tmp.1=subset(str1$bd,str1$bd$wy>=an1 & str1$bd$wy<=an1+9)
  tmp.2=subset(str2$bd,str2$bd$wy>=an1 & str2$bd$wy<=an1+9)
  tmp.3=subset(str3$bd,str3$bd$wy>=an1 & str3$bd$wy<=an1+9)
  tmp.4=subset(str4$bd,str4$bd$wy>=an1 & str4$bd$wy<=an1+9)
  tmp.5=subset(str5$bd,str5$bd$wy>=an1 & str5$bd$wy<=an1+9)
  stream1=aggregate(tmp.1$et,by=list(tmp.1$wy),FUN=sum)
  stream2=aggregate(tmp.2$et,by=list(tmp.2$wy),FUN=sum)
  stream3=aggregate(tmp.3$et,by=list(tmp.3$wy),FUN=sum)
  stream4=aggregate(tmp.4$et,by=list(tmp.4$wy),FUN=sum)
  stream5=aggregate(tmp.5$et,by=list(tmp.5$wy),FUN=sum)
  pcp=aggregate(tmp.5$precip,by=list(tmp.5$wy),FUN=sum)
  kk=cbind(pcp,stream1$x,stream5$x,stream2$x,stream3$x,stream4$x)
  colnames(kk)=c("year","pcp","base","thin95","thin75","thin50","thin25")
  kk
}

setwd("D:/arnas/out/results_thin_multiplier2001/")
thin01=first10year("sen3","sen7","sen11","sen15","sen19",2002)
setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=first10year("sen3","sen7","sen11","sen15","sen19",2003)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=first10year("sen3","sen7","sen11","sen15","sen19",2004)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=first10year("sen3","sen7","sen11","sen15","sen19",2005)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=first10year("sen3","sen7","sen11","sen15","sen19",2006)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=first10year("sen3","sen7","sen11","sen15","sen19",2007)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=first10year("sen3","sen7","sen11","sen15","sen19",2008)

thin=rbind(thin01,thin02,thin03,thin04,thin05,thin06,thin07)

#########################################warm
setwd("D:/arnas/out/results_thin_multiplier2001/")
thin01=first10year("sen4","sen8","sen12","sen16","sen20",2002)
setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=first10year("sen4","sen8","sen12","sen16","sen20",2003)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=first10year("sen4","sen8","sen12","sen16","sen20",2004)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=first10year("sen4","sen8","sen12","sen16","sen20",2005)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=first10year("sen4","sen8","sen12","sen16","sen20",2006)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=first10year("sen4","sen8","sen12","sen16","sen20",2007)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=first10year("sen4","sen8","sen12","sen16","sen20",2008)

thin.warm=rbind(thin01,thin02,thin03,thin04,thin05,thin06,thin07)
evap=cbind(thin,thin.warm)
setwd("D:/arnas/out/Totales/")
write.table(evap,"Evapotranspiration_evolution.csv",sep=";",dec=",",col.names = T,row.names = F)



