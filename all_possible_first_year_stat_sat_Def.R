library(dplyr)
library(lubridate)


firstyear=function(a,b,c,d,e,an1){
  str1=readin_rhessys_output(a,g=0)
  str2=readin_rhessys_output(b,g=0)
  str3=readin_rhessys_output(c,g=0)
  str4=readin_rhessys_output(d,g=0)
  str5=readin_rhessys_output(e,g=0)
  tmp.1=subset(str1$bd,str1$bd$wy==an1)
  tmp.2=subset(str2$bd,str2$bd$wy==an1)
  tmp.3=subset(str3$bd,str3$bd$wy==an1)
  tmp.4=subset(str4$bd,str4$bd$wy==an1)
  tmp.5=subset(str5$bd,str5$bd$wy==an1)
  sat.1=aggregate(tmp.1$sat_def,by=list(tmp.1$wy),FUN=mean)
  sat.2=aggregate(tmp.2$sat_def,by=list(tmp.2$wy),FUN=mean)
  sat.3=aggregate(tmp.3$sat_def,by=list(tmp.3$wy),FUN=mean)
  sat.4=aggregate(tmp.4$sat_def,by=list(tmp.4$wy),FUN=mean)
  sat.5=aggregate(tmp.5$sat_def,by=list(tmp.5$wy),FUN=mean)
  rz.1=aggregate(tmp.1$rz_storage,by=list(tmp.1$wy),FUN=mean)
  rz.2=aggregate(tmp.2$rz_storage,by=list(tmp.2$wy),FUN=mean)
  rz.3=aggregate(tmp.3$rz_storage,by=list(tmp.3$wy),FUN=mean)
  rz.4=aggregate(tmp.4$rz_storage,by=list(tmp.4$wy),FUN=mean)
  rz.5=aggregate(tmp.5$rz_storage,by=list(tmp.5$wy),FUN=mean)
  unsat.1=aggregate(tmp.1$unsat_stor,by=list(tmp.1$wy),FUN=mean)
  unsat.2=aggregate(tmp.2$unsat_stor,by=list(tmp.2$wy),FUN=mean)
  unsat.3=aggregate(tmp.3$unsat_stor,by=list(tmp.3$wy),FUN=mean)
  unsat.4=aggregate(tmp.4$unsat_stor,by=list(tmp.4$wy),FUN=mean)
  unsat.5=aggregate(tmp.5$unsat_stor,by=list(tmp.5$wy),FUN=mean)
  sd1=sat.1$x-rz.1$x-unsat.1$x
  sd2=sat.2$x-rz.2$x-unsat.1$x
  sd3=sat.3$x-rz.3$x-unsat.1$x
  sd4=sat.4$x-rz.4$x-unsat.1$x
  sd5=sat.5$x-rz.5$x-unsat.1$x
    pcp=aggregate(tmp.5$precip,by=list(tmp.5$wy),FUN=sum)
  kk=cbind(pcp,sd1,sd2,sd3,sd4,sd5)
  colnames(kk)=c("year","pcp","base","thin75","thin50","thin25","thin95")
  kk
}


setwd("D:/arnas/out/results_thin_multiplier2001/")
thin01=firstyear("sen1","sen5","sen9","sen13","sen17",2002)
setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=firstyear("sen1","sen5","sen9","sen13","sen17",2003)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=firstyear("sen1","sen5","sen9","sen13","sen17",2004)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=firstyear("sen1","sen5","sen9","sen13","sen17",2005)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=firstyear("sen1","sen5","sen9","sen13","sen17",2006)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=firstyear("sen1","sen5","sen9","sen13","sen17",2007)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=firstyear("sen1","sen5","sen9","sen13","sen17",2008)
setwd("D:/arnas/out/results_thin_multiplier2008/")
thin08=firstyear("sen1","sen5","sen9","sen13","sen17",2009)
setwd("D:/arnas/out/results_thin_multiplier2009/")
thin09=firstyear("sen1","sen5","sen9","sen13","sen17",2010)
setwd("D:/arnas/out/results_thin_multiplier2010/")
thin10=firstyear("sen1","sen5","sen9","sen13","sen17",2011)
setwd("D:/arnas/out/results_thin_multiplier2011/")
thin11=firstyear("sen1","sen5","sen9","sen13","sen17",2012)
setwd("D:/arnas/out/results_thin_multiplier2012/")
thin12=firstyear("sen1","sen5","sen9","sen13","sen17",2013)
setwd("D:/arnas/out/results_thin_multiplier2013/")
thin13=firstyear("sen1","sen5","sen9","sen13","sen17",2014)
setwd("D:/arnas/out/results_thin_multiplier2014/")
thin14=firstyear("sen1","sen5","sen9","sen13","sen17",2015)
setwd("D:/arnas/out/results_thin_multiplier2015/")
thin15=firstyear("sen1","sen5","sen9","sen13","sen17",2016)
setwd("D:/arnas/out/results_thin_multiplier2016/")
thin16=firstyear("sen1","sen5","sen9","sen13","sen17",2017)

thin=rbind(thin01,thin02,thin03,thin04,thin05,thin06,thin07,thin08,thin09,thin10,thin11,thin12,thin13,thin14,thin15,thin16)


################################################
################################################   
################################################

setwd("D:/arnas/out/results_thin_multiplier2001/")
thin01=firstyear("sen2","sen6","sen10","sen14","sen18",2002)
setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=firstyear("sen2","sen6","sen10","sen14","sen18",2003)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=firstyear("sen2","sen6","sen10","sen14","sen18",2004)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=firstyear("sen2","sen6","sen10","sen14","sen18",2005)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=firstyear("sen2","sen6","sen10","sen14","sen18",2006)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=firstyear("sen2","sen6","sen10","sen14","sen18",2007)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=firstyear("sen2","sen6","sen10","sen14","sen18",2008)
setwd("D:/arnas/out/results_thin_multiplier2008/")
thin08=firstyear("sen2","sen6","sen10","sen14","sen18",2009)
setwd("D:/arnas/out/results_thin_multiplier2009/")
thin09=firstyear("sen2","sen6","sen10","sen14","sen18",2010)
setwd("D:/arnas/out/results_thin_multiplier2010/")
thin10=firstyear("sen2","sen6","sen10","sen14","sen18",2011)
setwd("D:/arnas/out/results_thin_multiplier2011/")
thin11=firstyear("sen2","sen6","sen10","sen14","sen18",2012)
setwd("D:/arnas/out/results_thin_multiplier2012/")
thin12=firstyear("sen2","sen6","sen10","sen14","sen18",2013)
setwd("D:/arnas/out/results_thin_multiplier2013/")
thin13=firstyear("sen2","sen6","sen10","sen14","sen18",2014)
setwd("D:/arnas/out/results_thin_multiplier2014/")
thin14=firstyear("sen2","sen6","sen10","sen14","sen18",2015)
setwd("D:/arnas/out/results_thin_multiplier2015/")
thin15=firstyear("sen2","sen6","sen10","sen14","sen18",2016)
setwd("D:/arnas/out/results_thin_multiplier2016/")
thin16=firstyear("sen1","sen5","sen9","sen13","sen17",2017)
thin.warm=rbind(thin01,thin02,thin03,thin04,thin05,thin06,thin07,thin08,thin09,thin10,thin11,thin12,thin13,thin14,thin15,thin16)
sat_def=cbind(thin,thin.warm)

setwd("D:/arnas/out/Totales/")

write.table(sat_def,"first_year_stat_SatDef.csv",sep=";",dec=",",col.names = T,row.names = F)



