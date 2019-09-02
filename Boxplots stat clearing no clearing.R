library(dplyr)
library(lubridate)


firstyear=function(a,b,c,d,an){
  str1=readin_rhessys_output(a,g=0)
  str2=readin_rhessys_output(b,g=0)
  str3=readin_rhessys_output(c,g=0)
  str4=readin_rhessys_output(d,g=0)
  tmp.1=subset(str1$bd,str1$bd$wy==an)
  tmp.2=subset(str2$bd,str2$bd$wy==an)
  tmp.3=subset(str3$bd,str3$bd$wy==an)
  tmp.4=subset(str4$bd,str4$bd$wy==an)
  k=NULL
  k[1]=sum(tmp.1$et)
  k[2]=sum(tmp.2$et)
  k[3]=sum(tmp.3$et)
  k[4]=sum(tmp.4$et)
  k
}

setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=firstyear("sen5","sen9","sen13","sen17",2003)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=firstyear("sen5","sen9","sen13","sen17",2004)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=firstyear("sen5","sen9","sen13","sen17",2005)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=firstyear("sen5","sen9","sen13","sen17",2006)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=firstyear("sen5","sen9","sen13","sen17",2007)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=firstyear("sen5","sen9","sen13","sen17",2008)

thin=NULL
thin[1:4]=thin02
thin[5:8]=thin03
thin[9:12]=thin04
thin[13:16]=thin05
thin[17:20]=thin06
thin[21:24]=thin07

setwd("D:/arnas/out/results_thin_multiplier2002/")
str0=readin_rhessys_output("sen1",g=0)
setwd("D:/arnas/out/results_thin_multiplier2003/")
str1=readin_rhessys_output("sen1",g=0)
setwd("D:/arnas/out/results_thin_multiplier2004/")
str2=readin_rhessys_output("sen1",g=0)
setwd("D:/arnas/out/results_thin_multiplier2005/")
str3=readin_rhessys_output("sen1",g=0)
setwd("D:/arnas/out/results_thin_multiplier2006/")
str4=readin_rhessys_output("sen1",g=0)
setwd("D:/arnas/out/results_thin_multiplier2007/")
str5=readin_rhessys_output("sen1",g=0)

tmp.0=subset(str0$bd,str0$bd$wy==2003)
tmp.1=subset(str1$bd,str1$bd$wy==2004)
tmp.2=subset(str2$bd,str2$bd$wy==2005)
tmp.3=subset(str3$bd,str3$bd$wy==2006)
tmp.4=subset(str4$bd,str4$bd$wy==2007)
tmp.5=subset(str5$bd,str5$bd$wy==2008)
str=NULL
str[1]=sum(tmp.0$et)
str[2]=sum(tmp.1$et)
str[3]=sum(tmp.2$et)
str[4]=sum(tmp.3$et)
str[5]=sum(tmp.4$et)
str[6]=sum(tmp.5$et)

#########################################warm

setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=firstyear("sen6","sen10","sen14","sen18",2003)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=firstyear("sen6","sen10","sen14","sen18",2004)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=firstyear("sen6","sen10","sen14","sen18",2005)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=firstyear("sen6","sen10","sen14","sen18",2006)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=firstyear("sen6","sen10","sen14","sen18",2007)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=firstyear("sen6","sen10","sen14","sen18",2008)

thin.warm=NULL
thin.warm[1:4]=thin02
thin.warm[5:8]=thin03
thin.warm[9:12]=thin04
thin.warm[13:16]=thin05
thin.warm[17:20]=thin06
thin.warm[21:24]=thin07

setwd("D:/arnas/out/results_thin_multiplier2002/")
str0=readin_rhessys_output("sen2",g=0)
setwd("D:/arnas/out/results_thin_multiplier2003/")
str1=readin_rhessys_output("sen2",g=0)
setwd("D:/arnas/out/results_thin_multiplier2004/")
str2=readin_rhessys_output("sen2",g=0)
setwd("D:/arnas/out/results_thin_multiplier2005/")
str3=readin_rhessys_output("sen2",g=0)
setwd("D:/arnas/out/results_thin_multiplier2006/")
str4=readin_rhessys_output("sen2",g=0)
setwd("D:/arnas/out/results_thin_multiplier2007/")
str5=readin_rhessys_output("sen2",g=0)

tmp.0=subset(str0$bd,str0$bd$wy==2003)
tmp.1=subset(str1$bd,str1$bd$wy==2004)
tmp.2=subset(str2$bd,str2$bd$wy==2005)
tmp.3=subset(str3$bd,str3$bd$wy==2006)
tmp.4=subset(str4$bd,str4$bd$wy==2007)
tmp.5=subset(str5$bd,str5$bd$wy==2008)
str.warm=NULL
str.warm[1]=sum(tmp.0$et)
str.warm[2]=sum(tmp.1$et)
str.warm[3]=sum(tmp.2$et)
str.warm[4]=sum(tmp.3$et)
str.warm[5]=sum(tmp.4$et)
str.warm[6]=sum(tmp.5$et)



setwd("D:/arnas/out/Totales/")
matrice=array(NA,dim = c(length(thin),4))
matrice[1:6,1]=str
matrice[1:6,2]=str.warm
matrice[,3]=thin
matrice[,4]=thin.warm
colnames(matrice)=c("base","base.warm","thin","thin.warm")
write.table(matrice,"boxplot.stat.eavp.csv",sep=";",dec=",",col.names = T,row.names = F)

boxplot(str,str.warm,thin,thin.warm)
