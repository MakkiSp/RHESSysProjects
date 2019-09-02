library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
#require(reshape2)
library(RColorBrewer)
#install.packages("ggpubr")
library(ggpubr)


#funcion para dejar solo un decimal en los graficos
scaleFUN <- function(x) sprintf("%.1f", x)

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
  #pcp=aggregate(tmp.5$precip,by=list(tmp.5$wy),FUN=sum)
  years=seq(0,10,by=1)
  base=diff(cbind(years,stream1$x))
  thin95=diff(cbind(years,stream2$x))
  thin75=diff(cbind(years,stream3$x))
  thin50=diff(cbind(years,stream4$x))
  thin25=diff(cbind(years,stream5$x))
  list(base,thin95,thin75,thin50,thin25)
}

diff=function(data){
  for (i in 2:nrow(data)) {
    data[i,2]=data[i,2]-data[1,2]
  }
  data[2:11,]
}

setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=first10year("sen3","sen19","sen7","sen11","sen15",2002)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=first10year("sen3","sen19","sen7","sen11","sen15",2003)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=first10year("sen3","sen19","sen7","sen11","sen15",2004)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=first10year("sen3","sen19","sen7","sen11","sen15",2005)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=first10year("sen3","sen19","sen7","sen11","sen15",2006)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=first10year("sen3","sen19","sen7","sen11","sen15",2007)


base=rbind(as.data.frame(thin02[1]),as.data.frame(thin03[1]),as.data.frame(thin04[1]),as.data.frame(thin05[1]),as.data.frame(thin06[1]),as.data.frame(thin07[1]))
thin95=rbind(as.data.frame(thin02[2]),as.data.frame(thin03[2]),as.data.frame(thin04[2]),as.data.frame(thin05[2]),as.data.frame(thin06[2]),as.data.frame(thin07[2]))
thin75=rbind(as.data.frame(thin02[3]),as.data.frame(thin03[3]),as.data.frame(thin04[3]),as.data.frame(thin05[3]),as.data.frame(thin06[3]),as.data.frame(thin07[3]))
thin50=rbind(as.data.frame(thin02[4]),as.data.frame(thin03[4]),as.data.frame(thin04[4]),as.data.frame(thin05[4]),as.data.frame(thin06[4]),as.data.frame(thin07[4]))
thin25=rbind(as.data.frame(thin02[5]),as.data.frame(thin03[5]),as.data.frame(thin04[5]),as.data.frame(thin05[5]),as.data.frame(thin06[5]),as.data.frame(thin07[5]))
thin=cbind(base,thin25$V2,thin50$V2,thin75$V2,thin95$V2)

#escalacolores=colorRampPalette(rev(brewer.pal(4,"YlOrRd")))
#colfunc <- colorRampPalette(c("blue", "orange"))
#colfunc(14)

colnames(thin)=c("years","BG","CG25","CG50","CG75","CG95")
dat.m <- melt(thin,id.vars="years", measure.vars=c("BG","CG25","CG50","CG75","CG95"))
thin=arrange(dat.m,dat.m$years)




g1=ggplot(data=thin,aes(x=as.factor(thin$years),y=value,fill=thin$variable))+geom_boxplot(outlier.size = -1,position=position_dodge(width = 0.8),fatten = 0.75)+ theme_bw()+theme(axis.text=element_text(size=10),axis.title=element_text(size=10))
g1=g1+ scale_fill_manual(values=c("#00BFFF", "#FFFFB2", "#FECC5C", "#FD8D3C", "#E31A1C"))
g1=g1+labs(y="Average Plant Carbon Change KgC/m2",x="Years After Clearing")+scale_y_continuous(breaks=seq(-0.8,0.9,0.4),limits = c(-0.8,0.9))
g1=g1+ theme(legend.position="top",legend.title = element_blank())+ geom_hline(yintercept=0)+annotate("text", x=1.5, y=0.8, label= "1.A",size = 3)


#multiplot(g1,g1,g1,g1, cols=2)


#########################################warm
setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=first10year("sen4","sen20","sen8","sen12","sen16",2002)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=first10year("sen4","sen20","sen8","sen12","sen16",2003)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=first10year("sen4","sen20","sen8","sen12","sen16",2004)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=first10year("sen4","sen20","sen8","sen12","sen16",2005)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=first10year("sen4","sen20","sen8","sen12","sen16",2006)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=first10year("sen4","sen20","sen8","sen12","sen16",2007)


base=rbind(as.data.frame(thin02[1]),as.data.frame(thin03[1]),as.data.frame(thin04[1]),as.data.frame(thin05[1]),as.data.frame(thin06[1]),as.data.frame(thin07[1]))
thin95=rbind(as.data.frame(thin02[2]),as.data.frame(thin03[2]),as.data.frame(thin04[2]),as.data.frame(thin05[2]),as.data.frame(thin06[2]),as.data.frame(thin07[2]))
thin75=rbind(as.data.frame(thin02[3]),as.data.frame(thin03[3]),as.data.frame(thin04[3]),as.data.frame(thin05[3]),as.data.frame(thin06[3]),as.data.frame(thin07[3]))
thin50=rbind(as.data.frame(thin02[4]),as.data.frame(thin03[4]),as.data.frame(thin04[4]),as.data.frame(thin05[4]),as.data.frame(thin06[4]),as.data.frame(thin07[4]))
thin25=rbind(as.data.frame(thin02[5]),as.data.frame(thin03[5]),as.data.frame(thin04[5]),as.data.frame(thin05[5]),as.data.frame(thin06[5]),as.data.frame(thin07[5]))
thinw=cbind(base,thin25$V2,thin50$V2,thin75$V2,thin95$V2)

colnames(thinw)=c("years","BGW","CGW25","CGW50","CGW75","CGW95")
dat.m <- melt(thinw,id.vars="years", measure.vars=c("BGW","CGW25","CGW50","CGW75","CGW95"))
thinw=arrange(dat.m,dat.m$years)


g2=ggplot(data=thinw,aes(x=as.factor(thinw$years),y=value,fill=thinw$variable))+geom_boxplot(outlier.size = -1,position=position_dodge(width = 0.8),fatten = 0.75)+ theme_bw()+theme(axis.text=element_text(size=10),axis.title=element_text(size=10))
g2=g2+ scale_fill_manual(values=c("#00BFFF", "#FFFFB2", "#FECC5C", "#FD8D3C", "#E31A1C"))
g2=g2+labs(y="Average Plant Carbon Change KgC/m2",x="Years After Clearing")+scale_y_continuous(breaks=seq(-0.8,0.9,0.4),limits = c(-0.8,0.9))
g2=g2+ theme(legend.title = element_blank(),legend.position="top")+ geom_hline(yintercept=0)+annotate("text", x=1.5, y=0.8, label= "1.B",size = 3)


###################################################################

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
  stream1=aggregate(tmp.1$lai,by=list(tmp.1$wy),FUN=mean)
  stream2=aggregate(tmp.2$lai,by=list(tmp.2$wy),FUN=mean)
  stream3=aggregate(tmp.3$lai,by=list(tmp.3$wy),FUN=mean)
  stream4=aggregate(tmp.4$lai,by=list(tmp.4$wy),FUN=mean)
  stream5=aggregate(tmp.5$lai,by=list(tmp.5$wy),FUN=mean)
  #pcp=aggregate(tmp.5$precip,by=list(tmp.5$wy),FUN=sum)
  years=seq(0,10,by=1)
  base=diff(cbind(years,stream1$x))
  thin95=diff(cbind(years,stream2$x))
  thin75=diff(cbind(years,stream3$x))
  thin50=diff(cbind(years,stream4$x))
  thin25=diff(cbind(years,stream5$x))
  list(base,thin95,thin75,thin50,thin25)
}

diff=function(data){
  for (i in 2:nrow(data)) {
    data[i,2]=data[i,2]-data[1,2]
  }
  data[2:11,]
}

setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=first10year("sen3","sen19","sen7","sen11","sen15",2002)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=first10year("sen3","sen19","sen7","sen11","sen15",2003)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=first10year("sen3","sen19","sen7","sen11","sen15",2004)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=first10year("sen3","sen19","sen7","sen11","sen15",2005)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=first10year("sen3","sen19","sen7","sen11","sen15",2006)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=first10year("sen3","sen19","sen7","sen11","sen15",2007)


base=rbind(as.data.frame(thin02[1]),as.data.frame(thin03[1]),as.data.frame(thin04[1]),as.data.frame(thin05[1]),as.data.frame(thin06[1]),as.data.frame(thin07[1]))
thin95=rbind(as.data.frame(thin02[2]),as.data.frame(thin03[2]),as.data.frame(thin04[2]),as.data.frame(thin05[2]),as.data.frame(thin06[2]),as.data.frame(thin07[2]))
thin75=rbind(as.data.frame(thin02[3]),as.data.frame(thin03[3]),as.data.frame(thin04[3]),as.data.frame(thin05[3]),as.data.frame(thin06[3]),as.data.frame(thin07[3]))
thin50=rbind(as.data.frame(thin02[4]),as.data.frame(thin03[4]),as.data.frame(thin04[4]),as.data.frame(thin05[4]),as.data.frame(thin06[4]),as.data.frame(thin07[4]))
thin25=rbind(as.data.frame(thin02[5]),as.data.frame(thin03[5]),as.data.frame(thin04[5]),as.data.frame(thin05[5]),as.data.frame(thin06[5]),as.data.frame(thin07[5]))
thin=cbind(base,thin25$V2,thin50$V2,thin75$V2,thin95$V2)

#escalacolores=colorRampPalette(rev(brewer.pal(4,"YlOrRd")))
#colfunc <- colorRampPalette(c("deepskyblue1", "orange"))
#colfunc(14)

colnames(thin)=c("years","BG","CG25","CG50","CG75","CG95")
dat.m <- melt(thin,id.vars="years", measure.vars=c("BG","CG25","CG50","CG75","CG95"))
thin=arrange(dat.m,dat.m$years)




g3=ggplot(data=thin,aes(x=as.factor(thin$years),y=value,fill=thin$variable))+geom_boxplot(outlier.size = -1,position=position_dodge(width = 0.8),fatten = 0.75)+ theme_bw()+theme(axis.text=element_text(size=10),axis.title=element_text(size=10))
g3=g3+ scale_fill_manual(values=c("#00BFFF", "#FFFFB2", "#FECC5C", "#FD8D3C", "#E31A1C"))
g3=g3+labs(y="Average LAI Change",x="Years After Clearing")+scale_y_continuous(breaks=seq(-0.6,0.4,0.2),limits = c(-0.6,0.4),labels=scaleFUN)
g3=g3+ theme(legend.position="none",legend.title = element_blank())+ geom_hline(yintercept= 0)+annotate("text", x=1.5, y=0.35, label= "2.A",size = 3)






#########################################warm
setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=first10year("sen4","sen20","sen8","sen12","sen16",2002)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=first10year("sen4","sen20","sen8","sen12","sen16",2003)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=first10year("sen4","sen20","sen8","sen12","sen16",2004)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=first10year("sen4","sen20","sen8","sen12","sen16",2005)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=first10year("sen4","sen20","sen8","sen12","sen16",2006)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=first10year("sen4","sen20","sen8","sen12","sen16",2007)


base=rbind(as.data.frame(thin02[1]),as.data.frame(thin03[1]),as.data.frame(thin04[1]),as.data.frame(thin05[1]),as.data.frame(thin06[1]),as.data.frame(thin07[1]))
thin95=rbind(as.data.frame(thin02[2]),as.data.frame(thin03[2]),as.data.frame(thin04[2]),as.data.frame(thin05[2]),as.data.frame(thin06[2]),as.data.frame(thin07[2]))
thin75=rbind(as.data.frame(thin02[3]),as.data.frame(thin03[3]),as.data.frame(thin04[3]),as.data.frame(thin05[3]),as.data.frame(thin06[3]),as.data.frame(thin07[3]))
thin50=rbind(as.data.frame(thin02[4]),as.data.frame(thin03[4]),as.data.frame(thin04[4]),as.data.frame(thin05[4]),as.data.frame(thin06[4]),as.data.frame(thin07[4]))
thin25=rbind(as.data.frame(thin02[5]),as.data.frame(thin03[5]),as.data.frame(thin04[5]),as.data.frame(thin05[5]),as.data.frame(thin06[5]),as.data.frame(thin07[5]))
thinw=cbind(base,thin25$V2,thin50$V2,thin75$V2,thin95$V2)

colnames(thinw)=c("years","BGW","CGW25","CGW50","CGW75","CGW95")
dat.m <- melt(thinw,id.vars="years", measure.vars=c("BGW","CGW25","CGW50","CGW75","CGW95"))
thinw=arrange(dat.m,dat.m$years)


g4=ggplot(data=thinw,aes(x=as.factor(thinw$years),y=value,fill=thinw$variable))+geom_boxplot(outlier.size = -1,position=position_dodge(width = 0.8),fatten = 0.75)+ theme_bw()+theme(axis.text=element_text(size=10),axis.title=element_text(size=10))
g4=g4+ scale_fill_manual(values=c("#00BFFF", "#FFFFB2", "#FECC5C", "#FD8D3C", "#E31A1C"))
g4=g4+labs(y="Average LAI Change",x="Years After Clearing", size=5)+scale_y_continuous(breaks=seq(-0.6,0.4,0.2),limits = c(-0.6,0.4),labels=scaleFUN)
g4=g4+ theme(legend.position="none",legend.title = element_blank())+ geom_hline(yintercept=0)+annotate("text", x=1.5, y=0.35, label= "2.B",size = 3)

g1 <- ggplot_gtable(ggplot_build(g1))
g2 <- ggplot_gtable(ggplot_build(g2))
g3 <- ggplot_gtable(ggplot_build(g3))
g4 <- ggplot_gtable(ggplot_build(g4))


maxWidth = grid::unit.pmax( g1$widths[2:5],g2$widths[2:5], g3$widths[2:5],g4$widths[2:5])

g1$widths[2:5] <- maxWidth
g2$widths[2:5] <- maxWidth
g3$widths[2:5] <- maxWidth
g4$widths[2:5] <- maxWidth


x1=grid.arrange(g1,g3, ncol = 1, heights = c(3.5,3))
x2=grid.arrange(g2,g4, ncol = 1, heights = c(3.5,3))

graf=grid.arrange(x1,x2, ncol = 2)

setwd("D:/arnas/out/grafs/")
ggsave("boxplots_LAIPLANTC_recovery.tiff", plot =graf,dpi = 700, width = 8, height = 8,units = "in",compression="lzw")




jpeg(file="test2.jpeg",res = 600,width = 8, height = 8,units = "in")
graf
dev.off()


ggsave("foto.jpeg", plot =graf,dpi = 200, width = 8, height = 8,units = "in")
