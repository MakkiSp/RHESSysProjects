library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
#require(reshape2)
library(RColorBrewer)
#install.packages("ggpubr")
library(ggpubr)
library(Rmisc)

#funcion para dejar solo un decimal en los graficos
scaleFUN <- function(x) sprintf("%.1f", x)

first10year=function(a,b,c,d,e,an0,warm){
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
  dat=cbind(base,thin95$V2,thin75$V2,thin50$V2,thin25$V2)
  #xx=NULL
  #for (j in 1:10) {
  #  xx[j]=mean(dat[j])
  #}
  #kk=cbind(base,xx)
  if(warm==F){
  colnames(dat)=c("years","BG","CG95","CG75","CG50","CG25")}else{
  colnames(dat)=c("years","BGW","CGW95","CGW75","CGW50","CGW25")}
  as.data.frame(dat)
}

diff=function(data){
  for (i in 2:nrow(data)) {
    data[i,2]=data[i,2]-data[1,2]
  }
  as.data.frame(data[2:11,])
}

setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=first10year("sen3","sen19","sen7","sen11","sen15",2002,F)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=first10year("sen3","sen19","sen7","sen11","sen15",2003,F)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=first10year("sen3","sen19","sen7","sen11","sen15",2004,F)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=first10year("sen3","sen19","sen7","sen11","sen15",2005,F)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=first10year("sen3","sen19","sen7","sen11","sen15",2006,F)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=first10year("sen3","sen19","sen7","sen11","sen15",2007,F)

thin=rbind(thin02,thin03,thin04,thin05,thin06,thin07)
#datos <- melt(thin,id.vars="years", measure.vars=c("BG","CG95","CG75","CG50","CG25","BGW","CGW95","CGW75","CGW50","CGW25"))
#thin=arrange(datos,datos$years)

#tg=thin


#tgc <- summarySE(tg, measurevar="value", groupvars=c("years","variable"))


setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02.w=first10year("sen4","sen20","sen8","sen12","sen16",2002,T)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03.w=first10year("sen4","sen20","sen8","sen12","sen16",2003,T)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04.w=first10year("sen4","sen20","sen8","sen12","sen16",2004,T)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05.w=first10year("sen4","sen20","sen8","sen12","sen16",2005,T)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06.w=first10year("sen4","sen20","sen8","sen12","sen16",2006,T)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07.w=first10year("sen4","sen20","sen8","sen12","sen16",2007,T)

thin.w=rbind(thin02.w,thin03.w,thin04.w,thin05.w,thin06.w,thin07.w)
#datos <- melt(thin.w,id.vars="years", measure.vars=c("BGW","CGW95","CGW75","CGW50","CGW25"))
#thin.w=arrange(datos,datos$years)
#tg=thin.w


#tgc.w <- summarySE(tg, measurevar="value", groupvars=c("years","variable"))


#g1=ggplot()+geom_point(data=tgc, aes(x=tgc$years, y=tgc$value, colour=tgc$variable)) + ylim(-0.6,0.4)+
  #geom_errorbar(data=tgc,aes(x=tgc$years,ymin=tgc$value-tgc$se, ymax=tgc$value+tgc$se, colour=tgc$variable), width=.1) +geom_line(data=tgc,aes(x=tgc$years,y=tgc$value,colour=tgc$variable,linetype=tgc$variable),size=1) +
  #geom_point(data=tgc.w, aes(x=tgc.w$years, y=tgc.w$value, colour=tgc.w$variable)) +
  #geom_errorbar(data=tgc.w,aes(x=tgc.w$years,ymin=tgc.w$value-tgc.w$se, ymax=tgc.w$value+tgc.w$se, colour=tgc.w$variable), width=.1) +geom_line(data=tgc.w,aes(x=tgc.w$years,y=tgc.w$value,colour=tgc.w$variable,linetype=tgc.w$variable),size=1)+scale_linetype_manual(values=c("base"="solid", "base.w"="dashed", "thin"="solid", "thin.w"="dashed")) +
  #scale_color_manual(values=c( "base"="turquoise3", "base.w"="firebrick3", "thin"="turquoise3", "thin.w"="firebrick3"))
#g1=g1+labs(y="Average LAI Change",x="Years After Clearing")+scale_x_continuous(breaks=seq(1,10,1))

  
  
  data=cbind(thin,thin.w[,2:6])
  datos <- melt(data,id.vars="years", measure.vars=c("BG","CG95","CG75","CG50","CG25","BGW","CGW95","CGW75","CGW50","CGW25"))
  tg=arrange(datos,datos$years)
  tgc<- summarySE(tg, measurevar="value", groupvars=c("years","variable"))
  
a="BG"
b="BGW"
g1=ggplot(tgc,aes(x=tgc$years))+geom_point(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value, colour=variable),size = 1, stroke = 0, shape = 16)+ ylim(-0.6,0.4)+ geom_hline(yintercept=0,colour="grey60")+
  geom_errorbar(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,ymin=value-se, ymax=value+se, colour=variable), width=.1) +geom_line(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value,colour=variable))+
  labs(y="Average LAI Change",x="Years After Clearing")+scale_x_continuous(breaks=seq(1,10,1))+ theme(legend.title = element_blank(),legend.position = c(0.7, 0.2),legend.text = element_text(size = 10))+theme(axis.text=element_text(size=8),axis.title=element_text(size=8))
g1=g1+ scale_color_manual(values=c("#00BFC4","#F8766D"))

a="CG95"
b="CGW95"
g2=ggplot(tgc,aes(x=tgc$years))+geom_point(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value, colour=variable),size = 1, stroke = 0, shape = 16)+ ylim(-0.6,0.4)+ geom_hline(yintercept=0,colour="grey60")+
  geom_errorbar(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,ymin=value-se, ymax=value+se, colour=variable), width=.1) +geom_line(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value,colour=variable))+
  labs(y="Average LAI Change",x="Years After Clearing")+scale_x_continuous(breaks=seq(1,10,1))+ theme(legend.title = element_blank(),legend.position = c(0.7, 0.2),legend.text = element_text(size = 10))+theme(axis.text=element_text(size=8),axis.title=element_text(size=8))
g2=g2+ scale_color_manual(values=c("#00BFC4","#F8766D"))


a="CG75"
b="CGW75"
g3=ggplot(tgc,aes(x=tgc$years))+geom_point(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value, colour=variable),size = 1, stroke = 0, shape = 16)+ ylim(-0.6,0.4)+ geom_hline(yintercept=0,colour="grey60")+
  geom_errorbar(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,ymin=value-se, ymax=value+se, colour=variable), width=.1) +geom_line(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value,colour=variable))+
  labs(y="Average LAI Change",x="Years After Clearing")+scale_x_continuous(breaks=seq(1,10,1))+ theme(legend.title = element_blank(),legend.position = c(0.7, 0.2),legend.text = element_text(size = 10))+theme(axis.text=element_text(size=8),axis.title=element_text(size=8))
g3=g3+ scale_color_manual(values=c("#00BFC4","#F8766D"))


a="CG50"
b="CGW50"
g4=ggplot(tgc,aes(x=tgc$years))+geom_point(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value, colour=variable),size = 1, stroke = 0, shape = 16)+ ylim(-0.6,0.4)+ geom_hline(yintercept=0,colour="grey60")+
  geom_errorbar(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,ymin=value-se, ymax=value+se, colour=variable), width=.1) +geom_line(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value,colour=variable))+
  labs(y="Average LAI Change",x="Years After Clearing")+scale_x_continuous(breaks=seq(1,10,1))+ theme(legend.title = element_blank(),legend.position = c(0.7, 0.2),legend.text = element_text(size = 10))+theme(axis.text=element_text(size=8),axis.title=element_text(size=8))
g4=g4+ scale_color_manual(values=c("#00BFC4","#F8766D"))


a="CG25"
b="CGW25"
g5=ggplot(tgc,aes(x=tgc$years))+geom_point(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value, colour=variable),size = 1, stroke = 0, shape = 16)+ ylim(-0.6,0.4)+ geom_hline(yintercept=0,colour="grey60")+
  geom_errorbar(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,ymin=value-se, ymax=value+se, colour=variable), width=.1) +geom_line(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value,colour=variable))+
  labs(y="Average LAI Change",x="Years After Clearing")+scale_x_continuous(breaks=seq(1,10,1))+ theme(legend.title = element_blank(),legend.position = c(0.7, 0.2),legend.text = element_text(size = 10))+theme(axis.text=element_text(size=8),axis.title=element_text(size=8))
g5=g5+ scale_color_manual(values=c("#00BFC4","#F8766D"))


g1 <- ggplot_gtable(ggplot_build(g1))
g2 <- ggplot_gtable(ggplot_build(g2))
g3 <- ggplot_gtable(ggplot_build(g3))
g4 <- ggplot_gtable(ggplot_build(g4))
g5 <- ggplot_gtable(ggplot_build(g5))



maxWidth = grid::unit.pmax(g1$widths[2:5], g2$widths[2:5], g3$widths[2:5],g4$widths[2:5], g5$widths[2:5])

g1$widths[2:5] <- maxWidth
g2$widths[2:5] <- maxWidth
g3$widths[2:5] <- maxWidth
g4$widths[2:5] <- maxWidth
g5$widths[2:5] <- maxWidth

x1=grid.arrange(g1,g3,g5, ncol = 1, heights = c(3,3,3))
x2=grid.arrange(g2,g4, ncol = 1, heights = c(3,3,3))

graf=grid.arrange(x1,x2, ncol = 2)
setwd("D:/arnas/out/grafs/")
ggsave("lait_recovery.tiff", plot =graf,dpi = 1000, width = 8, height = 8,units = "in",compression="lzw")


first10year=function(a,b,c,d,e,an0,warm){
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
  dat=cbind(base,thin95$V2,thin75$V2,thin50$V2,thin25$V2)
  #xx=NULL
  #for (j in 1:10) {
  #  xx[j]=mean(dat[j])
  #}
  #kk=cbind(base,xx)
  if(warm==F){
    colnames(dat)=c("years","BG","CG95","CG75","CG50","CG25")}else{
      colnames(dat)=c("years","BGW","CGW95","CGW75","CGW50","CGW25")}
  as.data.frame(dat)
}

setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02=first10year("sen3","sen19","sen7","sen11","sen15",2002,F)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03=first10year("sen3","sen19","sen7","sen11","sen15",2003,F)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04=first10year("sen3","sen19","sen7","sen11","sen15",2004,F)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05=first10year("sen3","sen19","sen7","sen11","sen15",2005,F)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06=first10year("sen3","sen19","sen7","sen11","sen15",2006,F)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07=first10year("sen3","sen19","sen7","sen11","sen15",2007,F)

thin=rbind(thin02,thin03,thin04,thin05,thin06,thin07)

setwd("D:/arnas/out/results_thin_multiplier2002/")
thin02.w=first10year("sen4","sen20","sen8","sen12","sen16",2002,T)
setwd("D:/arnas/out/results_thin_multiplier2003/")
thin03.w=first10year("sen4","sen20","sen8","sen12","sen16",2003,T)
setwd("D:/arnas/out/results_thin_multiplier2004/")
thin04.w=first10year("sen4","sen20","sen8","sen12","sen16",2004,T)
setwd("D:/arnas/out/results_thin_multiplier2005/")
thin05.w=first10year("sen4","sen20","sen8","sen12","sen16",2005,T)
setwd("D:/arnas/out/results_thin_multiplier2006/")
thin06.w=first10year("sen4","sen20","sen8","sen12","sen16",2006,T)
setwd("D:/arnas/out/results_thin_multiplier2007/")
thin07.w=first10year("sen4","sen20","sen8","sen12","sen16",2007,T)

thin.w=rbind(thin02.w,thin03.w,thin04.w,thin05.w,thin06.w,thin07.w)

data=cbind(thin,thin.w[,2:6])
datos <- melt(data,id.vars="years", measure.vars=c("BG","CG95","CG75","CG50","CG25","BGW","CGW95","CGW75","CGW50","CGW25"))
tg=arrange(datos,datos$years)
tgc<- summarySE(tg, measurevar="value", groupvars=c("years","variable"))

a="BG"
b="BGW"
g1=ggplot(tgc,aes(x=tgc$years))+geom_point(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value, colour=variable),size = 1, stroke = 0, shape = 16)+ ylim(-0.8,1)+ geom_hline(yintercept=0,colour="grey60")+
  geom_errorbar(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,ymin=value-se, ymax=value+se, colour=variable), width=.1) +geom_line(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value,colour=variable))+
  labs(y="Average Plant Carbon Change KgC/m2",x="Years After Clearing")+scale_x_continuous(breaks=seq(1,10,1))+ theme(legend.title = element_blank(),legend.position = c(0.7, 0.2),legend.text = element_text(size = 10))+theme(axis.text=element_text(size=8),axis.title=element_text(size=8))
g1=g1+ scale_color_manual(values=c("#00BFC4","#F8766D"))


a="CG95"
b="CGW95"
g2=ggplot(tgc,aes(x=tgc$years))+geom_point(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value, colour=variable),size = 1, stroke = 0, shape = 16)+ ylim(-0.8,1)+ geom_hline(yintercept=0,colour="grey60")+
  geom_errorbar(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,ymin=value-se, ymax=value+se, colour=variable), width=.1) +geom_line(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value,colour=variable))+
  labs(y="Average Plant Carbon Change KgC/m2",x="Years After Clearing")+scale_x_continuous(breaks=seq(1,10,1))+ theme(legend.title = element_blank(),legend.position = c(0.7, 0.2),legend.text = element_text(size = 10))+theme(axis.text=element_text(size=8),axis.title=element_text(size=8))
g2=g2+ scale_color_manual(values=c("#00BFC4","#F8766D"))


a="CG75"
b="CGW75"
g3=ggplot(tgc,aes(x=tgc$years))+geom_point(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value, colour=variable),size = 1, stroke = 0, shape = 16)+ ylim(-0.8,1)+ geom_hline(yintercept=0,colour="grey60")+
  geom_errorbar(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,ymin=value-se, ymax=value+se, colour=variable), width=.1) +geom_line(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value,colour=variable))+
  labs(y="Average Plant Carbon Change KgC/m2",x="Years After Clearing")+scale_x_continuous(breaks=seq(1,10,1))+ theme(legend.title = element_blank(),legend.position = c(0.7, 0.2),legend.text = element_text(size = 10))+theme(axis.text=element_text(size=8),axis.title=element_text(size=8))
g3=g3+ scale_color_manual(values=c("#00BFC4","#F8766D"))


a="CG50"
b="CGW50"
g4=ggplot(tgc,aes(x=tgc$years))+geom_point(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value, colour=variable),size = 1, stroke = 0, shape = 16)+ ylim(-0.8,1)+ geom_hline(yintercept=0,colour="grey60")+
  geom_errorbar(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,ymin=value-se, ymax=value+se, colour=variable), width=.1) +geom_line(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value,colour=variable))+
  labs(y="Average Plant Carbon Change KgC/m2",x="Years After Clearing")+scale_x_continuous(breaks=seq(1,10,1))+ theme(legend.title = element_blank(),legend.position = c(0.7, 0.2),legend.text = element_text(size = 10))+theme(axis.text=element_text(size=8),axis.title=element_text(size=8))
g4=g4+ scale_color_manual(values=c("#00BFC4","#F8766D"))


a="CG25"
b="CGW25"
g5=ggplot(tgc,aes(x=tgc$years))+geom_point(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value, colour=variable),size = 1, stroke = 0, shape = 16)+ ylim(-0.8,1)+ geom_hline(yintercept=0,colour="grey60")+
  geom_errorbar(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,ymin=value-se, ymax=value+se, colour=variable), width=.1) +geom_line(data=subset(tgc,tgc$variable==a| tgc$variable==b),aes(x=years,y=value,colour=variable))+
  labs(y="Average Plant Carbon Change KgC/m2",x="Years After Clearing")+scale_x_continuous(breaks=seq(1,10,1))+ theme(legend.title = element_blank(),legend.position = c(0.7, 0.2),legend.text = element_text(size = 10))+theme(axis.text=element_text(size=8),axis.title=element_text(size=8))
g5=g5+ scale_color_manual(values=c("#00BFC4","#F8766D"))


g1 <- ggplot_gtable(ggplot_build(g1))
g2 <- ggplot_gtable(ggplot_build(g2))
g3 <- ggplot_gtable(ggplot_build(g3))
g4 <- ggplot_gtable(ggplot_build(g4))
g5 <- ggplot_gtable(ggplot_build(g5))



maxWidth = grid::unit.pmax(g1$widths[2:5], g2$widths[2:5], g3$widths[2:5],g4$widths[2:5], g5$widths[2:5])

g1$widths[2:5] <- maxWidth
g2$widths[2:5] <- maxWidth
g3$widths[2:5] <- maxWidth
g4$widths[2:5] <- maxWidth
g5$widths[2:5] <- maxWidth

x1=grid.arrange(g1,g3,g5, ncol = 1, heights = c(3,3,3))
x2=grid.arrange(g2,g4, ncol = 1, heights = c(3,3,3))

graf=grid.arrange(x1,x2, ncol = 2)
setwd("D:/arnas/out/grafs/")
ggsave("PLANTC_recovery.tiff", plot =graf,dpi = 1000, width = 8, height = 8,units = "in",compression="lzw")
