library(ggplot2)
library(scales)

nse = function(m,o) {
  err = m-o
  ns = 1-var(err)/var(o)
  ns
}

lognse = function(m,o) {
  m = log(m+0.000001)
  o = log(o+0.000001)
  err = m-o
  ns = 1-var(err)/var(o)
  ns
}

mper.err = function(m,o) {
  tmp = (mean(m)-mean(o))/mean(o)*100
  #tmp = abs(tmp)this makes it show if under or over predicting
  tmp}

rmse=function(m,o){
  err = m-o
  rmse <- sqrt(mean(err^2))
  rmse
}

rsr=function(m,o){
  err = m-o
  rmse <- sqrt(mean(err^2))
  rsr <- rmse/sd(o)
  rsr
}

pbias=function(m,o){
  err2 <- o - m
  err2 <- sum(err2)
  ob.sum <- sum(o)
  pbias <- ((err2*100)/ob.sum)
  pbias
}

#####Daily Calibratio and daily Validation#########

setwd("D:/arnas/calibracion")

results = read.table("str101", skip=1)
for (i in 102:105){
  str=paste("str",i,sep="")
  tmp = read.table(str, skip=1)
  results = cbind(results, tmp)
}


results$date = seq.dates(from="10/01/2003", to="09/30/2008", by="days")
results.bak=results

stats.d = read.table("par101")
for (i in 102:105){
  par=paste("par",i,sep="")
  tmp = read.table(par)
  stats.d = rbind(stats.d, tmp)
}
colnames(stats.d) = c("m","k","po","pa","gw1","gw2")

###########CALCULAR LAS ESTADISTICAS PARA LA CALIBRACION###########################

#results=results.bak
results=subset(results,results$date>="01/01/2004" & results$date<="09/30/2008")


obs = read.table("D:/arnas/obs/streamarnas.txt", header=TRUE,sep="\t")
dt <- seq.dates(from="1/1/2000", to="9/30/2010", by="days")
obs$date=dt


tmp=subset(obs,obs$date>="01/01/2004" & obs$date <= "09/30/2008")
dim(tmp)
obs=tmp

for (i in 1:nrow(obs)){
  if((!is.na(obs[i,2]) && obs[i,2]>15)==T){
    obs[i,2]<-NA
  }
}
plot(x=obs$date,y=obs$mm,type="l")

ncal =ncol(results)-1
day3filter = rep(1/3,times=3)

lc = ncol(results)
lr = nrow(results)
tmp = apply(results[,1:(lc-1)],2,stats::filter, filter=day3filter, method="convolution", sides=2)
tmp2 = as.data.frame(tmp[2:(lr-1),])
tmp2$date = results[2:(lr-1),"date"]
result3d = tmp2
lr = nrow(obs)
tmp = stats::filter(obs$mm, filter=day3filter, method="convolution", sides=2)
tmp2 = as.data.frame(tmp[2:(lr-1)])
colnames(tmp2) = c("mm")
obs3d = tmp2
obs3d$date = obs$date[2:(lr-1)]
tmp = merge(result3d, obs3d, by=c("date"))
result3d = tmp
tmp=subset(result3d,!is.na(result3d$mm))
result3d=tmp

media=NULL

for (i in 1:nrow(result3d)) {
  media[i]=mean(as.numeric(result3d[i,2:10]),na.rm=T)
}

tmp=result3d$mm
result3d$mm=media
result3d$obs=tmp
colnames(result3d)=c("date","sim1","sim2","sim3","sim4","sim5","sim6","sim7","sim8","sim9","sim10","media","mm")


ac=round(nse(result3d$media,result3d$mm),digits = 2)
bc=round(rsr(result3d$media,result3d$mm),digits = 2)
cc=round(mper.err(result3d$media,result3d$mm),digits = 2)

###########CALCULAR LAS ESTADISTICAS PARA LA VALIDACION###########################

#results=results.bak
results=subset(results,results$date>="01/01/2007" & results$date<="09/30/2008")


obs = read.table("../obs/streamarnas.txt", header=TRUE,sep="\t")
dt <- seq.dates(from="1/1/2000", to="9/30/2010", by="days")
obs$date=dt


tmp=subset(obs,obs$date>="01/01/2007" & obs$date <= "09/30/2008")
dim(tmp)
obs=tmp

for (i in 1:nrow(obs)){
  if((!is.na(obs[i,2]) && obs[i,2]>15)==T){
    obs[i,2]<-NA
  }
}
plot(x=obs$date,y=obs$mm,type="l")

ncal =ncol(results)-1
day3filter = rep(1/3,times=3)

lc = ncol(results)
lr = nrow(results)
tmp = apply(results[,1:(lc-1)],2,stats::filter, filter=day3filter, method="convolution", sides=2)
tmp2 = as.data.frame(tmp[2:(lr-1),])
tmp2$date = results[2:(lr-1),"date"]
result3d = tmp2
lr = nrow(obs)
tmp = stats::filter(obs$mm, filter=day3filter, method="convolution", sides=2)
tmp2 = as.data.frame(tmp[2:(lr-1)])
colnames(tmp2) = c("mm")
obs3d = tmp2
obs3d$date = obs$date[2:(lr-1)]
tmp = merge(result3d, obs3d, by=c("date"))
result3d = tmp
tmp=subset(result3d,!is.na(result3d$mm))
result3d=tmp

media=NULL

for (i in 1:nrow(result3d)) {
  media[i]=mean(as.numeric(result3d[i,2:10]),na.rm=T)
}

tmp=result3d$mm
result3d$mm=media
result3d$obs=tmp
colnames(result3d)=c("date","sim1","sim2","sim3","sim4","sim5","sim6","sim7","sim8","sim9","sim10","media","mm")

av=round(nse(result3d$media,result3d$mm),digits = 2)
bv=round(rsr(result3d$media,result3d$mm),digits = 2)
cv=round(mper.err(result3d$media,result3d$mm),digits = 2)

############################## GRAFICO G1 ##############################

setwd("D:/arnas/out/caltrueno/allsim/")

results = read.table("str1", skip=1)
for (i in 2:100){
  str=paste("str",i,sep="")
  tmp = read.table(str, skip=1)
  results = cbind(results, tmp)
}


results$date = seq.dates(from="10/01/2000", to="09/30/2010", by="days")
results.bak=results

stats.d = read.table("par1")
for (i in 2:100){
  par=paste("par",i,sep="")
  tmp = read.table(par)
  stats.d = rbind(stats.d, tmp)
}
colnames(stats.d) = c("m","k","po","pa","gw1","gw2")

obs = read.table("../../../obs/streamarnas.txt", header=TRUE,sep="\t")
dt <- seq.dates(from="1/1/2000", to="9/30/2010", by="days")
obs$date=dt


tmp=subset(obs,obs$date>="01/01/2004" & obs$date <= "09/30/2008")
dim(tmp)
obs=tmp

for (i in 1:nrow(obs)){
  if((!is.na(obs[i,2]) && obs[i,2]>15)==T){
    obs[i,2]<-NA
  }
}

plot(x=obs$date,y=obs$mm,type="l")

stats3d=stats.d

ncal =ncol(results)-1
day3filter = rep(1/3,times=3)

lc = ncol(results)
lr = nrow(results)
tmp = apply(results[,1:(lc-1)],2,stats::filter, filter=day3filter, method="convolution", sides=2)
tmp2 = as.data.frame(tmp[2:(lr-1),])
tmp2$date = results[2:(lr-1),"date"]
result3d = tmp2
lr = nrow(obs)
tmp = stats::filter(obs$mm, filter=day3filter, method="convolution", sides=2)
tmp2 = as.data.frame(tmp[2:(lr-1)])
colnames(tmp2) = c("mm")
obs3d = tmp2
obs3d$date = obs$date[2:(lr-1)]
tmp = merge(result3d, obs3d, by=c("date"))
result3d = tmp

media=NULL

for (i in 1:nrow(result3d)) {
  media[i]=mean(as.numeric(result3d[i,2:11]),na.rm=T)
}

result3d$date= as.Date(result3d$date)
setwd("D:/arnas/calibracion")
export=cbind(result3d,media)
write.table(export,"datos_calibracion.csv",row.names = F,col.names = T,sep = ";",dec=",")
  
g1= ggplot(result3d,aes(x=result3d$date,y=result3d[,2]),color="grey")+geom_line(aes(x=result3d$date,y=result3d[,3]),color="grey")+geom_line(aes(x=result3d$date,y=result3d[,4]),color="grey")+ geom_line(aes(x=result3d$date,y=result3d[,5]),color="grey")+geom_line(aes(x=result3d$date,y=result3d[,6]),color="grey")+geom_line(aes(x=result3d$date,y=result3d[,7]),color="grey")+ geom_line(aes(x=result3d$date,y=result3d[,8]),color="grey")+geom_line(aes(x=result3d$date,y=result3d[,9]),color="grey")+geom_line(aes(x=result3d$date,y=result3d[,10]),color="grey")+ylim(0,12)+scale_x_date(labels = date_format("%m-%d-%Y"))
g1=g1+ geom_line(aes(x=result3d$date,y=result3d$mm),color="gray34")+theme_bw()
g1=g1+ geom_line(aes(x=result3d$date,y=media),color="red",linetype = "dashed")
g1=g1+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
g1=g1 + labs(x = "Date",y="Streamflow (mm)")
g1=g1+ geom_segment(aes(x = result3d$date[1], y = 0, xend = result3d$date[1], yend = 11),linetype = "dotdash",size=0.2, data = result3d) +geom_segment(aes(x = result3d$date[730], y = 0, xend = result3d$date[730], yend = 11),linetype = "dotdash",size=0.2,data = result3d)

g1=g1+ geom_segment(aes(x = result3d$date[1096], y = 0, xend = result3d$date[1096], yend = 11),linetype = "dotdash", size=0.2, data = result3d)+ geom_segment(aes(x = result3d$date[nrow(result3d)], y = 0, xend = result3d$date[nrow(result3d)], yend = 11),linetype = "dotdash",size=0.2, data = result3d)
g1=g1+ geom_text(x=result3d$date[350], y=11, label="Calibration",size=5,fontface=3)+ geom_text(x=result3d$date[1400], y=11, label="Validation",size=5,fontface=3)
g1=g1+ geom_text(x=result3d$date[930], y=7.5, label="Not considered",size=3,fontface=3)

g1=g1+ geom_text(x=result3d$date[150], y=10, label=paste("NSE = ",ac,sep=""),size=3)+ geom_text(x=result3d$date[150], y=9.5, label=paste("RSR = ",bc,sep=""),size=3)+ geom_text(x=result3d$date[150], y=9, label=paste("PBIAS = ",cc,"%",sep=""),size=3)
g1=g1+ geom_text(x=result3d$date[1250], y=10, label=paste("NSE = ",av,sep=""),size=3)+ geom_text(x=result3d$date[1250], y=9.5, label=paste("RSR = ",bv,sep=""),size=3)+ geom_text(x=result3d$date[1250], y=9, label=paste("PBIAS = ",cv,"%",sep=""),size=3)




setwd("D:/arnas/out/Totales/")
jpeg(file="Calibracion_Vaalidacion_diaria.jpeg",res = 300,quality = 300,width = 2500, height = 1500)
g1
dev.off()
