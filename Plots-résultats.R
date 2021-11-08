
library(rgdal)
library(plyr)
library(ggplot2)
library(survival)
library(survminer)
library(grid)
library(gridExtra)
library(cowplot)
source("ReadAndFormatData.R")
source("CalcDistances.R")
FullData2=NULL

params<-read.delim("Scenarios.csv",header=TRUE,sep=";")
readAndFormat(FileMovements = "./Data/MovementDataFrame.RData",
              FileTournees = "./Data/Tournees.RData",
              fileHerdDesc = "./Data/herdDesc.RData")
herdDesc=data[[3]]

HD=herdDesc
coordinates(HD)<-~Lon+Lat
proj4string(HD)=CRS("+init=epsg:4326")
HD<-spTransform(HD,CRS("+proj=utm +zone=30"))
HT<-levels(herdDesc$site)
typo=HT[1:3]
res=NULL
scene=c(1,4,10,13)
scen=NULL
for (k in 1:4){
  i=scene[k]  
  scen2=paste("scen.",k,sep="")

  k=i
  scen=c(scen,paste("scen.",k,sep=""))
  load(paste0("./OutPut/InfectedHerds",params[i,1],".RData"))
  InfectedHerds=lapply(InfectedHerds,function(x) lapply(x, function(y) {
      
            y$dayEnd=min(as.numeric(y$infDate),na.rm=TRUE)+40
            return(y)}))

Events<-lapply(InfectedHerds, function(x) do.call(c,lapply(x,nrow)))
nTransmissionEvents<-do.call(c,lapply(Events,function(x) sum(x>1)))

a1=round(do.call(rbind,lapply(Events,function(x) quantile(x[x>1]-1,probs=c(0.5,0.025,0.975)))))
Infected=apply(a1,1,function(x) (paste(x[1]," [",x[2],";",x[3],"]",sep="")))


dataByType<-lapply(InfectedHerds,function(x) {do.call(rbind,x)})


FullData=NULL
for (j in 1:length(dataByType)){
  FullData<-rbind(FullData,data.frame(IndexType=typo[j],dataByType[[j]]))
}
factor(FullData$IndexType)->FullData$IndexType
factor(FullData$IndexType,sort(levels(FullData$IndexType)))->FullData$IndexType

RouteInfData<-FullData[-which(FullData$routeInf=="None"),]
RouteInfData$routeInf[RouteInfData$routeInf=="local"]<-"Local"
pie<-ggplot(data=RouteInfData, aes(x = "", fill = routeInf)) +
  geom_bar(position = "fill", width = 1) + 
  coord_polar(theta = "y") + 
  xlab("") + 
  ylab("IndexType") + labs(fill = "") +
  facet_grid(. ~ IndexType,) + ylab("Index farm category")+
  theme(legend.position = "top",panel.grid = element_blank(), axis.ticks = element_blank(), axis.text.x=element_blank(), panel.border = element_blank())



SecInfData<-FullData[-which(is.na(FullData$source)),]

# pie2<-ggplot(data=SecInfData, aes(x = "", fill = site)) +
#   geom_bar(position = "fill", width = 1) + 
#   coord_polar(theta = "y") + 
#   xlab("") + 
#   ylab("IndexType") + labs(fill = "Transmission\nroute") +
#   facet_grid(. ~ IndexType) + ylab("Type of the index herd")+
#   theme(panel.grid = element_blank(), axis.ticks = element_blank(), axis.text.x=element_blank(), panel.border = element_blank())


RouteInf<-data.frame(do.call(rbind,lapply(dataByType,
                                          function(x) {
                                            prop.table(table(factor(x$routeInf,
                                                                    levels=c("Direct","local","Transit"))))
                                            }
)))

RouteInf<-round(as.matrix(RouteInf),2)
colnames(RouteInf)=c("Direct","Local","Transit")
assign(paste('RouteInf',k),RouteInf)


dist=lapply(dataByType,calcDistance,HD)
FinalDist=NULL
for (j in 1:length(dist)){
  FinalDist<-rbind(FinalDist,data.frame(IndexType=typo[j],Transmission=nTransmissionEvents[j],distance=as.numeric(dist[[j]])))
}

cbbPalette <- c( "#CC79A7","#F0E442", "#E69F00",  "#0072B2", "#D55E00", "#CC79A7")
FinalDist$Transmission=as.factor(FinalDist$Transmission)
factor(FinalDist$IndexType,sort(levels(factor(FinalDist$IndexType))))->FinalDist$IndexType

p <- ggplot(FinalDist, aes(x=IndexType, y=distance,fill=IndexType)) + 
  geom_boxplot(outlier.shape = NA,notch=FALSE)+
  scale_fill_discrete(type=cbbPalette) +
  ylim(c(0,250))+xlab("Index farm category")+
  ylab("Distance (km)")+
  theme(legend.position = "none")

nEvents=NULL
for (j in 1:length(Events)){
  nEvents<-rbind(nEvents,data.frame(IndexType=typo[j],Transmission=nTransmissionEvents[j],Events=as.numeric(Events[[j]])))
}
nEvents$Transmission=as.factor(nEvents$Transmission)
quantile(nEvents$Events)

factor(nEvents$IndexType)->nEvents$IndexType
levels(nEvents$IndexType)=c("Farrowers","Finishers","Breeders")
factor(nEvents$IndexType,sort(levels(nEvents$IndexType)))->nEvents$IndexType

yMax=round_any(max(by(nEvents$Events,nEvents$IndexType,quantile,probs=0.95)), 10, f = ceiling)
p2 <- ggplot(nEvents, aes(x=IndexType, y=Events,fill=IndexType)) + 
  geom_boxplot(notch = FALSE,outlier.shape = NA)+
  scale_fill_discrete(type=cbbPalette)+
  ylim(c(0,yMax))+xlab("Index farm category")+
  ylab("Number of infected herds")+
  theme(legend.position = "none")


  labs(fill = "Simulations with \ntransmission (/1000)")
nEvents$Ev=as.numeric(nEvents$Events>1)
p3 <- ggplot(nEvents, aes(x=factor(IndexType),y=as.numeric(as.character(Ev)),fill=IndexType)) +
geom_bar(stat="identity")+
scale_fill_discrete(type=cbbPalette)+
xlab("Index farm category")+ylab("Number of simulations\n with secondary transmission (/1000))")+
  theme(legend.position = "none")

a<-lapply(dataByType,function(x) split(x,factor(x$routeInf,levels=c("Direct","local","Transit"))))

b=lapply(a, function(b) 
  lapply(b,function(x) quantile(calcDistance(x,HD),probs=c(0.5,0.05,0.95))))

z<-lapply(b, function(x) round(do.call(c,x),1))
TransDistance<-do.call(rbind,lapply(z,function(x) 
  c(paste(x[1]," [",x[2],";",x[3],"]",sep=""),
    paste(x[4]," [",x[5],";",x[6],"]",sep=""),
    paste(x[7]," [",x[8],";",x[9],"]",sep=""))))
colnames(TransDistance)=c("Direct","Local","Transit")
TransDistance



FullData$site=factor(FullData$site)
TimeD<-lapply(dataByType,function(x) x[which(x$isDetected),])
prop.table(table(FullData$status,FullData$IndexType))
FullData$DelayD<- as.Date(FullData$detectionDate,origin = "1970-01-01")-FullData$infDate
FullData$DelayD[FullData$status=="I"]<-as.Date(FullData$dayEnd[FullData$status=="I"],origin = "1970-01-01")-FullData$infDate[FullData$status=="I"]
S=Surv(FullData$DelayD[!is.na(FullData$DelayD)],!(FullData$status[!is.na(FullData$DelayD)]=="I"))
FullData$site

survPlot<-ggsurvplot(a<-survfit(Surv(DelayD,as.numeric(isDetected)) ~ site,
                   data=FullData[!is.na(FullData$DelayD),]), fun="event",
           FullData,
           palette = cbbPalette,  
           cumevents=TRUE,#risk.table = "nrisk_cumevents",
           legend.labs=levels(FullData$site),
           title=paste("Survival analysis of the detection delay"),
           legend.title="",legend=c(0.8,0.25),xlab="Detection time (days)",
           ylab="Detection probability",
           risk.table.y.text = FALSE,pval = TRUE,
           cumevents.y.text=FALSE,surv.median.line = "hv",#ncensor.plot=TRUE,  
           cumevents.title = "Cumulative number of detection events \n(3000 simulations)")

# a0=do.call(rbind,lapply(lapply(DelayD,function(x) quantile(x,probs=c(0.5,0.05,0.95),na.rm=TRUE)),round))
# Detect=apply(a0,1,function(x) (paste(x[1],"[",x[2],";",x[3],"]",sep="")))
FullData$Scenario=scen2
FullData2<-rbind(FullData2,FullData)
if (k==1){
  a=grid.arrange(
    grobs = list(pie,p,p2,p3),
    widths = c(1.5, 2),
    layout_matrix = rbind(c(4, 3),c(2,1)),gp=gpar(fontsize=20,font=3))
  
  
  finalPlot <- as_ggplot(a) +                                # transform to a ggplot
    draw_plot_label(label = c("A", "B", "C","D"), size = 15,
                    x = c(0, 0.43, 0,0.43), y = c(1, 1, 0.5,0.5))

  }else{
  a=grid.arrange(
    grobs = list(pie,p,p2,p3,survPlot$plot),
    widths = c(1.5, 2,4),
    layout_matrix = rbind(c(4, 3,5),c(2,1,5)),gp=gpar(fontsize=20,font=3))
  
  
  finalPlot <- as_ggplot(a) +                                # transform to a ggplot
    draw_plot_label(label = c("A", "B", "C","D","E"), size = 15,
                    x = c(0, 0.2, 0,0.2,0.5), y = c(1, 1, 0.5,0.5,1))
  
}


print(finalPlot)

#
if (!params[i,2]) Detect=rep("-",3)
res=rbind(res,
          data.frame(typo,nTransmissionEvents,
                     Infected,Detect,RouteInf,TransDistance))
}



survPlot<-ggsurvplot(a<-survfit(Surv(DelayD,as.numeric(isDetected)) ~ Scenario,
data=FullData2[!is.na(FullData2$DelayD),]), fun="event",
FullData2,
palette = cbbPalette[c(3,2,1,4)],
cumevents=TRUE,#risk.table = "nrisk_cumevents",
legend.labs=levels(FullData2$Scenario),
title=paste("Survival analysis of the detection delay"),
legend.title="",legend=c(0.8,0.25),xlab="Detection time (days)",
ylab="Detection probability",
risk.table.y.text = FALSE,pval = TRUE,
cumevents.y.text=FALSE,surv.median.line = "hv",#ncensor.plot=TRUE,
cumevents.title = "Cumulative number of detection events \n(3000 simulations)")
survPlot

