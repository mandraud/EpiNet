scen=NULL
for (i in 1:nrow(params)){
  scen=c(scen,paste("scen.",params[i,1],sep=""))
  load(paste0("./OutPut/output",params[i,1],".RData"))
  InfectedHerds<-lapply(Outputs, function(x) lapply(x,function(y) y[y$status %in% c("I","R","D"),]))
  save(InfectedHerds,file=paste0("./OutPut/InfectedHerds",as.numeric(params[i,1]),".RData"))
}
  

