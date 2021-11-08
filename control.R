Detection<-function(herdDesc,simDay,PrevThreshold,NumDeaths){

  rowSums(prop.table(as.matrix(herdDesc[,c("S","E","I","R")]),1)[,3:4])->PrevalenceI
  detected<-which(PrevalenceI>=PrevThreshold & 
                    herdDesc$R>=NumDeaths & 
                    !herdDesc$isDetected)
  herdDesc$detectionDate[detected]=simDay
  herdDesc$status[detected]="D"
  herdDesc$I[detected]=0
  herdDesc$isDetected[detected]=TRUE
  herdDesc$col[detected]="black"
  return(herdDesc)
}  