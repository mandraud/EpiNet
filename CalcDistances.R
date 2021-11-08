library(rgeos)
calcDistance<-function(a,HD){
  a<-a[!is.na(a$source),c("lieu","source")]


return(apply(a,1, function(x) 
  round(gDistance(HD[HD$lieu==as.numeric(x[1]),],
            HD[HD$lieu==x[2],])/1000,
  2)))}
