corr <- function(directory, threshold = 0) {
  setwd(directory)
  fls <- list.files(pattern = "*.csv")
  
  files <- lapply(fls, read.csv, header=T)
  numberOfFiles<-length(files)
  good <- lapply(files, complete.cases)
  cleanDataSets<-lapply(seq_along(fls),function(i) files[[i]][good[[i]],])
  a<-lapply(good,table)
  #numberOfCompleteCases<-lapply(seq_along(a) ,function(x)a[[length(list.files())]][2])
  #return(cor(cleanDataSets[[1]]$sulfate,cleanDataSets[[1]]$nitrate))
  setwd("..")
  goodCases<-vector(length=length(length(files)))
  corValues<-vector()
  for (i in 1:numberOfFiles){
    goodCases[i]=dim(files[[i]][good[[i]],])[1]
  }
  #   goodCases<-lapply(seq_along(files),function(x) dim(files[[x]][good[[x]],])[1])
  #   goodCases<-sapply(goodCases,"[",c(1:322))
  #   goodCases<-goodCases[1,]
  goodCases[is.na(goodCases)]<-0
  
  #la8os...epistrefei k NAs
  if (missing(threshold)){
    
    for (i in 1:332){
      corValues[i]=cor(cleanDataSets[[i]]$sulfate,cleanDataSets[[i]]$nitrate)
      
    }
    #corValues[275]=corValues[276]=corValues[278]=corValues[289]=corValues[291]=corValues[292]=corValues[293]=corValues[294]=corValues[286]=0
    #corValues[is.na(corValues)]<-0
    # a[is.na(a)]<-0
    corValues[is.na(corValues)]<-0
    t<-which(corValues!=0)
    corValues<-corValues[t]
  
    return (corValues) 
  }
  
  if (threshold>max(goodCases)){
    corValues<-vector(length=0)
    return (corValues[0])
  }
  
  if (max(goodCases)>threshold){
    indices<-(which(goodCases>threshold))
    
    for (i in 1:length(indices)){
      corValues[i]=cor(cleanDataSets[[indices[i]]]$sulfate,cleanDataSets[[indices[i]]]$nitrate)
      
    }
    corValues[is.na(corValues)]<-0

    return(corValues)
  }
  
  return (corValues)
}
