rankall<-function(disease,num="best"){
  setwd("/media/storage/storage/courseEra/Computing for Data Analysis/assignment2/")
  outcome<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  spec.state<-data.frame(outcome[,c(2,7,11,17,23)])
  states<-as.character(factor(outcome$State))
  temp<-split(spec.state,spec.state$State)
  final<-data.frame()
#  t<-lapply(seq_along(temp), function(x) temp[[x]][order(temp[[x]][,3]),])
#   result<-lapply(seq_along(t), function(x) t[[x]][20,c(1,2)])
#   test<-do.call(rbind,result)
 
  result<-list()
  if ((class(num)=="numeric")==TRUE){
    if(disease=="heart attack"){
      t<-lapply(seq_along(temp), function(x) temp[[x]][order(temp[[x]][,3],temp[[x]][,1]),])
      holdit<-lapply(seq_along(t), function(x) sapply(t[[x]], function(y) rbind(t[[x]][num,1],t[[x]][1,2])))
      #result<-lapply(seq_along(t), function(x) t[[x]][num,c(1,2)])
     # result<-lapply(seq_along(holdit) , function(x) holdit[[x]][,1]))
      final<-data.frame(do.call(rbind,lapply(seq_along(holdit),function(x) holdit[[x]][,1])))
      names(final)<-c("hospital","state")
      row.names(final)<-final$state
      return(final)
    }
    
    if (disease=="heart failure"){
      t<-lapply(seq_along(temp), function(x) temp[[x]][order(as.numeric(temp[[x]][,4]),temp[[x]][,1]),])
      holdit<-lapply(seq_along(t), function(x) sapply(t[[x]], function(y) rbind(t[[x]][num,1],t[[x]][1,2])))
      #result<-lapply(seq_along(t), function(x) t[[x]][num,c(1,2)])
      # result<-lapply(seq_along(holdit) , function(x) holdit[[x]][,1]))
      final<-data.frame(do.call(rbind,lapply(seq_along(holdit),function(x) holdit[[x]][,1])))
      names(final)<-c("hospital","state")
      row.names(final)<-final$state
      return(final)
    }
    
    if(disease=="pneumonia"){
      t<-lapply(seq_along(temp), function(x) temp[[x]][order(as.numeric(temp[[x]][,5]),temp[[x]][,1]),])
      holdit<-lapply(seq_along(t), function(x) sapply(t[[x]], function(y) rbind(t[[x]][num,1],t[[x]][1,2])))
      #result<-lapply(seq_along(t), function(x) t[[x]][num,c(1,2)])
      # result<-lapply(seq_along(holdit) , function(x) holdit[[x]][,1]))
      final<-data.frame(do.call(rbind,lapply(seq_along(holdit),function(x) holdit[[x]][,1])))
      names(final)<-c("hospital","state")
      row.names(final)<-final$state
      return(final)
    }
}#IF NUMERIC
###################---IF----WORST------------###############################################
  if(num=="worst"){
    if(disease=="pneumonia")
      t<-lapply(seq_along(temp), function(x) temp[[x]][order(as.numeric(temp[[x]][,5]),temp[[x]][,1],decreasing=T),])
    holdit<-lapply(seq_along(t), function(x) sapply(t[[x]], function(y) rbind(t[[x]][1,1],t[[x]][1,2])))
    # result<-lapply(seq_along(holdit) , function(x) holdit[[x]][,1]))
    #t<-lapply(seq_along(t), function(x) apply(t[[x]],2, function(y) gsub("Not Available","NA",y))) CONVERT "Not Available->NA
    final<-data.frame(do.call(rbind,lapply(seq_along(holdit),function(x) holdit[[x]][,1])))
    names(final)<-c("hospital","state")
    row.names(final)<-final$state
    return(final)
  }
  return(final) #TELIKO FINAL
  
}

#lapply(seq_along(t), function(x) sapply(t[[x]], function(y) rbind(t[[x]][20,1],t[[x]][1,2])))