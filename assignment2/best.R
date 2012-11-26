#setwd("/media/storage/storage/courseEra/Computing for Data Analysis/assignment2/")
#unzip("ProgAssignment2-data.zip")
#outcome<-read.csv("outcome-of-care-measures.csv",colClasses="character")

# par(mfrow=c(1,3))
# hist(as.numeric(outcome[,11]),main="Heart Attack",xlim=range(na.omit(as.numeric(outcome[,11]))),xlab="30--day Death Rate")
# #lines(density(as.numeric(outcome[,11])))
# abline(v=median(na.omit(as.numeric(outcome[,11]))),col="red")
# hist(as.numeric(outcome[,17]),main="Heart Failure",xlim=range(na.omit(as.numeric(outcome[,17]))),xlab="30--day Death Rate")
# abline(v=median(na.omit(as.numeric(outcome[,17]))),col="red")
# hist(as.numeric(outcome[,23]),main="Pneumonia",xlim=range(na.omit(as.numeric(outcome[,23]))),xlab="30--day Death Rate")
# abline(v=median(na.omit(as.numeric(outcome[,23]))),col="red")
#mask<-sapply(1:nrow(outcome),function(i) sum(outcome$State==outcome$State[i])>20)
#outcome2<-outcome[mask,]

#par(mfrow=c(1,1))


#outcome2<-subset(outcome,table(outcome$State)>20)

best<-function(state,disease){
  
  setwd("/media/storage/storage/courseEra/Computing for Data Analysis/assignment2/")
  outcome<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  correct<-c("heart attack","heart failure","pneumonia")
  if ((any(outcome$State==state))==FALSE)
    stop("invalid state")
  if((any(correct==disease)==FALSE))
    stop("invalid outcome")
  options(warn=-1)
  temp<-split(outcome,outcome$State)
  spec.state<-temp[[state]]
  spec.state<-data.frame(spec.state[,c(2,11,17,23)])
  spec.state$Hospital.Name<-as.character(spec.state$Hospital.Name)
  
  spec.state<-data.frame(spec.state$Hospital.Name,apply(spec.state[,c(2,3,4)],2,as.numeric))
  spec.state[,1]<-as.character(spec.state[,1])
  names(spec.state)[1]="Hospital.Name"
  hospital<-character()
  if (disease=="heart attack"){
    spec.state<-spec.state[order(spec.state[,2]),]
    hospital<-spec.state[,1][1]
    return (hospital)
  }
  
  
  if(disease=="heart failure"){
    spec.state<-spec.state[order(spec.state[,3]),]
    hospital<-spec.state[,1][1]
    
  return (hospital)
  }
  
  
  if(disease=="pneumonia"){
    #hospital<-spec.state$Hospital.Name[spec.state[,4]==min(spec.state[,4])]
    spec.state<-spec.state[order(spec.state[,4]),]
    hospital<-spec.state[,1][1]
  return (hospital)
  }
  
  
  return (hospital)
}