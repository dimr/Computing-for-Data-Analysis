rankhospital<-function(state,disease,num="best"){
  
  setwd("/media/storage/storage/courseEra/Computing for Data Analysis/assignment2/")
  outcome<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  temp<-split(outcome,outcome$State)
  spec.state<-temp[[state]]
  spec.state<-data.frame(spec.state[,c(2,11,17,23)])
  spec.state$Hospital.Name<-as.character(spec.state$Hospital.Name)
  
  spec.state<-data.frame(spec.state$Hospital.Name,apply(spec.state[,c(2,3,4)],2,as.numeric))
  spec.state[,1]<-as.character(spec.state[,1])
  names(spec.state)[1]="Hospital.Name"
  correct<-c("heart attack","heart failure","pneumonia")
  number.of.hospitals<-length(unique(spec.state$Hospital.Name))
  hospital<-character()
  x<-c(NA)
  if(class(num)=="numeric" && num>number.of.hospitals)
    return(x)
  if ((any(outcome$State==state))==FALSE)
    stop("invalid state")
  if((any(correct==disease)==FALSE))
    stop("invalid outcome")
  options(warn=-1)
  
  if ((class(num)=="numeric")==TRUE){
    if (disease=="heart failure"){
      spec.state<-spec.state[order(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,spec.state$Hospital.Name),]
      hospital=(spec.state$Hospital.Name[num])
      return(hospital)
    }
    if (disease=="heart attack"){
      spec.state<-spec.state[order(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,spec.state$Hospital.Name),]
      hospital=(spec.state$Hospital.Name[num])
      return(hospital)
    }
    if (disease=="pneumonia"){
      spec.state<-spec.state[order(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,spec.state$Hospital.Name),]
      hospital=(spec.state$Hospital.Name[num])
      return(hospital)
    }
  }#CLASS NUMERIC IF
    
#######################################---IF---WORST----#########################################
  
  if(num=="worst"){
    if (disease=="heart attack"){
     # spec.state<-spec.state[order(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,spec.state$Hospital.Name),]
      maximum<-max(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm=T)
      hospital<-spec.state$Hospital.Name[spec.state[,2]==maximum]
      
      return(hospital[1])
    }
    if (disease=="heart failure"){
      # spec.state<-spec.state[order(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,spec.state$Hospital.Name),]
      maximum<-max(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm=T)
      hospital<-spec.state$Hospital.Name[spec.state[,3]==maximum]
      
      return(hospital[1])
    }
    
    if (disease=="heart failure"){
      # spec.state<-spec.state[order(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,spec.state$Hospital.Name),]
      maximum<-max(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm=T)
      hospital<-spec.state$Hospital.Name[spec.state[,4]==maximum]
      
      return(hospital[1])
    }
  }#IF WORST CASE 
  
#########################################---IF--BEST---#########################################
  
  if(num=="best"){
    if (disease=="heart attack"){
      # spec.state<-spec.state[order(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,spec.state$Hospital.Name),]
      minimum<-min(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm=T)
      hospital<-spec.state$Hospital.Name[spec.state[,2]==minimum]
      
      return(hospital[1])
    }
    if (disease=="heart failure"){
      # spec.state<-spec.state[order(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,spec.state$Hospital.Name),]
      minimum<-mim(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm=T)
      hospital<-spec.state$Hospital.Name[spec.state[,3]==minimum]
      
      return(hospital[1])
    }
    
    if (disease=="heart failure"){
      # spec.state<-spec.state[order(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,spec.state$Hospital.Name),]
      minimum<-max(spec.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm=T)
      hospital<-spec.state$Hospital.Name[spec.state[,4]==minimum]
      
      return(hospital[1])
    
    }
    
    
  }#IF BEST CASE
  
  return (hospital)
  }