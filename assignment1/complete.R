complete <- function(directory,id) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
 # data<-read.csv(directory,header=T)
#   files<-lapply(list.files(), read.csv,header=T)
#   allBadCases<-lapply(files,complete.cases)
#   correct<-dim(data[bad,])[1]
  setwd(directory)
  #files<-as.list(list.files())
  #data<-read.csv(as.character(files[(id)]),header=T)
  files<-lapply(list.files(), read.csv,header=T)
  allBadCases<-lapply(files,complete.cases)
  cases<-lapply(allBadCases,table)
  test<-vector(length=length(id))
  for (i in 1:length(id))
    test[i]=cases[[id[i]]][2]#=test[i]
  test[is.na(test)]<-0
  setwd("..")
  final<-data.frame(id=id,nobs=test)
  
  #names(final)<-c("id","nobs")
 # print(length(id))
  return (final)
}
