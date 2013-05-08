library("foreign")
Sys.setenv(NOAWT=1)
library("Deducer")

#If needs to be removed: Sys.unsetenv("NOAWT")

library("Hmisc")
library("doBy")

#START
#Script for checking variables
#
varChecking <- function(varnum) {
  cname = colnames(mydata[varnum])
  ftable = frequencies(mydata[[varnum]])
  return(list(cname,ftable))
}

varChecking(945)



#
#Script for checking variables
#END

#START
#Script for changing multicodes into Yes/No
#

#Grab variable name
#Grab all the levels of the variable
#Create new variables with the first variable name and each of the levels, code all 0
#Go through the original variables and where level is present, code 1 in new variable

#change the number range to the variables you are looking to recode

#GettingToYes <- function(columnStart,columnEnd) {

columnStart=22
columnEnd=27


for(i in columnStart:columnEnd){
  columnName = colnames(mydata[i])
  print(columnName)
}

for(v in 1:length(levels(mydata[[columnName]]))){
  eachVariable = levels(mydata[[columnName]])[v]
  #print(eachVariable)
  trimColumn = substr(columnName, 1, nchar(columnName)-1)
  mydata[paste(trimColumn,eachVariable,sep="")] = 0
}

for(c in columnStart:columnEnd){
  for(r in 1:nrow(mydata)){
    if(is.na(mydata[r,c]) == FALSE){
      #print(paste(trimColumn,mydata[r, c],sep=""))
      mydata[r,(paste(trimColumn,mydata[r,c],sep=""))] = 1
    }
  }
}

print("Done")

frequencies(mydata[paste(trimColumn,eachVariable,sep="")])

mydata[paste(trimColumn,eachVariable,sep="")]

names(mydata)
#}

#
#Script for changing multicodes into Yes/No
#END

#START
#Script for turning financial number  (Congolese Francs) strings into values (in USD)
#

colNum = 45
colName = colnames(mydata[col])

mydata[paste(colName,"_fc",sep="")] = 0
mydata[paste(colName,"_clean",sep="")] = 0

for(r in 1:nrow(mydata)){
  if(is.na(mydata[r,colName]) == FALSE){
    isFC = grepl("fc", mydata[r,colName], ignore.case = TRUE)
    mydata[r,(paste(colName,"_fc",sep=""))] = isFC
    mydata[r,(paste(colName,"_clean",sep=""))] = as.integer(gsub("[^0-9]", "", mydata[r,colName]))
    if(isFC == TRUE){
      mydata[r,(paste(colName,"_clean",sep=""))] = (mydata[r,(paste(colName,"_clean",sep=""))])/917
    }
    
  }
}

frequencies(mydata[946])

#
#Script for changing financial number strings into values
#END

