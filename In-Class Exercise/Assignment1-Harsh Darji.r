
mydata<-read.csv("C:/Users/Harsh Darji/Desktop/EmployeeCleanedData.csv")
#Reading the csv file 

head(mydata)

install.packages("arules")
#Installing the package arules

library(arules)
#loading package arules

install.packages("arulesViz")
#Installing the package arulesViz

head(mydata)

str(mydata)
# Structure of Dataframe

mydf<-mydata[,-which(names(mydata)=="Voluntary.Involuntary.Termination.Flag")]
# Removing this column as there are 1200 na values, making this variable insignificant

mydf$Termination.Reason.Description[is.na(mydf$Termination.Reason.Description)]<-"other"
mydf$Protected.Veteran.Status[is.na(mydf$Protected.Veteran.Status)]<-0
#Replacing NA in Termination.reason.description column by other as the reason to leave can e something else.
# Replacing NA in Protected.Veteran.Status by 0 as we do not have the metadata where we can understand values like 2,4.

head(mydf)

mydf$Attrition<-ifelse(mydf$Attrition==0,"Not left","Left")
# Attrition=0 ->Not left, Attrition=1-> Left 


str(mydf)

mydf$Attrition<-as.factor(mydf$Attrition)
#Converting Attrition into factor

rules<-apriori(mydf,parameter=list(supp=0.001,conf=0.8),appearance=list(default='lhs',rhs='Attrition=Left'),control=list(verbose=F))
# Suport= 0.001
#Confidence=0.8

summary(rules)

inspect(rules[1:10])

#Following Factors lead to Attrition =1 i.e employee Leaving.
#Termination.Reason.Description=Death
#Protected.Veteran.Status=3
#Termination.Reason.Description=Fight on Co Prop
#Termination.Reason.Description=Poor Performance

new_rules<-apriori(mydf,parameter=list(supp=0.01,conf=0.7),appearance=list(default='lhs',rhs='Attrition=Left'),control=list(verbose=F))

#Support=0.01
#Confidence=0.7

summary(new_rules)

inspect(new_rules[1:10])

#Following Factors lead to Attrition =1 i.e employee Leaving.
#Termination.Reason.Description=Abandoned Job
#Termination.Reason.Description=Another Job
#Termination.Reason.Description=Abandoned Job,                                                         
      #Protected.Veteran.Status=2

top_rules<-head(sort(rules,by='lift'),10)

library(arulesViz)

plot(top_rules,method='graph')

plot(top_rules,method='paracoord')

new_top_rules<-head(sort(new_rules,by='lift'),10)

plot(new_top_rules,method='graph')

plot(top_rules,method='paracoord')

# Major Factors that lead to Attrition among employees are:
# Death
# Change of Job
#Poor Performance
#Abandoned Job and pay frequency is weekly
#Re location
#Fight on co op


