#Work out overallvalue of selected projects, value and cost change resulting from new entrants

value_proj=function(numbers){


project<-numbers
n1<-length(project)
var.names<-c('Project1', 'Project2', 'Project3','Project4','Project5')
#var.names<-c('Project 1', 'Project 2', 'Project 3','Project 4','Project 5','Project 6','Project 7','Project 8','Project 9','Project 10')
n<-length(var.names)
maxWeight<-7


pval<-matrix(c(1,1,2,2,5),ncol=n,nrow=1)
wval<-matrix(c(1,2,3,4,2),ncol=n,nrow=1)


#setwd('C:/Users/DKantu01/Documents/PortfolioExperiment')
C<-read.csv(file="interactions1.csv",header=FALSE)
#C<-read.csv(file="interactions2.csv",header=FALSE)


C1<-data.frame(C)


CI<-array(0,dim=c(n,n)) #Interactions between selected projects and others

for (i in project){
CI[,i]<-C1[,i]
}

interact<-apply(CI,1,sum)

#values of selected solution
ip<-0 #interaction sum for selected projects
vp<-0 
wp<-0 

for (i in project){

vp<-vp+pval[i]
wp<-wp+wval[i]
}



for (i in project){
for (j in project){
if (j>i){ip<-ip+CI[i,j]}
}
}

pOverall<-vp+ip # overall value for selected projects
wOverall<-wp # overall profit for selected projects
AvailableWeight<-maxWeight-wOverall


interact1<-interact
pval1<-pval
wval1<-wval

for (i in project){
interact[i]<-c(NA)
pval1[i]<-c(NA)
}

newval<-pOverall+pval+interact
newweight<-wOverall+wval

for (i in project){
newweight[i]<-c(NA)
wval1[i]<-c(NA)
}

project1<-c(1:n)
project1<-project1 [! project1 %in% project]

pval2<-pval
wval2<-wval


for (i in project1){
interact1[i]<-c(NA)
pval2[i]<-c(NA)
}

newval1<-pOverall-pval-interact1
newweight1<-wOverall-wval

for (i in project1){
newweight1[i]<-c(NA)
wval2[i]<-c(NA)
}

table1<-rbind(newval,newweight,interact,pval1,wval1)
table2<-rbind(newval1,newweight1,interact1,pval2,wval2)
#add column names and transpose

table.add<-t(table1)
table.remove<-t(table2)
rownames(table.add) <- var.names
colnames(table.add) <- c("New values","New Weight","Interaction", "Profit", "Cost")
rownames(table.remove) <- var.names
colnames(table.remove) <- c("New values","New Weight","Interaction", "Profit", "Cost")
table3<-cbind(pOverall,wOverall,AvailableWeight)
colnames(table3)<-c("Overall Profit","Overall Cost","Available spend")
rownames(table3)<-c("Values")
    
  return(table3)
  
}











