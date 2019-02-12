#Work out overallvalue of selected projects, value and cost change resulting from new entrants

value_proj2=function(numbers){


project<-numbers
var.names<-c('Project1', 'Project2', 'Project3','Project4','Project5')
#var.names<-c('p1', 'p2', 'p3','p4','p5','p6','p7','p8','p9','p10')
n<-length(var.names)
maxWeight<-7

#Function that checks whether two vectors have identical elements. It checks whether interactions have taken place.
SameElements <- function (a,b){
  l <- Map(table,list(a, b)) # Compute frequencies - returns ordered table
  Reduce(identical,l) # Check if frequencies are the same for all input vectors
}


pval<-matrix(c(1,1,2,2,5),ncol=n,nrow=1)
wval<-matrix(c(1,2,3,4,2),ncol=n,nrow=1)


interact<-array(0,dim=c(n))
interact1<-array(0,dim=c(n))

#values of selected solution
ip<-0 #interaction sum for selected projects
ip1<-0
vp<-0 
wp<-0 

for (i in project){

vp<-vp+pval[i]
wp<-wp+wval[i]
}


#Function that Generates all subsets of a set. This is to generate all possible combinations of selected alternatives. Goal: to capture subsets that yield synergies.
all.subsets.fast <- function(set) {
n <- length(set)
bin <- vector(mode = "list", length = n)
for (i in 1L:n) {
bin[[i]] <- rep.int(c(rep.int(F, 2L ^ (i - 1L)),
rep.int(T, 2L ^ (i - 1L))),
2L ^ (n - i))
}
apply(do.call(cbind, bin), 1L, function(x) { set[x] } )
}

if(!identical(project,numeric(0))){# To avoid Error in subset function because of the zero value inherited when launching the app: attempt to select less than one element in integerOneIndex
  if(length(project)>1){
    subsets<-all.subsets.fast(project)
    subsets[[1]]<-NULL
  #} else if(length(project)==1) {subsets<-project}
  } else {subsets<-project}
} else {subsets<-NULL}

n1<-length(project)
n3<-length(subsets)

for (k in 1:n3){
int<-0
int<-ifelse(SameElements(c(1, 2), subsets[k]),3,0)
ip<-ip+int
int<-ifelse(SameElements(c(1, 3), subsets[k]),3,0)
ip<-ip+int
int<-ifelse(SameElements(c(1, 2, 3), subsets[k]),3,0)
ip<-ip+int 
int<-ifelse(SameElements(c(2, 3, 4), subsets[k]),3,0)
ip<-ip+int
}

for (i in project){
ip1<-0
int<-0 
for (k in 1:n3){
int<-ifelse(SameElements(c(1, 2), subsets[k]),3,0)
ip1<-ip1+int
int<-ifelse(SameElements(c(1, 3), subsets[k]),3,0)
ip1<-ip1+int 
int<-ifelse(SameElements(c(1, 2, 3), subsets[k]),3,0)
ip1<-ip1+int
int<-ifelse(SameElements(c(2, 3, 4), subsets[k]),3,0)
ip1<-ip1+int
}
interact[i]<-ip1
  
}

project1<-c(1:n)
project1<-project1 [! project1 %in% project]

ln<-length(project1)
k<-0
for (j in 1:ln){
project2<-c()
project2<-c(project,project1[j])
project2<-sort(project2)
subsets<-all.subsets.fast(project2)
subsets[[1]]<-NULL
n2<-length(project2)
n4<-length(subsets)
k<-project1[j]
ip2<-0
int<-0
for (l in 1:n4){  
int<-ifelse(SameElements(c(1, 2), subsets[l]),3,0)
ip2<-ip2+int
int<-ifelse(SameElements(c(1, 3), subsets[l]),3,0)
ip2<-ip2+int 
int<-ifelse(SameElements(c(1, 2, 3), subsets[l]),3,0)
ip2<-ip2+int
int<-ifelse(SameElements(c(2, 3, 4), subsets[l]),3,0)
ip2<-ip2+int
}
interact[k]<-ip2

}


pOverall<-vp+ip # overall value for selected projects
wOverall<-wp # overall profit for selected projects
AvailableWeight<-maxWeight-wOverall


#interact1<-interact
pval1<-pval
wval1<-wval

for (i in project){
interact[i]<-c(NA)
pval1[i]<-c(NA)
}

interact2<-as.vector(interact)
newval<-vp+pval+interact2
newweight<-wOverall+wval
synergBenefit<-interact2-ip

for (i in project){
newweight[i]<-c(NA)
wval1[i]<-c(NA)
}

for (i in project){
project3<-setdiff(project,c(i)) #Subtract project c(1) from the project set to evaluate the effect of deselecting it.
#subsets<-all.subsets.fast(project3)
#subsets[[1]]<-NULL

if(!identical(project3,numeric(0))){# To avoid Error in subset function because of the zero value inherited when launching the app: attempt to select less than one element in integerOneIndex
  if(length(project3)>1){
    subsets<-all.subsets.fast(project3)
    subsets[[1]]<-NULL
  } else {subsets<-project3}
} else {subsets<-NULL}

n5<-length(subsets)

ip1<-0
int<-0 
for (k in 1:n5){
int<-ifelse(SameElements(c(1, 2), subsets[k]),3,0)
ip1<-ip1+int
int<-ifelse(SameElements(c(1, 3), subsets[k]),3,0)
ip1<-ip1+int 
int<-ifelse(SameElements(c(1, 2, 3), subsets[k]),3,0)
ip1<-ip1+int
int<-ifelse(SameElements(c(2, 3, 4), subsets[k]),3,0)
ip1<-ip1+int
}
interact1[i]<-ip1
  
}

pval2<-pval
wval2<-wval

for (i in project1){
interact1[i]<-c(NA)
pval2[i]<-c(NA)
}

interact3<-as.vector(interact1)
newval1<-vp-pval+interact3
newweight1<-wOverall-wval
synergLoss<-ip-interact3

for (i in project1){
newweight1[i]<-c(NA)
wval2[i]<-c(NA)
}

#Produce table of projects
table0<-rbind(pval,wval)
#add column names and transpose
table.project<-t(table0)
colnames(table.project) <- c("Points", "Cost")
rownames(table.project) <- var.names

#Produce Table1
table1<-rbind(newval,newweight,synergBenefit)

#add column names and transpose
table.add<-t(table1)

rownames(table.add) <- var.names
colnames(table.add) <- c("New values","New Weight","Interaction")

table3<-table.add[,c("New values","New Weight","Interaction")]
colnames(table3)<-c("New Overall Points","New Overall Cost","New Overall Synergistic Gain")	
   
#Produce Table2

table2<-rbind(newval1,newweight1,synergLoss)
#add column names and transpose

table.remove<-t(table2)

rownames(table.remove) <- var.names
colnames(table.remove) <- c("New values","New Weight","Interaction")
table4<-table.remove[,c("New values","New Weight","Interaction")]
colnames(table4)<-c("New Overall Points","New Overall Cost","New Overall Synergistic Loss")

#Produce Summary table
table<-cbind(table.project,table3,table4)
data.frame(table)
   
  return(table)

}
