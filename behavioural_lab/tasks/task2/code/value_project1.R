#Work out overallvalue of selected projects, value and cost change resulting from new entrants

value_proj1=function(numbers){

project<-numbers
var.names<-c('Project1', 'Project2', 'Project3','Project4','Project5')
#var.names<-c('p1', 'p2', 'p3','p4','p5','p6','p7','p8','p9','p10')
n<-length(var.names)
maxWeight<-7
warning<-c("")

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

if(AvailableWeight<0){warning<-c("You have exceeded your Budget!")}
    
  return(warning)
  
}











