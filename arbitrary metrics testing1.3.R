#Author: Mike Zizzi
#lksnfasnm;amsa;lsm
library(dplyr)
library(stringdist)
library(tidyr)
library(readr)
library(data.table)

setwd("~/mlearn")

load("C:/Users/mzizzi/Documents/mlearn/keywords_beta.RData")
vanilla.westonex<-read.csv("C:/Users/mzizzi/Documents/mlearn/weston/westonex.csv")


set.seed(696969)
samp.set<-c(sample(1:428000,2500))
westonex.sample<-vanilla.westonex[c(samp.set),c(95,43,76,91,13,69,80,97,142)]
actual.tax<-westonex.sample[,9]
westonex.sample<-westonex.sample[,-9]
#save(vanilla.westonex,file="vanilla_westonex.RData")
rm(vanilla.westonex)

### Run samples through current matching algo and store output in list, will take a while
metric.matrix.list<-vector("list",length = 2500)
trials<-50
for (i in 1:2500){
  metric.matrix.list[[i]]<-westonsampler(westonex.sample[i,])
}

## now you're thinking with portals ##  !!CLEAR YOUR RAM DUMMY!!
registerDoParallel(4)
trials<-2500
foreach(icount(trials)) %dopar%{
  metric.matrix.list[[i]]<-westonsampler(westonex.sample[i,])
}


metric.matrix.list[[1]]
rank.table.maker(metric.matrix.list,1)





#########semi-exhaustive test#########
#Vars
a<-combn(x=c(1:5),m=2)
b<-combn(x=c(1:5),m=3)
c<-combn(x=c(1:5),m=4)
combovector<-list(c(1),c(2),c(3),c(4),c(5),c(a[,1]),a[,2],a[3],a[,4],a[,5],a[,6],a[,7],a[,8],a[,9],a[,10],b[,1],b[,2],b[3],b[,4],b[,5],b[,6],b[,7],b[,8],b[,9],b[,10],c[,1],c[,2],c[3],c[,4],c[,5])
best.average<-c(100)
best.median<-c(100)
best.tags<-c(1,2,3,4) #1 = i for avg, 2 = j for avg, 3 = i for median, 4 = j for median
test.average<-0
test.median<-0
test.vector<-c(seq(1,1000,by=10))
best.log<-data.frame(sample=c(k),best.average=best.average,best.median=best.median,best.tag1=best.tags[1],best.tag2=best.tags[2],best.tag3=best.tags[3],best.tag4=best.tags[4])
set.seed(8675309)
tax.list<-list()
sample.list<-list()
timelist<-list()
#end Vars
#script


samp.set<-c(sample(1:428000,50))
for (k in 1:50){
  samp.set<-c(sample(1:428000,50))
  best.average<-c(100) #reset best average for a sample
  best.median<-c(100)  #reset best median for a sample
  test.average<-0      #reset itteration average
  test.median<-0       #reset itteration median
  westonex.sample<-read.csv(file="weston/westonex.csv")[samp.set,c(95,43,76,91,13,69,80,97,142)]
  actual.tax<-westonex.sample[,9]
  westonex.sample<-westonex.sample[,-9]
  metric.matrix.list.copy<-westonsampler(westonex.sample)
  for(i in 1:length(combovector)){
    for(a in 1:50){
      metric.matrix.list.copy[[a]][is.na(metric.matrix.list.copy[[a]])] <- as.numeric(c(0))
    }
    for(b in 1:50){
      class(metric.matrix.list.copy[[b]])<-"numeric"
    }
    
    
    for(j in 1:length(test.vector)){
      metric.matrix.list<-metric.matrix.list.copy #reset metric.matrix
      
      for(x in 1:50){
        
        metric.matrix.list[[x]][,c(combovector[[i]])]<-metric.matrix.list[[x]][,c(combovector[[i]])]*test.vector[j]
        
      } #end x loop
      
      for(z in 1:50){
        rank.tables[[z]]<-rank.table.maker(z)
      } #end z loop
      
      
      test.average<-mean(sapply(rank.tables, "[[", 2),na.rm = TRUE)
      test.median<-median(sapply(rank.tables, "[[", 2),na.rm = TRUE)
      #print(paste(test.average,test.median))
      if(test.average<best.average){
        best.average<-test.average
        best.tags[1]<-i
        best.tags[2]<-j
      }
      
      
      if(test.median<best.median){
        best.median<-test.median
        best.tags[3]<-i
        best.tags[4]<-j
      }
      
    } # end j loop
    
  } #end i loop
  #end script
  best.log[k,]<-data.frame(sample=c(k),best.average=best.average,best.median=best.median,best.tag1=best.tags[1],best.tag2=best.tags[2],best.tag3=best.tags[3],best.tag4=best.tags[4])
  tax.list[[k]]<-c(actual.tax)
  sample.list[[k]]<-c(samp.set)
  timelist[[k]]<-proc.time()
} #end k loop
arbittest<-list(best.log,tax.list,sample.list,timelist)
save(arbittest,file="arbit test run1.RData")

best.log

#conf-int for mean:   5.948101, 8.257915
#conf-int for median: 1.799274, 2.200726