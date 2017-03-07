##########################################  FUNCTIONS  #############################################



### westsampler ####
##Variables
hits<-vector("list",length = nrow(westonex.sample)) #final list of matrices 
##holder.mat<-matrix(nrow = 1,ncol = 4) #dummy matrix to store values
fails<-c()               #idk something
counter<-c(0)            #vector that metric is applied to
arb.ex.parent<-c(100)     #arbitrary metric for exact matches
arb.fuz.parent<-c(55)    #arbitrary metric for fuzzy matches
arb.exact.grep<-c(3)     #arbitrary metric for exact matches
arb.fuz.grep<-c(1)       #arbitrary metric for fuzzy matches
arb.forall<-c(3)         #arbitrary metric for every word matching
arbitrary<-c(arb.ex.parent,arb.fuz.parent,arb.exact.grep,arb.fuz.grep,arb.forall) #vector containing all class(arb) vectors
hit.counter<-c(0)
metric.matrix.list<-vector("list",length=length(samp.set))

westonsampler<-function(sampleset){
  # i loop for nrow in data import/export
  for (i in 1:nrow(sampleset)){
    metric.matrix.list[[i]]<-matrix(nrow=length(keywords.beta),ncol = 6)
    
    #hits[[i]]<-matrix(nrow=1,ncol=4)  #empty for next itteration
    #pastes all text from a row of the data file
    dummy.paste<-paste(sampleset[i,]$PO.Line.Description,sampleset[i,]$Commodity.Code.Description,sampleset[i,]$Description,sampleset[i,]$MCC.Description,sampleset[i,]$Faculty.or.Unit.ID,sampleset[i,]$Expense.Type, sampleset[i,]$Invoice.Line.Description,sampleset[i,]$Supplier.Parent.Name ,sep=" ")
    # j  for #entries in keywords lists
    for(j in(1:length(keywords.beta))){
      
      #print(j)
      #print(keywords.beta[[j]][[4]])
      hit.counter<-c(0)  #empty for next itteration
      counter<-c(0)      #empty for next itteration
      
      #Check for missing, blank, or NA
      ## first run for exact grep of parent and exact grep of additionals and then agrep of additonals
      
      if(keywords.beta[[j]][[4]]!=c("NA")){
        #print("echo")
        if((length(grep(keywords.beta[[j]][[4]], fixed=TRUE ,as.character(sampleset[i,8])))==1) && (length(keywords.beta[[j]][-c(1,2,3,4)])!=0)){
          #print(keywords.beta[[j]][[4]])
          #print("woohoo")
          counter<-counter+arb.ex.parent
          metric.matrix.list[[i]][j,1]<-c(1)
          # k for length of each entry in keywords list
          for(k in (1:length(keywords.beta[[j]][-c(1,2,3,4)]))){
            if (length(agrep("Does Not Contain",keywords.beta[[j]][k+4],ignore.case = TRUE ))!=0){
              ##use strsplit to gain keyword part of does not contain, grep phrase for part and if match, disqualify
              #print("anti")
              antiphrase.split<-strsplit(keywords.beta[[j]][k+4],"Contain ")
              antiphrase<-antiphrase.split[[1]][2]
              #disqualify if antiphrase matches
              if(length(agrep(antiphrase,ignore.case = TRUE, dummy.paste))!=0){
                break
              }
            }
            #check that keyword[j,k] is not empty
            else if((keywords.beta[[j]][[k+4]])==c("")){
              #print("book")
              next}
            
            else if(length(grep(keywords.beta[[j]][[k+4]],dummy.paste,fixed=TRUE))!=0){
              hit.counter<-hit.counter+1
              #print("ggggggggggg grep")
              ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
              ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
              ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
              counter<-counter+arb.exact.grep
              metric.matrix.list[[i]][j,3]<-c(1)
              metric.matrix.list[[i]][j,6]<-keywords.beta[[j]][[2]]
              
              
            }
            else if(length(agrep(keywords.beta[[j]][[k+4]],dummy.paste,ignore.case = TRUE))!=0){
              hit.counter<-hit.counter+1
              #print("AAA agrep")
              ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
              ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
              ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
              counter<-counter+arb.fuz.grep
              metric.matrix.list[[i]][j,4]<-c(1)
              metric.matrix.list[[i]][j,6]<-keywords.beta[[j]][[2]]
              
              
            }  
            
            
            
            
          } #end k loop 
          
        } #end first run grep parent
        
        ## second run for agrep parent
        
        else if((length(agrep(keywords.beta[[j]][[4]], ignore.case=TRUE ,as.character(sampleset[i,8])))==1) && (length(keywords.beta[[j]][-c(1,2,3,4)])!=0)){
          #print(keywords.beta[[j]][[4]])
          #print("splash")
          counter<-counter+arb.fuz.parent
          metric.matrix.list[[i]][j,2]<-c(1)
          
          # k for length of each entry in keywords list
          for(k in (1:length(keywords.beta[[j]][-c(1,2,3,4)]))){
            if (length(agrep("Does Not Contain",keywords.beta[[j]][k+4],ignore.case = TRUE ))!=0){
              ##use strsplit to gain keyword part of does not contain, grep phrase for part and if match, disqualify
              #print("anti")
              antiphrase.split<-strsplit(keywords.beta[[j]][k+4],"Contain ")
              antiphrase<-antiphrase.split[[1]][2]
              #disqualify if antiphrase matches
              if(length(agrep(antiphrase,ignore.case = TRUE, dummy.paste))!=0){
                break
              }
            }
            #check that keyword[j,k] is not empty
            else if((keywords.beta[[j]][[k+4]])==c("")){
              #print("book")
              next}
            
            else if(length(grep(keywords.beta[[j]][[k+4]],dummy.paste,fixed=TRUE))!=0){
              hit.counter<-hit.counter+1
              #print("ggggggggggg grep")
              ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
              ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
              ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
              counter<-counter+arb.exact.grep
              metric.matrix.list[[i]][j,3]<-c(1)
              metric.matrix.list[[i]][j,6]<-keywords.beta[[j]][[2]]
              
              
            }
            else if(length(agrep(keywords.beta[[j]][[k+4]],dummy.paste,ignore.case = TRUE))!=0){
              hit.counter<-hit.counter+1
              #print("AAA agrep")
              ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
              ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
              ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
              counter<-counter+arb.fuz.grep
              metric.matrix.list[[i]][j,4]<-c(1)
              metric.matrix.list[[i]][j,6]<-keywords.beta[[j]][[2]]
              
            }  
            
            
            
            
          } #end k loop 
        } #end second run agrep parent
        
        
        ## third run for parent IS NOT missing BUT NO parent match BUT additionals present
        else if(length(agrep(keywords.beta[[j]][[4]],sampleset[i,8]))==c(0) && (length(keywords.beta[[j]][-c(1,2,3,4)])!=0)){
          #print(keywords.beta[[j]][[4]])
          #print("fluffy")
          for(k in (1:length(keywords.beta[[j]][-c(1,2,3,4)]))){
            if (length(agrep("Does Not Contain",keywords.beta[[j]][k+4], ignore.case = TRUE ))!=0){
              ##use strsplit to gain keyword part of does not contain, grep phrase for part and if match, disqualify
              #print("anti")
              antiphrase.split<-strsplit(keywords.beta[[j]][k+4],"Contain ")
              antiphrase<-antiphrase.split[[1]][2]
              #disqualify if antiphrase matches
              if(length(agrep(antiphrase,ignore.case = TRUE, dummy.paste))!=0){
                break
              }
              
            }
            #check that keyword[j,k] is not empty
            else if((keywords.beta[[j]][[k+4]])==c("")){
              #print("book")
              next}
            
            else if(length(grep(keywords.beta[[j]][[k+4]],dummy.paste,fixed=TRUE))!=0){
              hit.counter<-hit.counter+1
              #print("ggggggggggg grep")
              ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
              ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
              ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
              counter<-counter+arb.exact.grep
              metric.matrix.list[[i]][j,3]<-c(1)
              metric.matrix.list[[i]][j,6]<-keywords.beta[[j]][[2]]
              
              
            }
            else if(length(agrep(keywords.beta[[j]][[k+4]],dummy.paste,ignore.case = TRUE))!=0){
              hit.counter<-hit.counter+1
              #print("AAA agrep")
              ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
              ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
              ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
              counter<-counter+arb.fuz.grep
              metric.matrix.list[[i]][j,4]<-c(1)
              metric.matrix.list[[i]][j,6]<-keywords.beta[[j]][[2]]
              
            }  
            
            
            
          } #end k loop 
        } #end third run
        
        # fourth run if parent is not missing but doesnt match w/ no additionals --> next
        else if(length(agrep(keywords.beta[[j]][[4]],sampleset[i,8]))==c(0)){
          #print(keywords.beta[[j]][[4]])
          #print("suda")
          next
        } #end fourth run    
        
        
      } #end if parent is not missing 
      ## fifth run for no parent present, but possible additionals    
      else if(keywords.beta[[j]][[4]]==c("NA") && (length(keywords.beta[[j]][-c(1,2,3,4)])!=0)){
        #print(keywords.beta[[j]][[4]])
        #print("bubble")
        for(k in (1:length(keywords.beta[[j]][-c(1,2,3,4)]))){
          if (length(agrep("Does Not Contain",keywords.beta[[j]][k+4], ignore.case = TRUE ))!=0){
            ##use strsplit to gain keyword part of does not contain, grep phrase for part and if match, disqualify
            #print("anti")
            antiphrase.split<-strsplit(keywords.beta[[j]][k+4],"Contain ")
            antiphrase<-antiphrase.split[[1]][2]
            #disqualify if antiphrase matches
            if(length(agrep(antiphrase,ignore.case = TRUE, dummy.paste))!=0){
              break
            }
            #check that keyword[j,k] is not empty
            else if((keywords.beta[[j]][[k+4]])==c("")){
              #print("book")
              next
            }  
            
            else if(length(grep(keywords.beta[[j]][[k+4]],dummy.paste,fixed=TRUE))!=0){
              hit.counter<-hit.counter+1
              #print("ggggggggggg grep")
              ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
              ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
              ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
              counter<-counter+arb.exact.grep
              metric.matrix.list[[i]][j,3]<-1
              metric.matrix.list[[i]][j,6]<-keywords.beta[[j]][[2]]
              
              
            }
            else if(length(agrep(keywords.beta[[j]][[k+4]],dummy.paste,ignore.case = TRUE))!=0){
              hit.counter<-hit.counter+1
              #print("AAA agrep")
              ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
              ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
              ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
              counter<-counter+arb.fuz.grep
              metric.matrix.list[[i]][j,4]<-1
              metric.matrix.list[[i]][j,6]<-keywords.beta[[j]][[2]]
              
            }  
            
            
          }
          
        } #end k loop 
      } #end fifth run
      #else parent is NA and no additionals -> next
      else {next}
      #if every word hits, add an additonal arb metric
      if(hit.counter==length(keywords.beta[[j]][-c(1,2,3,4)])){
        counter<-counter+arb.forall
      } 
      #priority adjustment
      ###counter<-counter+(10-as.numeric(keywords.beta[[j]][[3]]))
      if(counter!=0){##holder.mat[1,2]<-counter
        metric.matrix.list[[i]][j,5]<-1}
      #hits[[i]]<-rbind(hits[[i]],##holder.mat)
      
      
      
    } # end j loop
    metric.matrix.list[[i]][is.na(metric.matrix.list[[i]])]<-0
    class(metric.matrix.list[[i]])<-"numeric"
    
  } # end i loop
  return(metric.matrix.list)  
}



#### rank.tables ####
rank.table.maker<-function(input.list,index){
  met1<-data.frame(input.list[[index]])
  x6<-met1$X6
  met1<-apply(met1[,c(1:5)],2,as.numeric)
  met1<-data.frame(met1)
  met1$X6<-x6
  met1[is.na(met1)]<-0
  met1<-aggregate(cbind(X1,X2,X3,X4,X5) ~ X6,data=met1, FUN = sum)
  met1
  
  new.met.tab<-data.frame(taxid=met1$X6,sum=rowSums(met1[,c(2:6)]))
  a<-new.met.tab<-arrange(new.met.tab,desc(sum))
  b<-target.rank<-as.numeric(match(gsub("-","",as.character(actual.tax[index])),new.met.tab$taxid))
  return(list(a,b))
}



#### west.test ####
west.test<-function(number){
  abc<-data.frame(hits[number])
  abc$X2<- as.numeric(abc$X2)
  abc<-aggregate(X2 ~ X1, data=abc, FUN=sum)
  abc$X1<-as.character(abc$X1)
  x<-arrange(abc,desc(X2))
  y<-gsub("-","",as.character(actual.tax[number]))
  z<-match(y, x$X1)
  return(list(x,y,z))
}  
  
  
  
  
  
#west sampler mod
  
  westonsampler.mod<-function(sampleset){
    
      metric.matrix<-matrix(nrow=length(keywords.beta),ncol = 6)
      
      #hits[[i]]<-matrix(nrow=1,ncol=4)  #empty for next itteration
      #pastes all text from a row of the data file
      dummy.paste<-paste(sampleset[1,,]$PO.Line.Description,sampleset[1,,]$Commodity.Code.Description,sampleset[1,,]$Description,sampleset[1,,]$MCC.Description,sampleset[1,,]$Faculty.or.Unit.ID,sampleset[1,,]$Expense.Type, sampleset[1,,]$Invoice.Line.Description,sampleset[1,,]$Supplier.Parent.Name ,sep=" ")
      # j  for #entries in keywords lists
      for(j in(1:length(keywords.beta))){
        
        #print(j)
        #print(keywords.beta[[j]][[4]])
        hit.counter<-c(0)  #empty for next itteration
        counter<-c(0)      #empty for next itteration
        
        #Check for missing, blank, or NA
        ## first run for exact grep of parent and exact grep of additionals and then agrep of additonals
        
        if(keywords.beta[[j]][[4]]!=c("NA")){
          #print("echo")
          if((length(grep(keywords.beta[[j]][[4]], fixed=TRUE ,as.character(sampleset[1,,8])))==1) && (length(keywords.beta[[j]][-c(1,2,3,4)])!=0)){
            #print(keywords.beta[[j]][[4]])
            #print("woohoo")
            counter<-counter+arb.ex.parent
            metric.matrix[j,1]<-c(1)
            # k for length of each entry in keywords list
            for(k in (1:length(keywords.beta[[j]][-c(1,2,3,4)]))){
              if (length(agrep("Does Not Contain",keywords.beta[[j]][k+4],ignore.case = TRUE ))!=0){
                ##use strsplit to gain keyword part of does not contain, grep phrase for part and if match, disqualify
                #print("anti")
                antiphrase.split<-strsplit(keywords.beta[[j]][k+4],"Contain ")
                antiphrase<-antiphrase.split[[1]][2]
                #disqualify if antiphrase matches
                if(length(agrep(antiphrase,ignore.case = TRUE, dummy.paste))!=0){
                  break
                }
              }
              #check that keyword[j,k] is not empty
              else if((keywords.beta[[j]][[k+4]])==c("")){
                #print("book")
                next}
              
              else if(length(grep(keywords.beta[[j]][[k+4]],dummy.paste,fixed=TRUE))!=0){
                hit.counter<-hit.counter+1
                #print("ggggggggggg grep")
                ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
                ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
                ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
                counter<-counter+arb.exact.grep
                metric.matrix[j,3]<-c(1)
                metric.matrix[j,6]<-keywords.beta[[j]][[2]]
                
                
              }
              else if(length(agrep(keywords.beta[[j]][[k+4]],dummy.paste,ignore.case = TRUE))!=0){
                hit.counter<-hit.counter+1
                #print("AAA agrep")
                ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
                ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
                ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
                counter<-counter+arb.fuz.grep
                metric.matrix[j,4]<-c(1)
                metric.matrix[j,6]<-keywords.beta[[j]][[2]]
                
                
              }  
              
              
              
              
            } #end k loop 
            
          } #end first run grep parent
          
          ## second run for agrep parent
          
          else if((length(agrep(keywords.beta[[j]][[4]], ignore.case=TRUE ,as.character(sampleset[1,,8])))==1) && (length(keywords.beta[[j]][-c(1,2,3,4)])!=0)){
            #print(keywords.beta[[j]][[4]])
            #print("splash")
            counter<-counter+arb.fuz.parent
            metric.matrix[j,2]<-c(1)
            
            # k for length of each entry in keywords list
            for(k in (1:length(keywords.beta[[j]][-c(1,2,3,4)]))){
              if (length(agrep("Does Not Contain",keywords.beta[[j]][k+4],ignore.case = TRUE ))!=0){
                ##use strsplit to gain keyword part of does not contain, grep phrase for part and if match, disqualify
                #print("anti")
                antiphrase.split<-strsplit(keywords.beta[[j]][k+4],"Contain ")
                antiphrase<-antiphrase.split[[1]][2]
                #disqualify if antiphrase matches
                if(length(agrep(antiphrase,ignore.case = TRUE, dummy.paste))!=0){
                  break
                }
              }
              #check that keyword[j,k] is not empty
              else if((keywords.beta[[j]][[k+4]])==c("")){
                #print("book")
                next}
              
              else if(length(grep(keywords.beta[[j]][[k+4]],dummy.paste,fixed=TRUE))!=0){
                hit.counter<-hit.counter+1
                #print("ggggggggggg grep")
                ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
                ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
                ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
                counter<-counter+arb.exact.grep
                metric.matrix[j,3]<-c(1)
                metric.matrix[j,6]<-keywords.beta[[j]][[2]]
                
                
              }
              else if(length(agrep(keywords.beta[[j]][[k+4]],dummy.paste,ignore.case = TRUE))!=0){
                hit.counter<-hit.counter+1
                #print("AAA agrep")
                ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
                ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
                ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
                counter<-counter+arb.fuz.grep
                metric.matrix[j,4]<-c(1)
                metric.matrix[j,6]<-keywords.beta[[j]][[2]]
                
              }  
              
              
              
              
            } #end k loop 
          } #end second run agrep parent
          
          
          ## third run for parent IS NOT missing BUT NO parent match BUT additionals present
          else if(length(agrep(keywords.beta[[j]][[4]],sampleset[1,,8]))==c(0) && (length(keywords.beta[[j]][-c(1,2,3,4)])!=0)){
            #print(keywords.beta[[j]][[4]])
            #print("fluffy")
            for(k in (1:length(keywords.beta[[j]][-c(1,2,3,4)]))){
              if (length(agrep("Does Not Contain",keywords.beta[[j]][k+4], ignore.case = TRUE ))!=0){
                ##use strsplit to gain keyword part of does not contain, grep phrase for part and if match, disqualify
                #print("anti")
                antiphrase.split<-strsplit(keywords.beta[[j]][k+4],"Contain ")
                antiphrase<-antiphrase.split[[1]][2]
                #disqualify if antiphrase matches
                if(length(agrep(antiphrase,ignore.case = TRUE, dummy.paste))!=0){
                  break
                }
                
              }
              #check that keyword[j,k] is not empty
              else if((keywords.beta[[j]][[k+4]])==c("")){
                #print("book")
                next}
              
              else if(length(grep(keywords.beta[[j]][[k+4]],dummy.paste,fixed=TRUE))!=0){
                hit.counter<-hit.counter+1
                #print("ggggggggggg grep")
                ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
                ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
                ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
                counter<-counter+arb.exact.grep
                metric.matrix[j,3]<-c(1)
                metric.matrix[j,6]<-keywords.beta[[j]][[2]]
                
                
              }
              else if(length(agrep(keywords.beta[[j]][[k+4]],dummy.paste,ignore.case = TRUE))!=0){
                hit.counter<-hit.counter+1
                #print("AAA agrep")
                ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
                ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
                ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
                counter<-counter+arb.fuz.grep
                metric.matrix[j,4]<-c(1)
                metric.matrix[j,6]<-keywords.beta[[j]][[2]]
                
              }  
              
              
              
            } #end k loop 
          } #end third run
          
          # fourth run if parent is not missing but doesnt match w/ no additionals --> next
          else if(length(agrep(keywords.beta[[j]][[4]],sampleset[1,,8]))==c(0)){
            #print(keywords.beta[[j]][[4]])
            #print("suda")
            next
          } #end fourth run    
          
          
        } #end if parent is not missing 
        ## fifth run for no parent present, but possible additionals    
        else if(keywords.beta[[j]][[4]]==c("NA") && (length(keywords.beta[[j]][-c(1,2,3,4)])!=0)){
          #print(keywords.beta[[j]][[4]])
          #print("bubble")
          for(k in (1:length(keywords.beta[[j]][-c(1,2,3,4)]))){
            if (length(agrep("Does Not Contain",keywords.beta[[j]][k+4], ignore.case = TRUE ))!=0){
              ##use strsplit to gain keyword part of does not contain, grep phrase for part and if match, disqualify
              #print("anti")
              antiphrase.split<-strsplit(keywords.beta[[j]][k+4],"Contain ")
              antiphrase<-antiphrase.split[[1]][2]
              #disqualify if antiphrase matches
              if(length(agrep(antiphrase,ignore.case = TRUE, dummy.paste))!=0){
                break
              }
              #check that keyword[j,k] is not empty
              else if((keywords.beta[[j]][[k+4]])==c("")){
                #print("book")
                next
              }  
              
              else if(length(grep(keywords.beta[[j]][[k+4]],dummy.paste,fixed=TRUE))!=0){
                hit.counter<-hit.counter+1
                #print("ggggggggggg grep")
                ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
                ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
                ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
                counter<-counter+arb.exact.grep
                metric.matrix[j,3]<-1
                metric.matrix[j,6]<-keywords.beta[[j]][[2]]
                
                
              }
              else if(length(agrep(keywords.beta[[j]][[k+4]],dummy.paste,ignore.case = TRUE))!=0){
                hit.counter<-hit.counter+1
                #print("AAA agrep")
                ##holder.mat[1,1]<-keywords.beta[[j]][[2]]
                ##holder.mat[1,3]<-length(keywords.beta[[j]][-c(1,2,3)])
                ##holder.mat[1,4]<-keywords.beta[[j]][[3]]
                counter<-counter+arb.fuz.grep
                metric.matrix[j,4]<-1
                metric.matrix[j,6]<-keywords.beta[[j]][[2]]
                
              }  
              
              
            }
            
          } #end k loop 
        } #end fifth run
        #else parent is NA and no additionals -> next
        else {next}
        #if every word hits, add an additonal arb metric
        if(hit.counter==length(keywords.beta[[j]][-c(1,2,3,4)])){
          counter<-counter+arb.forall
        } 
        #priority adjustment
        ###counter<-counter+(10-as.numeric(keywords.beta[[j]][[3]]))
        if(counter!=0){##holder.mat[1,2]<-counter
          metric.matrix[j,5]<-1}
        #hits[[i]]<-rbind(hits[[i]],##holder.mat)
        
        
        
      } # end j loop
      metric.matrix <- metric.matrix[!apply(is.na(metric.matrix) | metric.matrix == "", 1, all),]
      metric.matrix[is.na(metric.matrix)]<-0
      class(metric.matrix)<-"numeric"
      

    return(metric.matrix)  
  }  
  
  
  
  
  #### Weight tester ####
  
  weight.tester<-function(trialnum){
    modified.matrix<-metric.matrix.list[[trialnum]]
    best.log.list<-vector('list',length=31)
    for(i in 1:10){
      if      (i%in%(1:5))  {perm.index=1}
      else if (i%in%(6:15)) {perm.index=2}
      else if (i%in%(16:25)){perm.index=3}
      else if (i%in%(26:30)){perm.index=4}
      else                  {perm.index=5}
      rank.tables<-matrix(nrow=nrow(perm.matrix.list[[perm.index]]),ncol=3)
      
      for(j in 1:nrow(perm.matrix.list[[perm.index]])){
        #if(best.rank.finder(trial,perm.index,i,j)[1]!=100){
          if(perm.index>1){
            modified.matrix[,c(combovector[[i]])]<-modified.matrix[,c(combovector[[i]])]%*% diag(c(perm.matrix.list[[perm.index]][j,]))
          }
          else{
            modified.matrix[,c(combovector[[i]])]<-modified.matrix[,c(combovector[[i]])]*(c(perm.matrix.list[[perm.index]][j,]))
          }
          
          test.rank<-rank.table.maker.mod(modified.matrix,trialnum)[[2]]
          if((!is.na(test.rank)) && (test.rank<10)){
          rank.tables[j,1]<-test.rank
          rank.tables[j,2]<-i
          rank.tables[j,3]<-j
        }
        
        
      } # end j loop
      best.log.frame<-data.frame(na.omit(rank.tables))
      
      best.log.list[[i]]<-best.log.frame
    } #end i loop
    final.bind<-rbindlist(best.log.list)
    return(final.bind)
  } #end function
  
  
  
  
#### Weight tester parallel ####
  
  weight.tester.par<-function(trial){
    modified.matrix<-metric.matrix.list[[trial]]
    best.log.list<-foreach(i=1:length(combovector), .packages = c("dplyr"), .export = c("%:%","best.rank.finder","best.tags","foreach","%dopar%","best.rank","actual.tax","rank.table.maker.mod","metric.matrix.list","perm.matrix.list","perm.index","combovector","best.average","best.median","test.average","test.median")) %:% {
      if(i%in%(1:5)){perm.index=1}
      else if (i%in%(6:15)){perm.index=2}
      else if (i%in%(16:25)){perm.index=3}
      else if (i%in%(26:30)){perm.index=4}
      rank.tables<-vector("list",length=nrow(perm.matrix.list[[perm.index]]))
      
      best.rank.list<-foreach(j=1:nrow(perm.matrix.list[[perm.index]]), .packages = c("dplyr"),.export = c("%dopar%","best.tags","best.rank.finder","best.rank","actual.tax","rank.table.maker.mod","metric.matrix.list","perm.matrix.list","perm.index","combovector","best.average","best.median","test.average","test.median") ) %dopar% {
        
       if(best.rank.finder(trial,i,j)[1]!=100){
          x<-c(X1=best.rank.finder(trial,i,j)[1],X2=best.rank.finder(trial,i,j)[2],X3=best.rank.finder(trial,i,j)[3])
       }
        
        
      } # end j loop
      best.log.frame<-data.frame(X1=unlist(best.rank.list[!sapply(best.rank.list,is.null)])[c(TRUE,FALSE,FALSE)],X2=unlist(best.rank.list[!sapply(best.rank.list,is.null)])[c(FALSE,TRUE,FALSE)],X3=unlist(best.rank.list[!sapply(best.rank.list,is.null)])[c(FALSE,FALSE,TRUE)])
      
    } #end i loop
    
    return(best.log.list)
  } #end function
  
  
  
  
  #### rank.tables MODIFIED ####
  rank.table.maker.mod<-function(input.mat,index){
    met1<-data.frame(input.mat)
    met1<-aggregate(cbind(X1,X2,X3,X4,X5) ~ X6,data=met1, FUN = sum)
    
    new.met.tab<-data.frame(taxid=met1$X6,sum=rowSums(met1[,c(2:6)]))
    a<-new.met.tab<-arrange(new.met.tab,desc(sum))
    b<-as.numeric(match(gsub("-","",as.character(actual.tax[index])),new.met.tab$taxid))
    return(list(a,b))
  }  
  
  
  ###Best rank finder ###
  best.rank.finder<-function(trial,perm.index,i,j){
    modified.matrix<-metric.matrix.list[[trial]]
    if(perm.index>1){
    modified.matrix[,c(combovector[[i]])]<-modified.matrix[,c(combovector[[i]])]%*% diag(c(perm.matrix.list[[perm.index]][j,]))
    }
    else{
      modified.matrix[,c(combovector[[i]])]<-modified.matrix[,c(combovector[[i]])]*(c(perm.matrix.list[[perm.index]][j,]))
}
    #rank.tables[[j]]<-rank.table.maker.mod(modified.matrix,j)
    
    
    #test.average<-mean(sapply(rank.tables, "[[", 2),na.rm = TRUE)
    #test.median<-median(sapply(rank.tables, "[[", 2),na.rm = TRUE)
    test.rank<-rank.table.maker.mod(modified.matrix,j)[[2]]
    
    if(!is.na(test.rank)){
      if(test.rank<best.rank){
      best.rank<-test.rank
      best.tags[1]<-i
      best.tags[2]<-j
      }
    }
    return(c(best.rank,best.tags[1],best.tags[2]))
  }
  