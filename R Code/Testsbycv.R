#Hannah Lerner
#7/13/16
#hlerner@bu.edu
#Pulmonomics Lab, Boston University

#pipouts<-function(testf , folder){
#require(plyr)
library(plyr)
#require(shiny)
#require(tcltk)
#testfs<-testf
#folders<-folder
folders<-"~/Dropbox/Summer_2016/Pipeline_Output"
#pulls annotations data and stores it to variable
annotations<-read.delim("~/Dropbox/Summer_2016/annotation.txt")
model_annotations<-read.delim("~/Dropbox/Summer_2016/model_annot.txt")
#testf<-tk_choose.files(default = "", caption = "Select your annotations file")
#annotations<-read.delim(testfs)

#create an empty data frame to store the information in
tests<- data.frame(Sample_ID=factor(),Score=factor(),prediction=factor(),cv_loop=integer(),model=integer())


#pipline output
#folder<-tk_choose.dir(default = "", caption = "Select your Pipeline output folder")
#the outer loop is for each cv_loop (10 total)
for(cv in 1:10){
  cvloop<-paste0("/cv_loop_",cv)
  
  #the inner loop is for each model (24 in total)
  for(mymodel in 1:24){
    
    #creating the pathway to look to read from
    modnum<-paste0("/model_",mymodel)
    modplace<-paste0(folders,cvloop)
    almostmod<-paste0(modplace,modnum)
    modfin<-paste0(almostmod,"/predictions.txt")

    #adds a column to differentiate the data in different cv_loops
    test<-cbind(read.table(modfin,header=TRUE,sep="\t"),cv)
    test<-cbind(test,model=mymodel)
    
    #puts the data into the table 1 model at a time
    tests<- rbind(tests,test)
  }
}

# make sure annotations is completely unique
# tests is expected to not be unique, but is tested as well
# View(tests)
# table(tests$Sample_ID %in% annotations$ARRAY)
# table(annotations$ARRAY %in% tests$Sample_ID)
# table(duplicated(annotations$ARRAY))
# table(duplicated(tests$Sample_ID))


#a new merged table with all the data merged
ant<- merge(tests,annotations,by.x="Sample_ID",by.y="ARRAY", all.x = TRUE, all.y = FALSE)

#This needed to be changed when calculating the ROC curve/AUC for uniformity
ant$SmokingStatus<-revalue(as.factor(ant$SmokingStatus), c("2"="0"))

#Test because ant$SmokingStatus was originally saved as an int
class(ant$SmokingStatus)


afinaltable<-c()
print("calculating...")
#loop to count each cv_loop
for(loop in 1:10){
  
  #loop to count each model
  for(mod in 1:24){
    
    #calculate a true positive
    a<-sum(ant$Prediction==1&ant$SmokingStatus==1&ant$cv==loop&ant$model==mod)
    as<-paste0("true positive: ",a)
    
  
    
    #calculate a false negative
    b<-sum(ant$Prediction==0&ant$SmokingStatus==1&ant$cv==loop&ant$model==mod)
    bs<-paste0("false negative: ",b)
    
    
    #calculate a false positive
    c<-sum(ant$Prediction==1&ant$SmokingStatus==0&ant$cv==loop&ant$model==mod)
    cs<-paste0("false positive: ",c)
    #print(cs)
  
    #calculate a true negative
    d<-sum(ant$Prediction==0&ant$SmokingStatus==0&ant$cv==loop&ant$model==mod)
    ds<-paste0("true negative: ",d)
   
  
    #sensitivity
    sens<-a/(a+b)
    senstest<-paste0("sensitivity: ",sens)
   
    
    #specificity
    spec<-d/(c+d)
    spectest<-paste0("specificity: ", spec)
   
     
    #AUC
    funsub <- subset(ant,cv==loop&model==mod)
    myauc <- AUC::auc(AUC::roc(funsub$Prediction,as.factor(funsub$SmokingStatus)))
    
    
    #Counts the number of NA's per model
    naCount<-sum(ant$Score=="NA"&ant$SmokingStatus=="NA"&ant$cv==loop&ant$model==mod)
    
    #saves the data from this iteration of the loop to a separate table
    afinaltable<-rbind(afinaltable,c(amodel=mod , asens=sens , aspec=spec , acv_loop=loop , AUC=myauc , naPerModel = naCount))

    }
}
#possibly unnecessary line
afinaltable <- data.frame(afinaltable)

#create new vector to house avgtable
avgtable<-c()

#averages the values from afinaltable to create a new table, avgtable
for(aloop in 1:24){
  
  #averages the values model by model (NOT cv_loop by cv_loop)
  tempsub <- subset(afinaltable, amodel==aloop)
  tempmeansens <- mean(tempsub$asens)
  tempmeanspec <- mean(tempsub$aspec)
  tempmeanAUC <- mean(tempsub$AUC)
  tempmeannaPerModel <- sum(tempsub$Score==NaN)
  
  #clears the temp subset for reuse
  tempsub<-c()
  avgtable<-rbind(avgtable,c(model=aloop,numNA=tempmeannaPerModel,avgsens=tempmeansens,avgspec=tempmeanspec,AUC=tempmeanAUC))
}
avgtable<-merge.data.frame(model_annotations,avgtable, by.x="model",by.y="model")
boxplot(avgtable$AUC~avgtable$FeatureFilter)
boxplot(avgtable$AUC~avgtable$FeatureSelection)
boxplot(avgtable$AUC~avgtable$BiomarkerSize)
boxplot(avgtable$AUC~avgtable$Classifier)
#array to assign colors
n<-c(31,87,100,153,258,374,147,116,371,400,503,573,650,429,477,551,434,12,62,94,584,211,442)

#plots the ROC curve
print("plotting...")
for(cvl in 1:10){
  
  for(mod in 1:24){
    
    notfunsubs <- subset(ant,cv==loop&model==mod)
    if(cvl==1&mod==1){
    plot(AUC::roc(notfunsubs$Prediction,as.factor(notfunsubs$SmokingStatus)))
    }
    else{
    plot(AUC::roc(notfunsubs$Prediction,as.factor(notfunsubs$SmokingStatus)),add=TRUE,col=n[mod])
    }
  }
}
#return(avgtable)
#used to distribute the avgtable
#write.table(avgtable,file="avgtable.txt",sep="\t",col.names=TRUE,row.names=FALSE,quote=FALSE)

#}