#Hannah Lerner
#7/13/16
#hlerner@bu.edu


#create an empty data frame to store the information in
tests<- data.frame(Sample_ID=factor(),Score=factor(),prediction=factor())
annotations<-read.delim("~/Dropbox/Summer_2016/annotation.txt")
#the files are stored in 2 sets of folders
#the first 10 contain the different cv loops
for(m in 1:10){
  bigm<-paste0("cv_loop_",m)
  
  #the inner loop is for each model (24 in total)
  for(i in 1:24){
    #creating the pathway to look to read from
    modnum<-paste0("/model_",i)
    modplace<-paste0("~/Dropbox/Summer_2016/Pipeline_Output/",bigm)
    almostmod<-paste0(modplace,modnum)
    modfin<-paste0(almostmod,"/predictions.txt")
    #puts the data into the table 1 model at a time
    tests<- rbind(tests,read.table(modfin,header=TRUE,sep="\t"))
  }
}
#make sure annotations is completely unique
#tests is expected to not be unique, but is tested as well
View(tests)
table(tests$Sample_ID %in% annotations$ARRAY)
table(annotations$ARRAY %in% tests$Sample_ID)
table(duplicated(annotations$ARRAY))
table(duplicated(tests$Sample_ID))

#a new merged table with all the data merged
ant<- merge(tests,annotations,by.x="Sample_ID",by.y="ARRAY", all.x = TRUE, all.y = FALSE)


#create pred and smokestat variables that can cover the array

#calculate a true positive
a<-sum(ant$Prediction==1&ant$SmokingStatus==1)
as<-paste0("true positive: ",a)
print(as)

#calculate a false negative
b<-sum(ant$Prediction==0&ant$SmokingStatus==1)
bs<-paste0("false negative: ",b)
print(bs)

#calculate a false positive
c<-sum(ant$Prediction==1&ant$SmokingStatus==2)
cs<-paste0("false positive: ",c)
print(cs)

#calculate a true negative
d<-sum(ant$Prediction==0&ant$SmokingStatus==2)
ds<-paste0("true negative: ",d)
print(ds)

#sensitivity
sens<-a/(a+b)
senstest<-paste0("sensitivity: ",sens)
print(senstest)

#specificity
spec<-d/(c+d)
spectest<-paste0("specificity: ", spec)
print(spectest)












