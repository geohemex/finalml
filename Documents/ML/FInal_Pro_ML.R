##Download Data
setwd("C://Users//Geovanni//Documents")
getwd()
urltr<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlts<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if(!file.exists("./ML")){
  dir.create("./ML")
}
setwd("./ML")
if(!file.exists("training.csv") | !file.exists("testing.csv")){
  file.tr <- "training.csv"
  file.ts <- "testing.csv"
  download.file(urltr,file.tr) 
  download.file(urlts,file.ts)
}

tra<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
tes<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

##Loading Data
training<-read.csv(tra, na.strings=c("NA","#DIV/0!",""),  sep=",", stringsAsFactors = F)
testing<-read.csv(file.ts, na.strings=c("NA","#DIV/0!",""),  sep=",", stringsAsFactors = F)
sum(training=='#DIV/0', na.rm=TRUE)
sum(training=='', na.rm=TRUE)
sum(testing=='#DIV/0', na.rm=TRUE)
sum(testing=='', na.rm=TRUE)
names(training)

#Create Data Partition
library(caret)
set.seed(1012)
inTrain<-createDataPartition(training$classe, p=0.7, list=F)
Training<-training[inTrain,]
Testing<-training[-inTrain,]
nz<-nearZeroVar(Training)
Training<-Training[,-nz]
Testing<-Testing[,-nz]
Training<-Training[ , colSums(is.na(Training)) == 0]
Testing<-Testing[ , colSums(is.na(Testing)) == 0]
Training<-Training[,-c(1,2,3,4,5)]
Testing<-Testing[,-c(1,2,3,4,5)]
names(Training)


#Decision Tree
library(rpart)
mod.Tree<-train(classe~.,method="rpart", data=Training)
library(rattle)
fancyRpartPlot(mod.Tree$finalModel)
mod.tree.predict<-predict(mod.Tree, Testing)
confusionMatrix(mod.tree.predict, Testing$classe)$overall[1]
predict(mod.Tree, newdata=testing)

#Random Forest
set.seed(1012)
trcr <- trainControl(method="boot", number=2, verboseIter=FALSE)
mod.RF <- train(classe~ ., data=Training, method="rf", trControl = trcr)
mod.RF$finalModel
mod.RF.predict<-predict(mod.RF, Testing)
confusionMatrix(mod.RF.predict, Testing$classe)$overall[1]
pred<-predict(mod.RF, newdata=testing)
print(pred)
