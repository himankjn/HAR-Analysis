  library(caret)
  set.seed(123)
  trainset<-read.csv("C:/Users/himank/Documents/R/ml course proj/pml-training.csv")
  testing<-read.csv("C:/Users/himank/Documents/R/ml course proj/pml-testing.csv")
trainset
dim(trainset)
ncol(trainset)
str(trainset)
trainset$classe

table(trainset$classe)

partition<-createDataPartition(y=trainset$classe,p=0.75,list=F)
training<-trainset[partition,]
validation<-trainset[-partition,]




names(training)

training<-training[,-nearZeroVar(training)]
#removing descriptive cols:
Cl<-c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
  "cvtd_timestamp", "new_window", "num_window")

training <- training[,!names(training) %in% Cl]
names(training)
#removing cols with more than 80%
training[training==""] <- NA
naproportion<-sapply(training,function(x) sum(is.na(x))/nrow(training))
training<-training[!(naproportion>0.60)]
names(training)

#randomForest

modelrf<-train(classe~.,data=training,method='rf',ntree=10)
predictrf<-predict(modelrf,training)
print(confusionMatrix(predictrf,training$classe))


#decision tree
library(rattle)
modelrp<-train(classe~.,data=training,method='rpart')
predictrp<-predict(modelrp,training)
print(confusionMatrix(predictrp,training$classe))
fancyRpartPlot(modelrp$finalModel)
