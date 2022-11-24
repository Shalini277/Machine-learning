
##Libraries used
library(caret)
library(car)
library(dplyr)
library(ggplot2)
library(fastDummies)
library(ROSE)
library(pROC)
##Import data into r
Complete_data<-read.csv("clipboard",sep = "\t",header = TRUE,stringsAsFactors = TRUE)
Hdata1<-Complete_data

## Data clearing
##Check for NAs in the dataset
colSums(is.na(Hdata1))

##Convert categorical field to factors 
str(Hdata1)
Hdata1$NPS_Status<-as.factor(Hdata1$NPS_Status)
levels(Hdata1$NPS_Status)

##Remove the field not relevant for model
Hdata1<-Hdata1[,-c(1,2,3,4,5,9:11,47,48,50)]
str(Hdata1)

##Model using Logistic Regression
LRModel1<-glm(NPS_Status~.,data=Hdata1,family = "binomial")
summary(LRModel1)
LRModel2<-step(LRModel1,trace = 0)
summary(LRModel2)

##Split the data into train and test
set.seed(1234)
Index<-createDataPartition(Hdata1$NPS_Status, p=.80, list=FALSE)
Train<-Hdata1[Index,]
Test<-Hdata1[-Index,]
prop.table(table(Hdata1$NPS_Status))
prop.table(table(Train$NPS_Status))
prop.table(table(Test$NPS_Status))

##Prediction model
LRModelP1<-glm(NPS_Status~.,data = Train, family="binomial")
summary(LRModelP1)
LRModelP2<-step(LRModelP1,trace=0)
summary(LRModelP2)

##Evaluate the model using the test data
Predicted<-predict(LRModelP2, newdata = Test, type="response")
Predicted
Test$Predicted<-Predicted
Test$Class<-ifelse(Test$Predicted>=0.5,1,0)
str(Test$Class)
Test$Actual<-ifelse(Test$NPS_Status=="Promotor",1,0)
Test$Class<-as.factor(Test$Class)
Test$Actual<-factor(Test$Actual)

##Find the sensitivity and specificity 
?confusionMatrix
confusionMatrix(Test$Class, Test$Actual,positive = "1")

##Check the Presecion, rcall and accuracy to check the data imbalance
accuracy.meas(Test$NPS_Status,Predicted)

##Find ROC and AUC
par(pty="s")
roc(Test$Actual,Predicted, plot=TRUE, print.auc=TRUE,legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
    ylab="True Positive Percentage", col="#de2d26",lwd=4)
legend("bottomright", legend = "Logistic Regression", col="#de2d26",lwd=4)

##Create a data frame with TPP, FPP and Threshold
roc.info<-roc(Test$Actual,Predicted, plot=TRUE, legacy.axes=TRUE)
roc.df<-data.frame(TPP=roc.info$sensitivities*100,FPP=(1-roc.info$specificities)*100,Threshold=roc.info$thresholds)

###-----------------------------------------------------------------------------------------------------------
##Model using decision tree

## Libraries used of decesion  tree
library(caTools)
library(party)
library(magrittr)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
##Import data into R
Ddata1<-Complete_data
Ddata1<-Ddata1[,-c(1,2,3,4,5,9:11,47,48,50)]
str(Ddata1)
Ddata1$NPS_Status<-as.factor(Ddata1$NPS_Status)

##Split the data into test and train
set.seed(1234)
DIndex<-sample.split(Ddata1, SplitRatio = 0.8)
DTrain<-subset(Ddata1,DIndex==TRUE)
DTest<-subset(Ddata1,DIndex==FALSE)

##Build the Model using train data
Dmodel<-ctree(NPS_Status~.,DTrain)
plot(Dmodel)

##Evaluate the model using test data
PredictD <- predict(Dmodel, DTest)   
DTest$Actual<-ifelse(DTest$NPS_Status=="Promotor",1,0)
DTest$Actual<-as.factor(DTest$Actual)
DTest$Predicted<-PredictD
DTest$Pre_Value<-ifelse(DTest$Predicted=="Promotor",1,0)
DTest$Predicted<-as.factor(DTest$Predicted)


##Create the confusion matrix
confusionMatrix(DTest$NPS_Status,PredictD)

##Compare ROC and AUC for logistic regression and Decision tree
par(pty="s")
roc(Test$Actual,Predicted, plot=TRUE, print.auc=TRUE,legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
    ylab="True Positive Percentage", col="#de2d26",lwd=4)
plot.roc(DTest$Actual, DTest$Pre_Value, percent=TRUE, col="#31a354", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=30)
legend("bottomright", legend = c("Logistic Regression", "Descision Tree"), col=c("#de2d26","#31a354"),lwd=4)

##Check for data imbalance
table(DTrain$NPS_Status)
prop.table(table(DTrain$NPS_Status))       
accuracy.meas(DTest$NPS_Status,PredictD)
##----------------------------------------------------------------------------------------------------------------
##### Random forest model
Rdata1<-Complete_data
Rdata1<-Rdata1[,-c(1,2,3,4,5,9:11,47,48,50)]
str(Rdata1)
Rdata1$NPS_Status<-as.factor(Rdata1$NPS_Status)

##Split the data into test and train
set.seed(1234)
RIndex<-sample.split(Rdata1, SplitRatio = 0.8)
RTrain<-subset(Rdata1,RIndex==TRUE)
RTest<-subset(Rdata1,RIndex==FALSE)

## Create a model
t<-tuneRF(RTrain[,-32], RTrain[,32],
          stepfactor=0.5,
          plot=TRUE,
          ntreeTry=300,
          trace = TRUE,
          improve = 0.05)

RModel<-randomForest(NPS_Status~.,data=RTrain,
                     ntree=300,
                     mtry=3,
                     importance=TRUE,
                     proximity=TRUE)
RModel

##Evaluate the model using Test data
PredictR<-predict(RModel,RTest)

##Confusion Matrix
confusionMatrix(PredictR,RTest$NPS_Status)
plot(RModel)
RTest$Predicted<-PredictR
RTest$Predicted_Value<-ifelse(RTest$Predicted=="Promotor",1,0)
RTest$Actual<-ifelse(RTest$NPS_Status=="Promotor",1,0)

##Which variable plays important role in the model
varImpPlot(RModel,
           Sort=T,
           n.var = 10,
           main = "Top 10 Important Variables")
##Compare ROC and AUC for Logistic Regression, Decision Tree, and Random Forest
par(pty="s")
roc(Test$Actual,Predicted, plot=TRUE, print.auc=TRUE,legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
    ylab="True Positive Percentage", col="#de2d26",lwd=4)
plot.roc(DTest$Actual, DTest$Pre_Value, percent=TRUE, col="#31a354", lwd=4, print.auc=TRUE,add=TRUE, print.auc.y=30)
plot.roc(RTest$NPS_Status,RTest$Predicted_Value,percent=TRUE, col="#756bb1", lwd=4, print.auc=TRUE,add=TRUE, print.auc.y=40)
legend("bottomright", legend = c("Logistic Regression", "Descision Tree", "Random Forest"), col=c("#de2d26","#31a354","#756bb1"),lwd=4)

##-------------------------------------------------------------------------------------------------------------
##boosting
Xdata1<-Complete_data
Xdata1<-Xdata1[,-c(1,2,3,4,5,9:11,47,48,50)]
str(Rdata1)
Rdata1$NPS_Status<-as.factor(Rdata1$NPS_Status)




