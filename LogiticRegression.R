# Diabetes dataset

#1st Step: Data Aquisiton

data<-read.csv("D:/Data Science/Machine Learning/Shakti/Dataset/diabetes.csv")
View(data)


#2nd Step: Dividing Dataset
library(caTools)
set.seed(2)
split<-sample.split(data,splitRatio=0.8)
split
training_data<-subset(data,split==TRUE)
testing_data<-subset(data,split==FALSE)


#3rd Step: Build Model

model<-glm(Outcome~.,training_data,family = 'binomial')
summary(model)


#4th Step: Optimize Model

model<-glm(Outcome~.-DiabetesPedigreeFunction-SkinThickness-Insulin-Age, training_data,family="binomial")
summary(model)


#5th Step: Prediction 

res<-predict(model,testing_data,type = "response")
res

#6th Step: Check Accuracy

res<-predict(model,testing_data,type = "response")
res

##Confusion Matrix

table(Actualvalue=testing_data$Outcome,Predictedvalue=res>0.5)

##FalsePositiveRate is high therefore we will chk threshold

##Roc Curve for checking threshold

library(ROCR)
res<-predict(model,training_data,type = "response")
ROCRPred<-prediction(res,training_data$Outcome)
ROCRPref<-performance(ROCRPred,"tpr","fpr")
plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))



##Accuracy Check(After Setting Threshold=0.4)

res<-predict(model,testing_data,type = "response")
res

##Confusion Matrix

table(Actualvalue=testing_data$Outcome,Predictedvalue=res>0.4)






