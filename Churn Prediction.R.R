#clearing the directory
rm(list=ls(all=T))

#setting the working directory
setwd("C:\\Users\\mvsvs\\Desktop\\insofe material\\PROJECTS\\churn data\\data")

#reading the data
churn_data<-read.csv("Data.csv", header=T, sep=',',na.strings = c("?","NA","NULL"))

#checking missing values
sum(is.na(churn_data))
#zero missing values

#understanding of data
summary(churn_data)
dim(churn_data) #give the dimensions of the data
str(churn_data) #shows the structure of the data
nrow(churn_data)#25000 rows
ncol(churn_data)#111 attributes

#quantiles for more understanding of data
quantiles <-data.frame(apply(churn_data,2,function(x) {quantile(x)}))
quantiles


##checking unique values
apply(churn_data,2,function(x){length(unique(x))})
churn_data$target

#type convertions
churn_data$target<-as.factor(churn_data$target)
churn_data$target

#moving target attribute to the end
churn_data = churn_data[,c(setdiff(names(churn_data),"target"),"target")]

#visualization of data
#plotting histograms for first fifty attribute
par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(5,10))
for(i in 1:50) {
  hist(churn_data[,i], main=names(churn_data)[i])
}
#plotting histograms for remaining attributes
par(mfrow=c(5,13))
for(i in 51:111) {
  hist(churn_data[,i], main=names(churn_data)[i])
}

#install.packages("corrgram")
library(corrgram)

corrgram(churn_data, order=NULL, panel=panel.shade, text.panel=panel.txt,
         main="Correlogram")


##boxplot###
boxplot(churn_data)
#ploting outliers
LogReg <- glm(target ~., data=churn_data,family=binomial)
summary(LogReg)

#install.packages("car")
install.packages("pbkrtest")
install.packages("lme4")
install.packages("car")
library(car)
outliers<-influenceIndexPlot(LogReg,vars = c("cook"),id.n = 5)
#removing outliers from the data  
churn_data1 <- churn_data[-c(5821,10433,14739,15054,23022),]
names(churn_data)
boxplot(churn_data1)

##removing outliers
install.packages("outliers")
library(outliers)
churn_data2 <- churn_data[,c(1:110)]
churn_data2 = rm.outlier(churn_data2,fill = TRUE) 
boxplot(churn_data2)
names(churn_data2)
churn_data2$target = churn_data$target
names(churn_data2)

#looking at points which are outside the boxplot
outliers = apply(churn_data2[,c(1:110)],2,function (x) boxplot(x)$out)
outlier_data = lapply(churn_data2[,c(1:110)],function (x) boxplot(x)$out)
lapply(outlier_data,length) 
# some columns have many outliers
# we cannot consider them as outliers
# the data is skewed

#standardizing the data Using range method
library(vegan)
standardized_data <- churn_data2[,-c(111)] # removing target attribute
standardized_data <- decostand(standardized_data,"range")
standardized_data

#attaching the target attribute
standardized_data$target<-churn_data2$target
names(standardized_data)


#checking multicollinearity
library("VIF")
names(standardized_data)
churnfinal<-data.frame(subset(standardized_data[-c(111)]))
xyz = vif(as.numeric(standardized_data$target),churnfinal)
str(xyz)
y=data.frame(xyz$modelmatrix)
View(y)
data_vif = data.frame(y,standardized_data$target)
dim(data_vif)
names(data_vif)
---------------------------------------------------------------------------------
# checking important attributes by using random forests
install.packages("randomForest")
library(randomForest)
data_rf <- randomForest(standardized_data.target ~., data=data_vif, keep.forest=TRUE, ntree=36) 
summary(data_rf)
print(data_rf)
names(data_rf)
data_rf$predicted
data_rf$fitted
data_rf$importance
round(importance(data_rf), 2) 

# Extract and store important variables obtained from the random forest model
data_rf <- data.frame(data_rf$importance)
data_rf <- data.frame(row.names(data_rf),data_rf[,1])
colnames(data_rf) = c('Attributes','Importance')
data_rf1 <- data_rf[order(data_rf$Importance , decreasing = TRUE),]
data_rf1
dim(data_rf1)
data_rf <- data_rf1[1:20,]
names(data_rf)
data_rf$Attributes
#selecting the attributes with highest mean decrease genefrom random forest
data_rf$Attributes%in%names(data_vif)

#subsetting data with selected variables               
data_newrf <- churn_data1[data_rf$Attributes]
data_newrf$target <- churn_data1$target 
names(data_newrf)

################
#install.packages("ROSE")
## for class imbalance
library(ROSE)
data_new1 <- ROSE(target ~ ., data = data_newrf, seed = 1)$data
table(data_new1$target)
str(data_new1)
names(data_new1)

###############
# splitting of data into train and test
rows<-seq(1,nrow(data_new1),1)
set.seed(1129)
trainrows<-sample(rows,0.7*nrow(data_new1))
Train<-data_new1[trainrows,]
Test<-data_new1[-trainrows,]
names(Train)

#----------------------model building-----------------------------------------
## 1) random forest
library(randomForest)
random_forest <- randomForest(target ~ ., data=Train, keep.forest=TRUE, ntree=300) 
# View results and understand important attributes
print(random_forest)
random_forest$predicted 
random_forest$importance # gives 1st column (accuracy will reduce if imp var are removed) 
# Predict on Train data 
pred_model_train <-predict(random_forest,Train[,-c(21)],type="response", norm.votes=TRUE)
result_train <- table("actual _values"= Train$target,pred_model_train);result_train
# or table(trainR$target, predict(hepatitis_rf, trainR, type="response", norm.votes=TRUE)) 

# Predicton Test Data
pred_model_test <-predict(random_forest,Test[,-c(21)],type="response", norm.votes=TRUE)
result_test <- table("actual _values"= Test$target,pred_model_test);result_test

# Accuracy,Precision and Recall on test
test_accuracy <- sum(diag(result_test))/sum(result_test)*100;test_accuracy
test_recall <- ((result_test[2,2])/(result_test[2,2]+result_test[2,1])*100);test_recall
test_precision <-((result_test[2,2])/(result_test[2,2]+result_test[1,2])*100);test_precision

---------------------------------------------
## 2)logistic regression
Log12 = glm(target~., data = Train, family=binomial)
summary(Log12)

#Accuracy on the training set
predicttrain = predict(Log12, type="response", newdata=Train)

# Confusion matrix with threshold of 0.5
metrix=table(Train$target, predicttrain > 0.5)

# accuracy,recall and precision on train data
accuracy =(metrix[1,1]+metrix[2,2])/(length(predicttrain));accuracy
Recall = metrix[2,2]/(metrix[2,2]+metrix[2,1]);Recall
Precision = metrix[2,2]/(metrix[2,2]+metrix[1,2]);Precision


# Predictions on the test set
predictTest = predict(Log12, type="response", newdata=Test)

# Confusion matrix with threshold of 0.5
metrix1= table(Test$target, predictTest > 0.5)

# accuracy,recall and precision on test
accuracy =(metrix1[1,1]+metrix1[2,2])/(length(predictTest));accuracy
Recall= metrix1[2,2]/(metrix1[2,2]+metrix1[2,1]);Recall
Precision = metrix1[2,2]/(metrix1[2,2]+metrix1[1,2]);Precision

-----------------------------------------------------
##3) Decision trees 
###building classification model using c50

library(C50)
C50_model =C5.0(target~.,data=Train,rules=T )
summary(C50_model)
#building confusion matrix for C5.0
conf.train=table(Train$target, predict(C50_model, newdata=Train, type="class"))
conf.test=table(Test$target, predict(C50_model, newdata=Test, type="class"))

#compute error matrix on train data
accuracy <- (sum(diag(conf.train))/sum(conf.train))*100;accuracy
recall<-(conf.train[2,2]/(conf.train[2,1]+conf.train[2,2]))*100;recall  
precision<-(conf.train[2,2])/(conf.train[1,2]+conf.train[2,2])*100;precision 

#compute error matrix on test data
accuracy <- (sum(diag(conf.test))/sum(conf.test))*100;accuracy
recall<-(conf.test[2,2]/(conf.test[2,1]+conf.test[2,2]))*100;recall  
precision<-(conf.test[2,2])/(conf.test[1,2]+conf.test[2,2])*100;precision 

-------------------------------------------------------------------------------
## 4) building SVM Model
install.packages("e1071")
library(e1071)
x = subset(Train, select = -target)
y  = as.factor(Train$target)

#tunning svm using grid search 
tuned.svm<- tune.svm(target~.,data = data_new1,cost = c(0.1,0.2,0.3,0.4,0.5),kernel="radial",
                     gamma = c(0.0001,0.001,0.1,0.2,1), tunecontrol=tune.control(sampling = "fix"))

Churn_svm <- svm(x,y, method = "C-classification", kernel = "linear", cost = 0.1) 
Churn_svm1 <- svm(x,y, method = "C-classification", kernel = "radial", cost = 0.5,gamma = 0.2)
#linear kernel
svmpredict.train <- predict(Churn_svm,x)
conf.test <- table(y,svmpredict.train)
conf.test
###building confusion matrix for SVM
a = subset(Test, select = -target)
b1 = as.factor(Test$target)

svmpredict.test <- predict(Churn_svm,a)
conf.test <- table(b1,svmpredict.test)
# accuracy on test data
accuracy <- (sum(diag(conf.test))/sum(conf.test))*100;accuracy
#recall on test data
recall=(conf.test[2,2]/(conf.test[2,1]+conf.test[2,2]))*100;recall 
# precision on test data
precision=(conf.test[2,2])/(conf.test[1,2]+conf.test[2,2])*100;precision  

# radial kernel
svmpredict.train <- predict(Churn_svm1,x)
conf.test <- table(y,svmpredict.train)
conf.test
###building confusion matrix for SVM
a = subset(Test, select = -target)
b1 = as.factor(Test$target)

svmpredict.test <- predict(Churn_svm1,a)
conf.test <- table(b1,svmpredict.test)
# accuracy on test data
accuracy <- (sum(diag(conf.test))/sum(conf.test))*100;accuracy
#recall on test data
recall=(conf.test[2,2]/(conf.test[2,1]+conf.test[2,2]))*100;recall 
# precision on test data
precision=(conf.test[2,2])/(conf.test[1,2]+conf.test[2,2])*100;precision