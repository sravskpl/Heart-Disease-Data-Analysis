#Sravani Korupolu
#Final Project 
#Utilizing Cardiovascular Data Sets to Enable Predictive Diagnostics
library(caret)
library(corrgram)
library (Rmagic)
library (magic)
library(pROC)

#Accessing Data
summary(heart_disease)
heart_disease <- heart_disease[ , !(names(heart_disease) %in% c("id"))]
scaled <- scale(heart_disease)
names(heart_disease) <- c("age", "sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","predicted_attribute")
names(heart_disease)
summary(heart_disease)


#Setting up a function I found online to switch types from numeric 
convert.magic <- function(obj,types){
  out <- lapply(1:length(obj),FUN = function(i){FUN1 <- switch(types[i],character = as.character,numeric = as.numeric,factor = as.factor); FUN1(obj[,i])})
  names(out) <- colnames(obj)
  as.data.frame(out,stringsAsFactors = FALSE)
}


#analyzing correlations between heart disease and variables
cor(heart_disease$predicted_attribute, heart_disease)
corrgram(heart_disease, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Variable Correlations for Heart Disease")


#Raw Data Visualization 
heart_disease$predicted_attribute[heart_disease$predicted_attribute > 0] <- 1
chclass <-c("numeric","factor","factor","numeric","numeric","factor","factor","numeric","factor","numeric","factor","factor","factor","factor")
heart_disease <- convert.magic(heart_disease,chclass)
heart = heart_disease #add labels only for plot
levels(heart$predicted_attribute) = c("No disease","Disease")
levels(heart$sex) = c("female","male","")
mosaicplot(heart$sex ~ heart$predicted_attribute,
           main="Fate by Gender", shade=FALSE,color=TRUE,
           xlab="Gender", ylab="Heart disease")
boxplot(heart$age ~ heart$predicted_attribute, main = "Fate by Age", ylab = "age", xlab = "Heart Disease")


#Training
inTrainRows <- createDataPartition(heart_disease$predicted_attribute,p=0.7,list=FALSE)
trainingData <- heart_disease[inTrainRows,]
testingData <-  heart_disease[-inTrainRows,]
nrow(trainingData)/(nrow(testingData)+nrow(trainingData))

AUC = list()
Accuracy = list()

#Logistic Regression 
set.seed(10)
Model <- train(predicted_attribute ~ ., data = trainingData, method = 'glm', family = 'binomial')
Prediction <- predict(Model, testingData)
Probability <- predict(Model, testingData, type = 'prob')[2]
ConfMat <- confusionMatrix(Prediction, testingData[,"predicted_attribute"])
AUC$logReg <- roc(as.numeric(testingData$predicted_attribute),as.numeric(as.matrix((Probability))))$auc
Accuracy$logReg <- ConfMat$overall['Accuracy']  #found names with str(logRegConfMat)  
Accuracy$logReg

