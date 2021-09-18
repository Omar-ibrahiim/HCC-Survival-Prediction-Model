#loding the library

library(mice)
library(lattice)
library(caret)
library(psych)
library(Amelia)
library(GGally)
library(rpart)

#model of logistics regression
set.seed(123)

#lodeing the dataset
#redfine the symbole '?' to be NA in the dataset

data<-read.csv(choose.files(),header = TRUE ,na.string = "?" )
#convert numeric to factor
data$gender <- as.factor(data$gender)
data$symptoms <- as.factor(data$symptoms)
data$alcohol <- as.factor(data$alcohol)
data$hepatitis.b.surface.antigen <- as.factor(data$hepatitis.b.surface.antigen)
data$hepatitis.b.e.antigen <- as.factor(data$hepatitis.b.e.antigen)
data$hepatitis.b.core.antibody <- as.factor(data$hepatitis.b.core.antibody)
data$hepatitis.c.virus.antibody <- as.factor(data$hepatitis.c.virus.antibody)
data$cirrhosis <- as.factor(data$cirrhosis)
data$endemic.countries <- as.factor(data$endemic.countries)
data$smoking <- as.factor(data$smoking)
data$diabetes <- as.factor(data$diabetes)
data$obesity <- as.factor(data$obesity)
data$hemochromatosis <- as.factor(data$hemochromatosis)
data$X.arterial.hypertension <- as.factor(data$X.arterial.hypertension)
data$X.chronic.renal.insufficiency <- as.factor(data$X.chronic.renal.insufficiency)
data$human.immunodeficiency.virus <- as.factor(data$human.immunodeficiency.virus)
data$nonalcoholic.steatohepatitis. <- as.factor(data$nonalcoholic.steatohepatitis.)
data$X.esophageal.varices <- as.factor(data$X.esophageal.varices)
data$splenomegaly <- as.factor(data$splenomegaly)
data$portal.hypertension <- as.factor(data$portal.hypertension)
data$portal.vein.thrombosis <- as.factor(data$portal.vein.thrombosis)
data$liver.metastasis <- as.factor(data$liver.metastasis)
data$radiological.hallmark. <- as.factor(data$radiological.hallmark.)
data$performance.status <- as.factor(data$performance.status)
data$encephalopathy.degree <- as.factor(data$encephalopathy.degree)
data$ascites.degree <- as.factor(data$ascites.degree)
data$number.of.nodules <- as.factor(data$number.of.nodules)


# replace the missing values with the mean by imputing (for factor data)
impute <- mice (data[,1:50] , m = 3 , seed = 123 ) 
print(impute)
#impute$imp$symptoms
#newdata <- complete(impute , 1)

for(i in 1:ncol(data))
{
  if (is.numeric(data[,i])) {
    data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
  }
  if (is.factor(data[,i])){
    impute$imp$data[,i]   
  }
}
data <- complete(impute , 1)
for (i in 1:ncol(data))
{
  data[,i] <- as.numeric(data[,i])
}


#Setting outcome variables as categorical
data$class.attribute <- factor(data$class.attribute, levels = c(0,1), labels = c("0", "1"))

library(caTools)


# spliting  the data into a trianing set and a testing set 

split =sample.split(data$class.attribute,SplitRatio=0.75)
training_set=subset(data,split==TRUE)
test_set=subset(data,split==FALSE)
#feature scalling
training_set[,   1:49]=scale(training_set[ ,  1:49])
test_set[,   1:49]=scale(test_set[ ,  1:49])
#fitting to training set of logistics regression
classifier = glm(formula=class.attribute ~ .,family=binomial,data =training_set)

#predict the test set results value from set training
prob_pred=predict(classifier,type='response',newdata = test_set[-50])
#y_pred=ifelse(prob_pred>0.5,1,0)
#cm=table(test_set[ , 50],y_pred)
#to get confusion matrix:
y_pred1 <- ifelse(prob_pred> 0.5, 1, 0)
cm=table(test_set[ , 50],y_pred1)
y_pred2 <- factor(y_pred1, levels=c(0, 1))
y_act <- test_set$class.attribute
#to get accuracy:
mean(y_pred2 == y_act)
