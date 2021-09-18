#loding the library

library(mice)
library(lattice)
library(caret)
library(psych)
library(Amelia)
library(GGally)
library(rpart)
library(VIM)

# package that holds the Naive Bayes function
library(e1071)
set.seed(123)

"endemic.countries","smoking","diabetes","obesity","hemochromatosis","X.arterial.hypertension",
"X.chronic.renal.insufficiency","human.immunodeficiency.virus","nonalcoholic.steatohepatitis.",
"X.esophageal.varices","splenomegaly","portal.hypertension","portal.vein.thrombosis",
"liver.metastasis","radiological.hallmark."

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
#Setting outcome variables as categorical
data$class.attribute <- factor(data$class.attribute, levels = c(0,1), labels = c("False", "True"))




# devideing the data into a trianing set and a testing set 

#indxTrain <- createDataPartition(y = data$class.attribute,p = 0.75,list = FALSE)
#traindata <- data[indxTrain,]
#testdata <- data[-indxTrain,]

#shuffling data
n <- nrow(data)
shuffled <- data[sample(n),]
train <- shuffled[1:round(0.7 * n),]  #70% train
test <- shuffled[(round(0.7 * n) + 1):n,]   #30% test

# Initialize the accs vector
accs <- rep(0,10)

for (i in 1:10) {
  # These indices indicate the interval of the test set.....10 folds
  indices <- (((i-1) * round((1/10)*nrow(shuffled))) + 1):((i*round((1/10) * nrow(shuffled))))
}
# Exclude them from the train set
train <- shuffled[-indices,]

# Include them in the test set
test <- shuffled[indices,]

#Check dimensions of the split

prop.table(table(data$class.attribute)) * 100
prop.table(table(train$class.attribute)) * 100
prop.table(table(test$class.attribute)) * 100

#create objects x which holds the predictor variables and y which holds the response variables
x = train[,-50]
y = train$class.attribute

# the NB model

model = train(x,y,'nb' ,trControl=trainControl(method='cv',number=10))

model

#Model Evaluation
#Predict testing set

Predict <- predict(model,newdata = test )

#Get the confusion matrix to see accuracy value and other parameter values

confusionMatrix(Predict, test$class.attribute )

#Plot Variable performance
X <- varImp(model)
plot(X)

