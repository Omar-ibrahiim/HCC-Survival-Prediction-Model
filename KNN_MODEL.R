#loding the library

library(class)
library(caret)

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
# Normalization min-max scaling 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

for(i in 1:ncol(data)){
  data[,i] <- normalize(data[,i])
}
 
# deviding the dataset into training and testing sets 
devider <- sample(1:nrow(data),size=nrow(data)*0.7,replace = FALSE) #random selection of 70% data.

train_data <- data[devider,] # 70% training data
test_data <- data[-devider,] # remaining 30% test data

# Creating seperate dataframe for 'class attribute' feature which is our target.
# still not sure 
train_data_compare <- data[devider,50]
test_data_compare <-data[-devider,50]

#Find the number of observation
# we want to initialize the value of 'K' in the KNN model.
# One of the ways to find the optimal K value is to 
# calculate the square root of the total number of observations in the data set. 

NROW(train_data_compare)

# the # of observations is 115 the square root is 10.75 so 
# we will use the value of K= 10 , 11 .

knn_10 <- knn(train=train_data, test=test_data, cl=train_data_compare, k=10)
knn_11 <- knn(train=train_data, test=test_data, cl=train_data_compare, k=11)

# model evaluation 
#Calculate the confusion matrix for classification for k = 10 , 11
confusionMatrix(table(knn_10 ,test_data_compare))
confusionMatrix(table(knn_11 ,test_data_compare))
