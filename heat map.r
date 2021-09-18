library(mice)
library(lattice)

#lodeing the dataset
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
data <- data[order(data$PTS),]
row.names(data) <- data$Name
data <- data[,2:20]
data_matrix <- data.matrix(data)
data_heatmap <- heatmap(data_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))

data_heatmap <- heatmap(data_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))