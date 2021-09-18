#library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)


data <- read.csv(choose.files())

#gender
data$gender <- as.character(data$gender)
data$gender[which(data$gender=="1")] <- "male"
data$gender[which(data$gender=="0")] <- "female"
#data$gender <- as.numeric(data$gender)
ggplot(data, aes(x = gender, y = values, fill=gender)) + 
  geom_violin(trim = FALSE)+
  geom_boxplot(width=0.1)

#symptoms

ggplot(data, aes(x = values, y =symptoms )) + 
  geom_violin(trim = FALSE , fill = "#FFFFB3" , color = "blue" )

#alcohol

data$alcohol <- as.character(data$alcohol)
data$alcohol[which(data$alcohol=="1")] <- "Drink"
data$alcohol[which(data$alcohol=="0")] <- "No Drink"
ggplot(data, aes(x = alcohol, y = values, fill=alcohol)) + 
  geom_violin(trim = FALSE )+
  geom_boxplot(width=0.1)

#hepatitis.b.surface.antigen

ggplot(data, aes(x = values, y = hepatitis.b.surface.antigen )) + 
  geom_violin( trim = FALSE, fill = "#BEBADA" , color = "red")

#hepatitis.b.e.antigen

ggplot(data, aes(x = values, y = hepatitis.b.e.antigen )) + 
  geom_violin(trim =FALSE, fill = "#8DD3C7" , color = "blue")

#hepatitis.b.core.antibody

ggplot(data, aes(x = values, y = hepatitis.b.core.antibody)) + 
  geom_violin(trim = FALSE , fill = "#FF0000FF" ,color = "black")

#hepatitis.c.virus.antibody

ggplot(data, aes(x = values, y = hepatitis.c.virus.antibody)) + 
  geom_violin(trim = FALSE , fill = "#FF8000FF" , color = "blue")

#cirrhosis

data$cirrhosis <- as.character(data$cirrhosis)
data$cirrhosis[which(data$cirrhosis=="1")] <- "Cirrhosis"
data$cirrhosis[which(data$cirrhosis=="0")] <- "No Cirrhosis"
ggplot(data, aes(y = values, x = cirrhosis, fill = cirrhosis )) + 
  geom_violin(trim = FALSE  )+
  geom_boxplot(width = 0.1)

#endemic.countries

ggplot(data, aes(x = values , y = endemic.countries)) +
  geom_violin(trim = FALSE , fill = "#FFFF80FF", color = "black")

#smoking

data$smoking <- as.character(data$smoking)
data$smoking[which(data$smoking=="1")] <- "Smoking"
data$smoking[which(data$smoking=="0")] <- "No Smoking"
#data$smoking[which(data$smoking=="0.508064516129032")] <- "Smoking"
ggplot(data, aes(y = values, x = smoking , fill = smoking))+
  geom_violin(trim = FALSE )+
  geom_boxplot(width = 0.1)

#diabetes

data$diabetes <- as.character(data$diabetes)
data$diabetes[which(data$diabetes=="1")] <- "Diabetes"
data$diabetes[which(data$diabetes=="0")] <- "No Diabetes"
#data$diabetes[which(data$diabetes=="0.345679012345679")] <- "Diabetes"
ggplot(data, aes(y = values, x = diabetes , fill = diabetes))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = 0.1)

#obesity

data$obesity <- as.character(data$obesity)
data$obesity[which(data$obesity=="1")] <- "Obesity"
data$obesity[which(data$obesity=="0")] <- "No Obesity"
#data$obesity[which(data$obesity=="0.129032258064516")] <- "besity"
ggplot(data, aes(y = values, x = obesity , fill = obesity))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = 0.1)

#hemochromatosis

data$hemochromatosis <- as.character(data$hemochromatosis)
data$hemochromatosis[which(data$hemochromatosis=="1")] <- "Hemochromatosis"
data$hemochromatosis[which(data$hemochromatosis=="0")] <- "No Hemochromatosis"
ggplot(data , aes(y = values , x = hemochromatosis , fill = hemochromatosis ))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = 0.1)

#X.arterial.hypertension

data$X.arterial.hypertension <- as.character(data$X.arterial.hypertension)
data$X.arterial.hypertension[which(data$X.arterial.hypertension=="1")] <- "X.Arterial.Hypertension"
data$X.arterial.hypertension[which(data$X.arterial.hypertension=="0")] <- "No X.Arterial.Hypertension"
ggplot(data , aes(y = values , x = X.arterial.hypertension , fill = X.arterial.hypertension))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = 0.1)

#X.chronic.renal.insufficiency

data$X.chronic.renal.insufficiency <- as.character(data$X.chronic.renal.insufficiency)
data$X.chronic.renal.insufficiency[which(data$X.chronic.renal.insufficiency=="1")] <- "X.Chronic.renal.insufficiency"
data$X.chronic.renal.insufficiency[which(data$X.chronic.renal.insufficiency=="0")] <- "No X.Chronic.renal.insufficiency"
ggplot(data , aes(y = values , x = X.chronic.renal.insufficiency , fill = X.chronic.renal.insufficiency))+
  geom_violin(trim = FALSE)
geom_boxplot(width = 0.1)

#human.immunodeficiency.virus

data$human.immunodeficiency.virus <- as.character(data$human.immunodeficiency.virus)
data$human.immunodeficiency.virus[which(data$human.immunodeficiency.virus=="1")] <- "human.immunodeficiency.virus"
data$human.immunodeficiency.virus[which(data$human.immunodeficiency.virus=="0")] <- "No human.immunodeficiency.virus"
ggplot(data , aes(y = values , x = human.immunodeficiency.virus , fill = human.immunodeficiency.virus))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = 0.1)

#nonalcoholic.steatohepatitis.

data$nonalcoholic.steatohepatitis. <- as.character(data$nonalcoholic.steatohepatitis.)
data$nonalcoholic.steatohepatitis.[which(data$nonalcoholic.steatohepatitis.=="1")] <- "nonalcoholic.steatohepatitis."
data$nonalcoholic.steatohepatitis.[which(data$nonalcoholic.steatohepatitis.=="0")] <- "Alcoholic.steatohepatitis."
ggplot(data, aes(y = values , x = nonalcoholic.steatohepatitis. , fill = nonalcoholic.steatohepatitis.))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = 0.1)

#X.esophageal.varices

data$X.esophageal.varices <- as.character(data$X.esophageal.varices)
data$X.esophageal.varices[which(data$X.esophageal.varices=="1")] <- "X.esophageal.varices"
data$X.esophageal.varices[which(data$X.esophageal.varices=="0")] <- "NO X.esophageal.varices"
ggplot(data, aes(y = values , x = X.esophageal.varices , fill = X.esophageal.varices))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = 0.1)

#splenomegaly

data$splenomegaly <- as.character(data$splenomegaly)
data$splenomegaly[which(data$splenomegaly=="1")] <- "splenomegaly"
data$splenomegaly[which(data$splenomegaly=="0")] <- "NO splenomegaly"
ggplot(data, aes(y = values , x = splenomegaly , fill = splenomegaly))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = 0.1)

#portal.hypertension

data$portal.hypertension <- as.character(data$portal.hypertension)
data$portal.hypertension[which(data$portal.hypertension=="1")] <- "portal.hypertension"
data$portal.hypertension[which(data$portal.hypertension=="0")] <- "NO portal.hypertension"
ggplot(data, aes(y = values , x = portal.hypertension , fill = portal.hypertension))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = 0.1)

#portal.vein.thrombosis

data$portal.vein.thrombosis <- as.character(data$portal.vein.thrombosis)
data$portal.vein.thrombosis[which(data$portal.vein.thrombosis=="1")] <- "portal.vein.thrombosis"
data$portal.vein.thrombosis[which(data$portal.vein.thrombosis=="0")] <- "NO portal.vein.thrombosis"
ggplot(data, aes(y = values , x = portal.vein.thrombosis , fill = portal.vein.thrombosis))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = 0.1)

#liver.metastasis

data$liver.metastasis <- as.character(data$liver.metastasis)
data$liver.metastasis[which(data$liver.metastasis=="1")] <- "liver.metastasis"
data$liver.metastasis[which(data$liver.metastasis=="0")] <- "NO liver.metastasis"

ggplot(data, aes(y = values , x = liver.metastasis , fill = liver.metastasis))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = 0.1)

#radiological.hallmark.

data$radiological.hallmark. <- as.character(data$radiological.hallmark.)
data$radiological.hallmark.[which(data$radiological.hallmark.=="1")] <- "radiological.hallmark."
data$radiological.hallmark.[which(data$radiological.hallmark.=="0")] <- "NO radiological.hallmark."
ggplot(data, aes(y = values , x = radiological.hallmark. ,fill =  radiological.hallmark.))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = 0.1)

#age

ggplot(data, aes(x = values , y = age , fill = age))+
  geom_violin(trim = FALSE , fill = "#FFFFB3" , color = "blue")
#  geom_boxplot(width = 0.1)

#grams.of.alcohol.per.day

ggplot(data, aes(x = values , y = grams.of.alcohol.per.day ))+
  geom_violin(trim = FALSE , fill = "#FFFFB3" , color = "blue")

#packs.of.cigarets.per.year

ggplot(data, aes(x = values , y = packs.of.cigarets.per.year))+
  geom_violin(trim = FALSE , fill = "#FFFF80FF", color = "black")

#performance.status

ggplot(data, aes(y = values , x = performance.status ))+
  geom_violin(trim = FALSE , fill = "#BEBADA" , color = "red")

#encephalopathy.degree

ggplot(data, aes(x = values , y = encephalopathy.degree))+
  geom_violin(trim = FALSE , fill = "#FFFF80FF" , color = "black")

#ascites.degree

ggplot(data, aes(x = values , y = ascites.degree))+
  geom_violin(trim = FALSE , fill = "#FFFF80FF" , color = "black")

#international.normalised.ratio

ggplot(data, aes(x = values , y = international.normalised.ratio))+
  geom_violin(trim = FALSE , fill = "#FF8000FF" , color = "blue")

#alpha.fetoprotein

ggplot(data, aes(x = values , y = alpha.fetoprotein))+
  geom_violin(trim = FALSE , fill = "#FF8000FF" , color = "blue")

#haemoglobin

ggplot(data, aes(x = values , y = haemoglobin))+
  geom_violin(trim = FALSE , fill = "#FFFF80FF" , color = "black")

#mean.corpuscular.volume

ggplot(data, aes(x = values , y = mean.corpuscular.volume))+
  geom_violin(trim = FALSE , fill = "#FFFF80FF" , color = "black")

#leukocytes

ggplot(data, aes(x = values , y = leukocytes))+
  geom_violin(trim = FALSE , fill = "#FF8000FF" , color = "blue")

#platelets

ggplot(data, aes(x = values , y = platelets))+
  geom_violin(trim = FALSE , fill = "#FF8000FF" , color = "blue")

#albumin

ggplot(data, aes(x = values , y = albumin))+
  geom_violin(trim = FALSE , fill = "#FF8000FF" , color = "blue")

#total.bilirubin

ggplot(data, aes(x = values , y = total.bilirubin))+
  geom_violin(trim = FALSE , fill = "#FFFF80FF" , color = "black")

#alanine.transaminase

ggplot(data, aes(x = values , y = alanine.transaminase))+
  geom_violin(trim = FALSE , fill = "#FFFF80FF" , color = "black")

#gamma.glutamyl.transferase

ggplot(data, aes(x = values , y = gamma.glutamyl.transferase))+
  geom_violin(trim = FALSE , fill = "#FF8000FF" , color = "blue")

#alkaline.phosphatase

ggplot(data, aes(x = values , y = alkaline.phosphatase))+
  geom_violin(trim = FALSE , fill = "#FF8000FF" , color = "blue")

#total.proteins

ggplot(data, aes(x = values , y = total.proteins))+
  geom_violin(trim = FALSE , fill = "#FF8000FF" , color = "blue")

#creatinine

ggplot(data, aes(x = values , y = creatinine))+
  geom_violin(trim = FALSE ,fill = "#FF8000FF" , color = "blue")

#number.of.nodules

ggplot(data, aes(x = values , y = number.of.nodules))+
  geom_violin(trim = FALSE ,fill = "#FF8000FF" , color = "blue")

#major.dimension.of.nodule.cm

ggplot(data, aes(x = values , y = major.dimension.of.nodule.cm))+
  geom_violin(trim = FALSE ,fill = "#FF8000FF" , color = "blue")

#iron

ggplot(data, aes(x = values , y = iron))+
  geom_violin(trim = FALSE ,fill = "#FF8000FF" , color = "blue")  

#oxygen.saturation..

ggplot(data, aes(x = values , y = oxygen.saturation..))+
  geom_violin(trim = FALSE ,fill = "#FF8000FF" , color = "blue")  

#ferritin

ggplot(data, aes(x = values , y = ferritin))+
  geom_violin(trim = FALSE ,fill = "#FF8000FF" , color = "blue")  

#class.attribute

ggplot(data , aes(x = values , y = class.attribute))+
  geom_violin(trim = FALSE ,fill = "#FF8000FF" , color = "blue")
