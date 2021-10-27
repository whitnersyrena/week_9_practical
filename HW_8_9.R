### Sam Shedd and I worked on this assignment together!

### libraries
library(tidyverse)
## haha that this worked at my office desktop and not my laptop :') 

### Q1 Load the data into a variable called survey_data
survey_data = read.csv("/Users/Syrena/Desktop/UH_classes/MBIO612/W9/RIKZ.txt", sep="\t", header=TRUE)

### Q2 display the first 6 lines of the tables
head(survey_data)

### Q3 The columns C1 P1-P25, N1, CR1-28, M1-17 and I1-5 of the table represent the 
survey_data$richness = rowSums(survey_data[,2:76]>0)

### Q4 Create a copy of the variable survey_data that does not have columns C1 P1-P25 
survey_data_richness = survey_data[,77:90]

### Q6 Use the lm function to model the richness as a function of the remaining variables 
lm_richness = lm(richness~., data = survey_data_richness[,2:14])

summary(lm(richness~angle1, data = survey_data_richness))
summary(lm(richness~angle2, data = survey_data_richness))
summary(lm(richness~exposure, data = survey_data_richness))
summary(lm(richness~salinity, data = survey_data_richness))
summary(lm(richness~temperature, data = survey_data_richness))
summary(lm(richness~NAP, data = survey_data_richness))
summary(lm(richness~penetrability, data = survey_data_richness))
summary(lm(richness~grainsize, data = survey_data_richness))
summary(lm(richness~humus, data = survey_data_richness))
summary(lm(richness~chalk, data = survey_data_richness))
summary(lm(richness~sorting1, data = survey_data_richness))

ggplot(survey_data_richness) +
  geom_point(aes(x=angle1, y=richness)) +
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=angle2, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=exposure, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=salinity, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=temperature, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=NAP, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=penetrability, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=grainsize, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=humus, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=chalk, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=sorting1, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=Beach, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

### Q7 What do the various output of the lm mean? Interpret the results of your model
lm_richness_NAP = lm(richness~I(NAP), data = survey_data_richness)
summary(lm_richness_NAP)
ggplot(survey_data_richness) +
  geom_point(aes(x=NAP, y=richness)) + 
  geom_abline(intercept = lm_richness_NAP$coefficients[1], slope = lm_richness_NAP$coefficients[2], color = 2, size=2)

sum(residuals(lm_richness_NAP))

ggplot()+
  geom_density(aes(x=residuals(lm_richness_NAP)), bw=2)+
  xlim(-5,5)

### Q8 Build a model that includes all the parameters and assess the fit of the data
lm_richness = lm(richness~., data = survey_data_richness[,2:14])
summary(lm_richness)

### Q9 Use an appropriate method that only selects a subset of the data 
survey_data_no_outliers = survey_data_richness[-c(9,22),]
lm_richness_NAP = lm(richness~I(NAP), data = survey_data_richness[-c(9,22),])
summary(lm_richness_NAP)
ggplot(survey_data_no_outliers) +
  geom_point(aes(x=NAP, y=richness)) + 
  geom_abline(intercept = lm_richness_NAP$coefficients[1], slope = lm_richness_NAP$coefficients[2], color = 2, size=2)

### the R^2 value is 40% so I can't say with confidence that this model is best fitted for the data - this number should be much higher correct? You want a high percentage of the points to be explainable by the model? 
### Relationship is stastically significant - p-value is rather small at 3.18e-6
### t-value is pretty high so I assume these populations are quite different, suported by p-value as well 
### also NAP is pretty low, which I BELIEVE (not sure if I am getting this right) that this means there is not a high overlap in values, once again supporting the poopulations are indeed different? 
