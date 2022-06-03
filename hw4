install.packages('oddsratio')
install.packages('epiDisplay')
install.packages("epitools")
install.packages("pscl")
install.packages("caret")
install.packages("car")
install.packages("InformationValue")

library("epitools")
library('epiDisplay')
library('oddsratio')
library("pscl")
library("caret")
library("car")
library("InformationValue")

setwd("/Users/bellalee/Documents/OSUCOM/Graduate School/BSGP5750")
getwd()

heart.data <- read.csv("heart.csv", header=TRUE)

## Step Six: Re-code the Variables we will use for our prediction model
## Classify patients as high vs. low cholesterol
heart.data$Chol1 <- ifelse(heart.data$Chol > 240, "High", "Low")

## Change the new variable class to a factor 
heart.data$Chol1 <- factor(heart.data$Chol1)

## Make sure the refrence for the new variable is Low
heart.data$Chol1 <- relevel(heart.data$Chol1, ref="Low")

## Recode the sex, ChestPain and Fbs variables 
## fbs: The person's fasting blood sugar (> 120 mg/dl, 1 = true; 0 = false)
heart.data$Sex <- factor(heart.data$Sex, levels=c(1, 0), labels=c("Male", "Female"))
heart.data$Fbs1 <- factor(heart.data$Fbs, levels=c(1, 0), labels=c("Yes", "No"))
heart.data$ChestPain <- factor(heart.data$ChestPain)

## Re-code the AHD variable into 1 for yes and 0 for no
## this is needed to fit the logisitic regression model 
heart.data$AHD1 <- factor(heart.data$AHD, levels =c("No", "Yes"), labels= c(0, 1))

#################################################################################
#################################################################################
# Use 70% of dataset as training set and remaining 30% as testing set
# For grading, to make sure you get the same results I got, make sure to run the set.seed with the 
# same number (5750). 
## Also note, every time you run the sample function you need to rerun the set.seed first

# Use 70% of dataset as training set and remaining 30% as testing set
set.seed(5750)
sample <- sample(c(TRUE, FALSE), nrow(heart.data), replace=TRUE, prob=c(0.7,0.3))
heart.train <- heart.data[sample, ]
heart.test <- heart.data[!sample, ]  

## fit an adjusted model to the train data to predict AHD1

## Model should include Chol1 + Age + Sex + Oldpeak + RestBP + MaxHR + ChestPain + Fbs1 
train.heart.glm <- glm(formula = AHD1 ~ Chol1 + Age + Sex + Oldpeak + RestBP + MaxHR + ChestPain + Fbs1, 
                       data = heart.train, family = binomial)

## Save your model results to an ojest called "train.heart.glm"
## The following code to get the odds ratios for the model 

logistic.display(train.heart.glm)

pR2(train.heart.glm)["McFadden"]
vif(train.heart.glm)

#calculate probability of AHD for each individual in test dataset
predicted <- predict(train.heart.glm, heart.test, type="response")
# By default  any individual in the test dataset with a probability of AHD 
# greater than 0.5 will be predicted to have AHD
# we can find the optimal probability to use to maximize the accuracy of our model

#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(heart.test$AHD1, predicted)[1]

#calculate sensitivity
sensitivity(heart.test$AHD1, predicted)

#calculate specificity
specificity(heart.test$AHD1, predicted)

#calculate total mis-classification error rate
misClassError(heart.test$AHD1, predicted, threshold=optimal)

# we can plot the ROC (Receiver Operating Characteristic) 
# Curve which displays the percentage of true positives predicted by the model 
# as the prediction probability cutoff is lowered from 1 to 0. 
# The higher the AUC (area under the curve), the more accurately our model is able to predict outcomes

#plot the ROC curve
plotROC(heart.test$AHD1, predicted)
