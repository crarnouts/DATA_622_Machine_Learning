# You just recently joined a datascience team.
# 
# There are two datasets junk1.txt and junk2.csv
# They have two options
# 1. They can go back to the client and ask for more data
# to remedy problems with the data.
# 2. They can accept the data and undertake a major analytics exercise.
# 
# The team is relying on your dsc skills to determine how they
# should proceed.
# 
# Can you explore the data and recommend actions for each file
# enumerating the reasons.
# 
# grade -- 20


## AUTHOR: Corey Arnouts


df <- read.table("junk.txt",header = T,sep='')

df2 <- read.csv("junk2.csv")


## Logistic Regression with each of the datasets predicting the target variable

library(caret)
library(pROC)


# For dataset df I am turning the class outcome of 2 into 0 so that I can apply classification methods

df$class[df$class==2] <- 0

set.seed(42)

train.index1 <- createDataPartition(df$class, p = .7, list = FALSE)
train_df<- df[ train.index1,]
hold_out_df  <- df[-train.index1,]

logitMod <- glm(class ~ a + b, data=train_df, family=binomial(link="logit"))
summary(logitMod)

hold_out_df$predicted_logistic <- plogis(predict(logitMod, hold_out_df))

roc(hold_out_df[["class"]], hold_out_df[["predicted_logistic"]], plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)


## try a random forest instead

source("https://raw.githubusercontent.com/crarnouts/Data_605_Final/master/RandomForestNulls_testing.R")

hold_out_df <- RF_with_Nulls(train_df,hold_out_df,"class",.5,4,10,.005,5,1)


roc(hold_out_df[["class"]], hold_out_df[["prediction_overall"]], plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)

## It appears the data that is in the junk.txt has some non linearities and interactions between the two variables in the feature space this is
## why the simple 10 tree random forest does better than the logistic model

### Let's try out a Support Vector Machine for this Classification Task
train_df$a <- scale(train_df$a)
train_df$b <- scale(train_df$b)

hold_out_df$a <- scale(hold_out_df$a)
hold_out_df$b <- scale(hold_out_df$b)

library(e1071) 

classifier = svm(formula = class ~ ., 
                 data = train_df, 
                 type = 'C-classification', 
                 kernel = 'linear') 

hold_out_df$svm_prediction<-predict(classifier,newdata = hold_out_df)
hold_out_df$svm_prediction <- as.integer(hold_out_df$svm_prediction)
#ROC Curve for the Support Vector Machine

roc(hold_out_df[["class"]], hold_out_df[["svm_prediction"]], plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)


## What does the SVM look like

# installing library ElemStatLearn 
library(ElemStatLearn) 

# Plotting the training data set results 
set = train_df 
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01) 
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01) 

grid_set = expand.grid(X1, X2) 
colnames(grid_set) = c('a', 'b') 
y_grid = predict(classifier, newdata = grid_set) 

plot(set[, -3], 
     main = 'SVM (Training set)', 
     xlab = 'a', ylab = 'b', 
     xlim = range(X1), ylim = range(X2)) 

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE) 

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine')) 

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3')) 

######################################### CONCLUSION FOR JUNK.txt File ###########################################################################
# The data in the junk.txt file can not be predicted well with a logistic regression model but the 10 tree random forest model did a much better job
# at predicting the class variable. This shows that the dataset does have non linear and interaction patterns in the feature space. For this dataset
# I think it would be better to go back and ask for some more features or data to build a more robust algorithm.
##################################################################################################################################################

########### Let's Look at the Junk2 File ######################
set.seed(42)

train.index1 <- createDataPartition(df2$class, p = .7, list = FALSE)
train_df2<- df2[ train.index1,]
hold_out_df2  <- df2[-train.index1,]



logitMod <- glm(class ~ a + b, data=train_df2, family=binomial(link="logit"))
summary(logitMod)

hold_out_df2$predicted_logistic <- plogis(predict(logitMod, hold_out_df2))

roc(hold_out_df2[["class"]], hold_out_df2[["predicted_logistic"]], plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)


## try a random forest instead

source("https://raw.githubusercontent.com/crarnouts/Data_605_Final/master/RandomForestNulls_testing.R")

hold_out_df2 <- RF_with_Nulls(train_df2,hold_out_df2,"class",.5,4,10,.005,5,1)


roc(hold_out_df2[["class"]], hold_out_df2[["prediction_overall"]], plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)

## The Junk 2 File seems to perform very well with both logistic regression classification and decision tree classification

######################################### CONCLUSION FOR JUNK2.csv File ###########################################################################
# Based on the results that I have generated in the junk2 file I would say that based on these two features we can pretty confidently predict the 
# class of the class variable. It is hard to say for sure that we do not need more data to make more accurate predictions, but I would need to know more about 
# the application of the algortihm to know for sure how it will perform.
##################################################################################################################################################
