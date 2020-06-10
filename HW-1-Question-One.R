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


cor(hold_out_df$class,hold_out_df$prediction_overall)

roc(hold_out_df[["class"]], hold_out_df[["prediction_overall"]], plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)

## It appears the data that is in the junk.txt has some non linearities and interactions between the two variables in the feature space this is
## why the simple 10 tree random forest does better than the logistic model

### Let's try out a Support Vector Machine for this Classification Task

