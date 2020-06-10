

df <- read.table("hw2.txt",header = T,sep=',')

df$target <- ifelse(df$label =="BLACK",1,0)
df$Y_num <- as.numeric(df$Y)
df$X_fact <- as.factor(df$X)

library(caret)
library(gbm)
library(e1071)
library(dplyr)
library(MASS)
library(ISLR)
set.seed(998)
inTraining <- createDataPartition(df$target, p = .7, list = FALSE)
training <- df[ inTraining,]
testing  <- df[-inTraining,]




################################################################### K Nearest Neighbors ########################################################################


set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(label ~ X+Y_num, data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit2 <- train(target ~ X+Y_num, data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
testing$predictionknn_num <- predict(knnFit2,newdata = testing )
testing$predictionknn <- predict(knnFit,newdata = testing )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(testing$label, testing$predictionknn)

################################################################## Naive Bayes ################################################################################

# e1071 naiveBayes
set.seed(3456)

nb_training <- training %>% dplyr::select(X_fact,Y)
nb_target <- training %>% dplyr::select(label)

fit1 <- naiveBayes(nb_training , nb_target$label, type="raw")
testing$nb_Prediction <- predict(fit1, testing, type="class")
nb_Prediction_num <- predict(fit1, testing, type="raw")
testing$nb_Prediction_num <- nb_Prediction_num[,1]

confusionMatrix(testing$nb_Prediction, testing$label)

################################################################ Linear Discriminant Analysis ################################################################

ldafit <- lda(label~X+Y_num, data = training)

ldaprediction <- predict(ldafit, newdata = testing)

testing$ldapredict <- ldaprediction$class
testing$ldapredictnum <- ldaprediction$posterior[,1]
confusionMatrix(testing$ldapredict, testing$label)

################################################################ Logistic Regression #########################################################################

glm.fit <- glm(target ~ X+Y, data = training, family = binomial)

testing$logisticpredictnum <- predict(glm.fit,newdata = testing,type = "response")

testing$logisticpredict <- as.factor(ifelse(testing$logisticpredictnum >=.5,"BLACK","BLUE"))

confusionMatrix(testing$logisticpredict, testing$label)


################################################################ Support vector Machine ######################################################################
svmfit = svm(target ~ X+Y, data = training, kernel = "linear")
testing$svmpredictnum <- predict(svmfit,newdata = testing)
testing$svmpredict <-  as.factor(ifelse(testing$svmpredictnum >=.5,"BLACK","BLUE"))

confusionMatrix(testing$svmpredict, testing$label)

################################################################ RBF Kernel ##################################################################################
svmfitradial = svm(target ~ X+Y, data = training, kernel = "radial")
testing$svmradialpredictnum <- predict(svmfit,newdata = testing)
testing$svmradialpredict <-  as.factor(ifelse(testing$svmpredictnum >=.5,"BLACK","BLUE"))

confusionMatrix(testing$svmradialpredict, testing$label)


################################################################ DECISION TREE ###############################################################################
library(tree)
tree= tree(label ~ X+Y, data = training)

testing$treepredict= predict(tree, testing, type="class")

tree= tree(target ~ X+Y, data = training)

testing$treepredictnum = predict(tree, testing)

################################################### AVERAGE ACROSS ALL MODELS ################################

testing$averageprediction_num <- (testing$predictionknn_num+testing$nb_Prediction_num+testing$ldapredictnum+testing$logisticpredictnum+testing$treepredictnum)/5

testing$averageprediction <- as.factor(ifelse(testing$averageprediction_num >= .5,"BLACK","BLUE"))

confusionMatrix(testing$averageprediction, testing$label)



################################################# AUC ACCURACY TPR FPR #######################################################################################

library(pROC)
roc_obj <- roc(testing$label, testing$predictionknn_num)
o <- confusionMatrix(testing$predictionknn, testing$label)
auc(roc_obj)
## knn - .5417,.6,.8333,.75

roc_obj <- roc(testing$label, testing$nb_Prediction_num)
confusionMatrix(testing$nb_Prediction, testing$label)
auc(roc_obj)
# naive bayes - .9167, .667, .8, 0

roc_obj <- roc(testing$label, testing$logisticpredictnum)
confusionMatrix(testing$logisticpredict, testing$label)
auc(roc_obj)
# logistic - .5,.7,1,.75

roc_obj <- roc(testing$label, testing$ldapredictnum)
confusionMatrix(testing$ldapredict, testing$label)
auc(roc_obj)
#lda - .75, .4, .5, .75


roc_obj <- roc(testing$label, testing$svmpredictnum)
confusionMatrix(testing$svmpredict, testing$label)
auc(roc_obj)
## SVM - .4583, .7,1,.75


roc_obj <- roc(testing$label, testing$svmradialpredictnum)
confusionMatrix(testing$svmradialpredict, testing$label)
auc(roc_obj)
## SVM radial - .4583, .7,1,.75

roc_obj <- roc(testing$label, testing$treepredictnum)
confusionMatrix(testing$treepredict, testing$label)
auc(roc_obj)
## tree -.7292, .6, .5, .25

