#traindatafile<-'/home/data/car/car_eval_train.csv'
traindatafile<-'car_eval_train.csv'
trdata<-read.csv(traindatafile,head=T,sep=',')

## Corey's Comments
### Reading in the data using relative paths 

car_eval<-trdata
names(car_eval)<-c("buying","maint","doors","persons","lug_boot","safety","class")
names(trdata)<-c("buying","maint","doors","persons","lug_boot","safety","class")



#tstdatafile<-'/home/data/car/car_eval_test.csv'
tstdatafile<-'car_eval_test.csv'
tstdata<-read.csv(tstdatafile,head=T,sep=',')
names(tstdata)<-names(car_eval)

##################################### Corey's Comments ####################################
##################################### Corey's Comments ####################################
### Reading in the data using relative paths 


##################################### Corey's Comments ####################################
##################################### Corey's Comments ####################################
### In the Data Science Process there is always room to get a sense of the relationships 
### that are present in the data. Maybe some exploratory data analysis would be useful
### here to get an idea of what variables are highly correlated with the target variable

## look at the structure of the data 
str(trdata)



x<-tstdata[,1:6]
y<-tstdata[[7]]

## Corey's Comments
### Seperating out the Feature Space from the target variable in the testing data

if(!require(VGAM)) library(VGAM)
if(!require(caret)) library(caret)

vglm_model<-vglm(class~buying+maint+doors+persons+lug_boot+safety,family = "multinomial",data=car_eval)

vglm_class_probabilities<-predict(vglm_model,tstdata[,1:6],type="response")

vglm_predicted_class<-apply(vglm_class_probabilities,1,which.max)

vglm_pred<-c()
vglm_pred[which(vglm_predicted_class=="1")]<-levels(y)[1]
vglm_pred[which(vglm_predicted_class=="2")]<-levels(y)[2]
vglm_pred[which(vglm_predicted_class=="3")]<-levels(y)[3]
vglm_pred[which(vglm_predicted_class=="4")]<-levels(y)[4]

vglm_mtab<-table(vglm_pred,tstdata[[7]])
(vglm_cmx<-confusionMatrix(table(vglm_pred,tstdata[[7]])))

(vglm_accuracy<-sum(diag(vglm_mtab))/sum(vglm_mtab))

if(!require(MASS)) library(MASS)

lda_model<-lda(class~buying+maint+doors+persons+lug_boot+safety,data=car_eval)

lda_class_probabilities<-predict(lda_model,tstdata[,1:6],type="response")

(lda_cmx<-table(lda_class_probabilities$class,tstdata[[7]]))
lda_mtab<-table(lda_class_probabilities$class,tstdata[[7]])
(lda_accuracy<-sum(diag(lda_cmx))/sum(lda_cmx))
lda_cmx<-confusionMatrix(table(lda_class_probabilities$class,tstdata[[7]]))

if(!require(rpart)) library(rpart)
if(!require(rpart.plot)) library(rpart.plot)
if(!require(randomForest)) library(randomForest)

rpart_model<-rpart(class~buying+maint+doors+persons+lug_boot+safety,data=car_eval)

rpart_class_probabilities<-predict(rpart_model,tstdata[,1:6],type="class")

(rpart_mtab<-table(rpart_class_probabilities,tstdata[[7]]))
rpart_cmx<-confusionMatrix(rpart_mtab)
(rpart_accuracy<-sum(diag(rpart_mtab))/sum(rpart_mtab))

# Now let us do ensemble methods
# let us start with bagging bootstrap aggregation

#bagging
if(!require(ipred)) library(ipred)

bag_model<-bagging(class~buying+maint+doors+persons+lug_boot+safety,data=car_eval)

bag_class_probabilities<-predict(bag_model,tstdata[,1:6])#,type="response")

(bag_mtab<-table(bag_class_probabilities,tstdata[[7]]))
(bag_cmx<-confusionMatrix(bag_mtab))

(bag_accuracy<-sum(diag(bag_mtab))/sum(bag_mtab))


nlev<-4 # number of classes


##################################### Corey's Comments ####################################
##################################### Corey's Comments ####################################
### I do not see why the interaction depth parameter below should be set to the number
### of classes in the dataset it seems that the interaction depth should be a paramete
### that could be determined through cross validation
### you could add in the tuneGrid parameter in order to determine what the optimal 
### interaction depth would be
##### Example of TUNE GRID #####
# gbmGrid <-  expand.grid(interaction.depth = c(5, 10, 15),
#                         n.trees = (1:30)*50,
#                         shrinkage = 0.001)
##### You would then add then add the tuneGrid parameter: tuneGrid = gbmGrid
### you could also use cross validation to determine the optimal number of trees for your algorithm

if(!require(gbm)) library(gbm)
gbm_model<-gbm(class~buying+maint+doors+persons+lug_boot+safety, 	
               data=car_eval,n.trees=5000,interaction.depth=nlev,
               shrinkage=0.001,bag.fraction=0.8,distribution="multinomial",verbose=FALSE,n.cores=4)

gbm_class_probabilities<-predict(gbm_model,tstdata[,1:6],n.trees=5000,type="response")

gbm_pred<-apply(gbm_class_probabilities,1,which.max) # selects the class with the highest probability

gbm_predicted_class<-unlist(lapply(gbm_pred,FUN=function(x)levels(tstdata[[7]])[[x]]))

(gbm_mtab<-table(gbm_predicted_class,tstdata[[7]]))
(gbm_accuracy<-sum(diag(gbm_mtab))/sum(gbm_mtab))


### Corey's Comments 
### Again for this model as well cross validation could be used to determine the corrrect number of parameters
gbm_model2<-gbm(class~buying+maint+doors+persons+lug_boot+safety,data=car_eval,n.trees=5000,interaction.depth=nlev,
                shrinkage=0.001,bag.fraction=0.8,distribution="multinomial",
                verbose=FALSE,n.cores=4)
gbm_class_probabilities2<-predict(gbm_model2,tstdata[,1:6],n.trees=5000,type="response")
gbm_pred2<-apply(gbm_class_probabilities2,1,which.max)
gbm_pred2[which(gbm_pred2=="1")]<-levels(tstdata[[7]])[1]
gbm_pred2[which(gbm_pred2=="2")]<-levels(tstdata[[7]])[2]
gbm_pred2[which(gbm_pred2=="3")]<-levels(tstdata[[7]])[3]
gbm_pred2[which(gbm_pred2=="4")]<-levels(tstdata[[7]])[4]
gbm_pred2<-as.factor(gbm_pred2)
l<-union(gbm_pred2,tstdata[[7]])
(gbm_mtab2<-table(factor(gbm_pred2,l),factor(tstdata[[7]],l)))
(gbm_accuracy2<-sum(diag(gbm_mtab2))/sum(gbm_mtab2))
(gbm_cmx2<-confusionMatrix(gbm_mtab2))


nlev<-5 # number of classes+1
gbm_model3<-gbm(class~buying+maint+doors+persons+lug_boot+safety, 	
                data=car_eval,n.trees=5000,interaction.depth=nlev,
                shrinkage=0.001,bag.fraction=0.8,distribution="multinomial",verbose=FALSE,n.cores=4)
gbm_class_probabilities3<-predict(gbm_model3,tstdata[,1:6],n.trees=5000,type="response")
gbm_pred3<-apply(gbm_class_probabilities3,1,which.max)
##############
gbm_pred3[which(gbm_pred3=="1")]<-levels(tstdata[[7]])[1]
gbm_pred3[which(gbm_pred3=="2")]<-levels(tstdata[[7]])[2]
gbm_pred3[which(gbm_pred3=="3")]<-levels(tstdata[[7]])[3]
gbm_pred3[which(gbm_pred3=="4")]<-levels(tstdata[[7]])[4]
gbm_pred3<-as.factor(gbm_pred3)
l<-union(gbm_pred3,tstdata[[7]])
(gbm_mtab3<-table(factor(gbm_pred3,l),factor(tstdata[[7]],l)))
(gbm_accuracy3<-sum(diag(gbm_mtab3))/sum(gbm_mtab3))
(gbm_cmx3<-confusionMatrix(gbm_mtab3))

##################################### Corey's Comments ####################################
##################################### Corey's Comments ####################################
### The model above appears to be the best performing gradient boosting model, I believe this
### is because this model used higher levels of interaction
### In my example below you can see that I increase the level of interactions to 10
### this improved performance because it was able to capture increasingly complex
### relationships between the variables


nlev<-10 
gbm_modelcorey<-gbm(class~buying+maint+doors+persons+lug_boot+safety, 	
                data=car_eval,n.trees=5000,interaction.depth=nlev,
                shrinkage=0.001,bag.fraction=0.8,distribution="multinomial",verbose=FALSE,n.cores=4)
gbm_class_probabilitiescorey<-predict(gbm_modelcorey,tstdata[,1:6],n.trees=5000,type="response")
gbm_predcorey<-apply(gbm_class_probabilitiescorey,1,which.max)
##############
gbm_predcorey[which(gbm_predcorey=="1")]<-levels(tstdata[[7]])[1]
gbm_predcorey[which(gbm_predcorey=="2")]<-levels(tstdata[[7]])[2]
gbm_predcorey[which(gbm_predcorey=="3")]<-levels(tstdata[[7]])[3]
gbm_predcorey[which(gbm_predcorey=="4")]<-levels(tstdata[[7]])[4]
gbm_predcorey<-as.factor(gbm_predcorey)
l<-union(gbm_predcorey,tstdata[[7]])
(gbm_mtabcorey<-table(factor(gbm_predcorey,l),factor(tstdata[[7]],l)))
(gbm_accuracycorey<-sum(diag(gbm_mtabcorey))/sum(gbm_mtabcorey))
(gbm_cmxcorey<-confusionMatrix(gbm_mtabcorey))




###################
#gbm_predicted_class3<-unlist(lapply(gbm_pred3,FUN=function(x)levels(tstdata[[7]])[[x]]))

#(gbm_mtab3<-table(gbm_predicted_class3,tstdata[[7]]))
#(gbm_accuracy3<-sum(diag(gbm_mtab3))/sum(gbm_mtab3))
#(gbm_cmx3<-confusionMatrix(gbm_mtab3))


##################################### Corey's Comments ####################################
##################################### Corey's Comments ####################################
### Random Forest models make use of bagging and bootstrapping and are algorithms
### that are able to be parallelized
### The random forest model below does not perform quite as well as the gbm models



if(!require(randomForest))require(randomForest)
rf_model<-randomForest(class~buying+maint+doors+persons+lug_boot+safety,
                       data=car_eval)
rf_pred<-predict(rf_model,tstdata[,1:6])
rf_mtab<-table(rf_pred,tstdata[[7]])
rf_cmx<-confusionMatrix(rf_mtab)
rf_cmx$overall
rf_cmx$byClass


#XGBoost only works with numeric vectors. 
#need to convert all other forms of data into numeric vectors.
# we use Matrix sparse.model.matrix for that

if(!require(xgboost)) library(xgboost)
if(!require(Matrix)) library(Matrix)
trdatamx<-sparse.model.matrix(class~.-1,data=trdata)
tstdatamx<-sparse.model.matrix(class~.-1,data=tstdata)

xgb_model<-xgboost(data=trdatamx,label=trdata$class,max_depth = 2, 
                   eta = 1, nrounds = 2,nthread = 2, objective = "multi:softmax",num_class=5)

xgb_pred <- predict(xgb_model,tstdatamx)
xgb_tab<-table( xgb_pred)
xgb_mtab<-table(xgb_pred,tstdata[[7]])
#xgb_cmx<-confusionMatrix(xgb_mtab)
#xgb_cmx$overall
#xgb_cmx$byClass


xgb_model4<-xgboost(data=trdatamx,label=trdata$class,max_depth = 4, 
                    eta = 1, nrounds = 3,nthread = 2, objective = "multi:softmax",num_class=5)
xgb_pred4 <- predict(xgb_model4,tstdatamx)
xgb_tab4<-table( xgb_pred4)
temp_xgb_tab4<-xgb_tab4

xgb_pred4[which(xgb_pred4=="1")]<-levels(y)[1]
xgb_pred4[which(xgb_pred4=="2")]<-levels(y)[2]
xgb_pred4[which(xgb_pred4=="3")]<-levels(y)[3]
xgb_pred4[which(xgb_pred4=="4")]<-levels(y)[4]
xgb_mtab4<-table(xgb_pred4,tstdata[[7]])
xgb_cmx4<-confusionMatrix(xgb_mtab4)
xgb_cmx4$overall
xgb_cmx4$byClass

xgb_model5<-xgboost(data=trdatamx,label=trdata$class,max_depth = 5, 
                    eta = 1, nrounds = 4,nthread = 2, objective = "multi:softmax",num_class=5)
xgb_pred5 <- predict(xgb_model5,tstdatamx)
table( xgb_pred5)

xgb_tab5<-table( xgb_pred5)
temp_xgb_tab5<-xgb_tab5

xgb_pred5[which(xgb_pred5=="1")]<-levels(y)[1]
xgb_pred5[which(xgb_pred5=="2")]<-levels(y)[2]
xgb_pred5[which(xgb_pred5=="3")]<-levels(y)[3]
xgb_pred5[which(xgb_pred5=="4")]<-levels(y)[4]
xgb_mtab5<-table(xgb_pred5,tstdata[[7]])
xgb_cmx5<-confusionMatrix(xgb_mtab5)
xgb_cmx5$overall
xgb_cmx5$byClass


##################################### Corey's Comments ####################################
##################################### Corey's Comments ####################################
### XG boost models do not perform as well as the tree based models
### XG boost models are designed to control over-fitting in order to improve generalization
### It appears however they are outperformed by the gbm models perhaps this is because 
### the signal in the test set is very similar to the signal modelled in the training set
### therefore causing XG boost to have higher bias when compared to the gbm models

lapply(ls()[grep("mtab",ls())],FUN=function(x)eval(x))

txt<-capture.output({lapply(ls()[grep("cmx",ls())],FUN=function(x)eval(parse(text=x)))})

writeLines(txt,"confusionMxOutput_2.txt")

mtabtxt<-capture.output({lapply(ls()[grep("mtab",ls())],FUN=function(x)eval(parse(text=x)))})
writeLines(mtabtxt,"mtabOutput_2.txt") 



############################## Corey's Overview ######################################

## Approach to Data science ##
# Take the time to learn the subject matter there are only six variables present 
# in your feature space, there are most likely many many more predictors that could
# potentially be used but you need to learn more about the domain in order to figure out
# what these predictor variables might be.

# Also once you have some of the variables that you think may be influential take the time
# to understand the variables through some exploratory data analysis.

# Also you can include the parameter optimization right in your models by using cross validation
# and a grid search.

# collaborately with the business partners is also a beneficial exercise because it allows
# them to know what you are doing and it allows you to gain more insight on how to build
# a better predictive model

## Choice of Classifiers that Ruffio took

# I am glad that used many different classifiers to determine which one would perform
# the best on the dataset. One classifier that you didn't try that may have performed well
# is the naive bayes classifier.


### Compare Performance of Different Algorithms ####

# The vglm algorithm performed relatively well it performed especially well on the vgood class
# there must be something about the vgood class that is easily picked up the generalized linear model
# they make use of mulitplicative relationships in the feature space when relating to the target variable

# The accuracy of Linear Discriminant analysis was lower than that of the generalized linear model
# this makes sense because all of the features of factor variables and LDA typically does 
# better on numerical data in the feature space


# Rpart does actually really well especially considering that decision trees are considered
# weak learners I think one of the reasons that it does really well is because decision trees 
# and random forests do really well on class data

# Bagging does really well with an overall accuracy of .988 and I think the reason that 
# it does so well is because tree based methods do really well with categorical data
# and bagged models outperform individual decision trees

# The gradient boosting machine does well but like I said above I think you need to 
# increase the interaction depth of the gbm model I showed above that increasing the 
# interaction depth to 10 can have a significant impact on the model, once you increase
# the interaction depth the gbm does almost as well as the bagged model

# The random forest model does not do quite as well as the bagged model which is suprising 
# random forests take a subset of the feature space as well whereas bagged models do not
# the bagged model probably outperforms the random forest because there are very few variables
# in this dataset and they are all important

# The xgboost does not do as well as the other tree based methods most likely because it 
# does regularization and tries not to overfit 

######################## Conclusion #############################################
# Overall Ruffio you did a good job with this data science intiative I think the models
# that you fit were sufficient for what the business was looking for, you could of spent more
# time in the exploratory data analysis phase and also spent more time looking for some
# additional features.
# Now that the model has been built though that is only part of the battle as implementing 
# a predictive model is equally if not more challenging
