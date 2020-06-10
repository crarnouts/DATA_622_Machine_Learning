# 
# Please find kNN.R
# 
# This R script requires a dataset, labelcol and K (number of nearest neighbors to be considered)
# 
# The dataset MUST Be numeric, except the labelcol
# The labelcol must be the last column in the data.frame
# All the other columns must be before the labelcol
# 
# To DO:
#   
#   Please find icu.csv 
# The formula to fit is "STA ~ TYP + COMA + AGE + INF"
# 
# Read the icu.csv subset it with these 5 features in the formula and STA is the labelcol.
# 
# Split the icu 70/30 train/test and
# run the kNN.R for K=(3,5,7,15,25,50)
# 
# submit the result confusionMatrix, Accuracy for each K
# 
# Plot Accuracy vs K.
# 
# write a short summary of your findings.
# 
# Grade-->40
# Changing the code 10
# Running for different values of K 10
# Plot Accuracy 10
# Summary 10


euclideanDist <- function(a, b){
  d = 0
  for(i in c(1:(length(a)) ))
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

knn_predict2 <- function(test_data, train_data, k_value, labelcol){
  pred <- c()  #empty pred vector 
  #LOOP-1
  for(i in c(1:nrow(test_data))){   #looping over each record of test data
    eu_dist =c()          #eu_dist & eu_char empty  vector
    eu_char = c()
    good = 0              #good & bad variable initialization with 0 value
    bad = 0
    
    #LOOP-2-looping over train data 
    for(j in c(1:nrow(train_data))){
 
      #adding euclidean distance b/w test data point and train data to eu_dist vector
      eu_dist <- c(eu_dist, euclideanDist(test_data[i,-c(labelcol)], train_data[j,-c(labelcol)]))
 
      #adding class variable of training data in eu_char
      eu_char <- c(eu_char, as.character(train_data[j,][[labelcol]]))
    }
    
    eu <- data.frame(eu_char, eu_dist) #eu dataframe created with eu_char & eu_dist columns
 
    eu <- eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    eu <- eu[1:k_value,]               #eu dataframe with top K neighbors
 
    tbl.sm.df<-table(eu$eu_char)
    cl_label<-  names(tbl.sm.df)[[as.integer(which.max(tbl.sm.df))]]
    
    pred <- c(pred, cl_label)
    }
    return(pred) #return pred vector
  }
  

accuracy <- function(test_data,labelcol,predcol){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,labelcol] == test_data[i,predcol]){ 
      correct = correct+1
    }
  }
  accu = (correct/nrow(test_data)) * 100  
  return(accu)
}

#load data
knn.df<-iris
labelcol <- 5 # for iris it is the fifth col 
predictioncol<-labelcol+1
# create train/test partitions
set.seed(2)
n<-nrow(knn.df)
knn.df<- knn.df[sample(n),]

train.df <- knn.df[1:as.integer(0.7*n),]

K = 3 # number of neighbors to determine the class
table(train.df[,labelcol])
test.df <- knn.df[as.integer(0.7*n +1):n,]
table(test.df[,labelcol])

predictions <- knn_predict2(test.df, train.df, K,labelcol) #calling knn_predict()

test.df[,predictioncol] <- predictions #Adding predictions in test data as 7th column
print(accuracy(test.df,labelcol,predictioncol))
table(test.df[[predictioncol]],test.df[[labelcol]])

########################################################################################################################################
########################################### SAME PROCESS FOR THE ICU DATASET ###########################################################
########################################################################################################################################

icu <- read.csv("icu.csv")

icu$COMA <- ifelse(icu$LOC==2,1,0)

icu$LOC <- NULL
set.seed(42)
knn.df<-icu
labelcol <- 21 # for iris it is the fifth col 
predictioncol<-labelcol+1

n<-nrow(knn.df)
knn.df<- knn.df[sample(n),]

train.df <- knn.df[1:as.integer(0.7*n),]

Klist <- c(1,3,5,7,15,25,50)

accuracylist <- c()


for (K in Klist){
#K = 3 # number of neighbors to determine the class
table(train.df[,labelcol])
test.df <- knn.df[as.integer(0.7*n +1):n,]
table(test.df[,labelcol])

predictions <- knn_predict2(test.df, train.df, K,labelcol) #calling knn_predict()

test.df[,predictioncol] <- predictions #Adding predictions in test data as 7th column
print(accuracy(test.df,labelcol,predictioncol))
print(table(test.df[[predictioncol]],test.df[[labelcol]]))

accuracylist <- append(accuracylist,accuracy(test.df,labelcol,predictioncol))
}


accuracydf <- as.data.frame(Klist)

accuracydf$Accuracy <- accuracylist


plot(accuracydf$Klist,accuracydf$Accuracy)

#################################################### CONCLUSION ##########################################################################
# The only K that yields any different results is when k=1, when k>1 then the algorithm just guesses that the target variable = 0 in every situation
# which is not very helpful


