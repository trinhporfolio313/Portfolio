getwd()
accident_data = read.csv("~/Accidents.csv", stringsAsFactors = TRUE)
View (accident_data)

pkgs <- c ("rpart", "rpart.plot", "caret", "randomForest", "xgboost", "Boruta")
to_install <- pkgs [!pkgs %in% rownames (installed.packages())]
if (length (to_install) > 0) install.packages (to_install)

library (rpart)
library (rpart.plot)
library (caret)
library (randomForest)
library (xgboost)
library (Boruta)



#Attribute Selection

colnames (accident_data )

worker_vars = c ("SUBUNIT", "OCCUPATION", "MINING_EQUIP")
OpEx_vars = c ("TOT_EXPER", "MINE_EXPER", "JOB_EXPER")
accident_vars = c("ACCIDENT_TYPE","ACTIVITY", "INJURY_SOURCE", "DEGREE_INJURY")

accident_data2 = accident_data[,c(worker_vars, OpEx_vars, accident_vars)]

View(accident_data2)


set.seed (123)

selectedVars_list = list()

for (i in 1:10) {
  
  set.seed (i)
  
  boruta_accident = Boruta(DEGREE_INJURY ~ ., 
                           data = accident_data2, 
                           doTrace = 2, 
                           maxRuns = 1000)
  
  selectedVar = getSelectedAttributes(boruta_accident)
  selectedVars_list[[i]] = selectedVar
}

selectedVars_list

var_frequency = table (unlist (selectedVars_list))
var_frequency


#Omit NA
table (accident_data2$DEGREE_INJURY)

accident_data3 = na.omit (accident_data2)
accident_data3 = droplevels(accident_data3)

str(accident_data2)
str(accident_data3)


#Independent test

set.seed (1234)

independent.test.tree = NULL


for(i in 1:5){
  test = createDataPartition(accident_data3$DEGREE_INJURY, p = 0.33, list=FALSE)
  test.data = accident_data3[test,]
  train.data = accident_data3[-test,]
  
  tree.model = train(DEGREE_INJURY~.,
                     data=train.data,
                     method= "rpart",
                     tuneLength=20)
  tree.prediction = predict (tree.model, test.data)
  CM.tree = table(tree.prediction,test.data$DEGREE_INJURY)
  independent.test.tree[i] = sum(diag (CM.tree))/sum(CM.tree)
  
}
independent.test.tree
mean(independent.test.tree)
sqrt(var(independent.test.tree))


#Cross Validation
set.seed(3)
cv.estimatesSD = NULL
for(i in 2:13) {
  cv.model = train(DEGREE_INJURY~.,
                   data=accident_data3,
                   method= "rpart",
                   tuneLength=20,
                   trControl = trainControl(method="cv", number=i))
  cv.estimatesSD[i] = max(cv.model$results$AccuracySD)
}
plot(cv.estimatesSD,
     ylim = c(0,0.5),
     main="CV Estimator Error for Decision Tree",
     xlab = "Number of folds",
     ylab = "Standard Deviation")
cv.model$results$Accuracy
mean(cv.model$results$Accuracy)
sqrt(var(cv.model$results$Accuracy))

#0.632-bootstrap
boot632.estimate = NULL
for(i in 1:5) {
  boot632.model = train(DEGREE_INJURY~.,
                        data=accident_data3,
                        method="rpart",
                        tuneLength=20,
                        trControl = trainControl(method="boot632"))
  boot632.estimate [i] = max(boot632.model$results$Accuracy)
}
boot632.estimate
mean(boot632.estimate)
sqrt(var(boot632.estimate))

#Reduce categories levels
sapply(accident_data3, nlevels)


collapse_levels = function (x, min_count = 20){
  freq = table(x)
  rare = names(freq[freq<min_count])
  x = as.character(x)
  x[x %in% rare] = "OTHER"
  factor(x)
}

accident_data3$OCCUPATION = collapse_levels (accident_data3$OCCUPATION,20)
accident_data3$MINING_EQUIP = collapse_levels (accident_data3$MINING_EQUIP,20)
accident_data3$ACTIVITY = collapse_levels (accident_data3$ACTIVITY,20)
accident_data3$INJURY_SOURCE = collapse_levels (accident_data3$INJURY_SOURCE,20)

sapply(accident_data3, nlevels)


#Decision Tree

test = createDataPartition(accident_data3$DEGREE_INJURY, p = 0.33, list=FALSE)
test.data = accident_data3[test,]
train.data = accident_data3[-test,]
  
tree.model = train(DEGREE_INJURY~.,
                     data=train.data,
                     method= "rpart",
                     tuneLength=20)

tree.prediction = predict (tree.model, test.data)
CM.tree = confusionMatrix(tree.prediction,test.data$DEGREE_INJURY)
tree.accuracy.estimate = CM.tree$overall ["Accuracy"]
balanced.acc.tree = mean(CM.tree$byClass [,"Balanced Accuracy"], na.rm =TRUE)
kappa.tree = CM.tree$overall["Kappa"]
rpart.plot(tree.model$finalModel)
CM.tree

#Random Forest 

  
rf.model = randomForest(DEGREE_INJURY~.,data = train.data)
rf.prediction = predict (rf.model, test.data)
CM.rf = confusionMatrix(rf.prediction,test.data$DEGREE_INJURY)
rf.accuracy.estimate = CM.rf$overall ["Accuracy"]
balanced.acc.rf = mean(CM.rf$byClass [,"Balanced Accuracy"], na.rm =TRUE)
kappa.rf = CM.rf$overall["Kappa"]
CM.rf

#XgBoost

train.data.xgb = train.data
test.data.xgb = test.data

train.data.xgb[] = lapply(train.data.xgb, function(x){
  if(is.factor(x)) as.integer(x) else x
})

test.data.xgb[] = lapply(test.data.xgb, function(x){
  if(is.factor(x)) as.integer(x) else x
})

xtrain = as.matrix(train.data.xgb [, -1])
xtest = as.matrix(test.data.xgb [, -1])

ytrain = as.integer(train.data$DEGREE_INJURY) - 1
ytest = as.integer(test.data$DEGREE_INJURY) - 1

trainData_xb = xgb.DMatrix(data = xtrain, label=ytrain)
testData_xb = xgb.DMatrix(data = xtest,label=ytest)

num_class = length(unique(accident_xgb$DEGREE_INJURY))

params = list(booster="gbtree", eta=0.3,
              max_depth=6, gamma=0, subsample=1,
              colsample_bytree=1,
              colsample_bylevel=1,
              colsample_bynode=1,
              lambda = 1, alpha = 0,
              objective="multi:softprob",
              eval_metric="merror", 
              num_class = num_class)


xb.model = xgb.train(data=trainData_xb,
                     nrounds = 100,
                     params=params)

xb.prediction = predict (xb.model,testData_xb)

xb.prediction = matrix (xb.prediction, 
                        ncol = num_class,
                        byrow = TRUE)

xb.pred = max.col (xb.prediction) -1

xb.pred = factor (xb.pred,
			levels = 0:(length(unique(ytrain))-1),
			labels = levels (accident_data3$DEGREE_INJURY))
ytest = factor (ytest, 
			levels = 0:(length(unique(ytrain))-1),
			labels = levels (accident_data3$DEGREE_INJURY))
			
CM.xb = confusionMatrix(xb.pred, ytest)

xb.accuracy.estimate = CM.xb$overall ["Accuracy"]

balanced.acc.xb = mean(CM.xb$byClass [,"Balanced Accuracy"], na.rm =TRUE)
kappa.xb = CM.xb$overall["Kappa"]
CM.xb

#Evaluation
tree.accuracy.estimate
rf.accuracy.estimate
xb.accuracy.estimate


balanced.acc.tree
balanced.acc.rf
balanced.acc.xb

kappa.tree
kappa.rf
kappa.xb


#Tuning for xgboost

accident_xgb = accident_data3

accident_xgb[] = lapply(accident_xgb, function(x){
  if(is.factor(x)) as.integer(x)-1 else x
})

set.seed(1234)


folds = createFolds(accident_xgb$DEGREE_INJURY, k = 5)

eta_grid = c(0.05, 0.3,0.6)
lambda_grid = c(0.5,1,1.5)

fold_accuracy_estimate = NULL

for (f in 1:5){
  
  trainData = accident_xgb[-folds[[f]],]
  testData = accident_xgb[folds[[f]],]
  
  xtrain = as.matrix(trainData [, -1])
  xtest = as.matrix(testData [, -1])
  
  ytrain = trainData$DEGREE_INJURY
  ytest = testData$DEGREE_INJURY
  
  dtrain = xgb.DMatrix(data = xtrain, label=ytrain)
  dtest = xgb.DMatrix(data = xtest,label=ytest)
  
  best_acc = -Inf
  best_params = NULL
  
  for (eta in eta_grid){
    for (lambda in lambda_grid){
      params = list(booster="gbtree", eta=eta,
                    max_depth=6, gamma=0, subsample=1,
                    colsample_bytree=1,
                    colsample_bylevel=1,
                    colsample_bynode=1,
                    lambda = lambda, 
                    alpha = 0,
                    objective="multi:softprob",
                    eval_metric="merror", 
                    num_class = length(unique(ytrain)))
      
      xb.model = xgb.train(data=dtrain,
                           nrounds = 1000,
                           params=params)
      
      xb.prediction = predict (xb.model,dtest)
      
      xb.prediction = matrix (xb.prediction, 
                              ncol = length(unique(ytrain)),
                              byrow = TRUE)
      
      colnames(xb.prediction) = unique(ytrain)
      
      xb.pred = max.col (xb.prediction) -1
      
      acc = mean(xb.pred==ytest)
      
      if(acc > best_acc) {
        best_acc = acc
        best_params = list (eta = eta, lambda = lambda)
      }
      
    }
  }
  
  fold_accuracy_estimate[f] = best_acc
}

best_params$eta
best_params$lambda
mean(fold_accuracy_estimate)

#Repeat XgBoost 

train.data.xgb = train.data
test.data.xgb = test.data

train.data.xgb[] = lapply(train.data.xgb, function(x){
  if(is.factor(x)) as.integer(x) else x
})

test.data.xgb[] = lapply(test.data.xgb, function(x){
  if(is.factor(x)) as.integer(x) else x
})

xtrain = as.matrix(train.data.xgb [, -1])
xtest = as.matrix(test.data.xgb [, -1])

ytrain = as.integer(train.data$DEGREE_INJURY) - 1
ytest = as.integer(test.data$DEGREE_INJURY) - 1

trainData_xb = xgb.DMatrix(data = xtrain, label=ytrain)
testData_xb = xgb.DMatrix(data = xtest,label=ytest)

num_class = length(unique(accident_xgb$DEGREE_INJURY))

params = list(booster="gbtree", eta=0.6,
              max_depth=6, gamma=0, subsample=1,
              colsample_bytree=1,
              colsample_bylevel=1,
              colsample_bynode=1,
              lambda = 1, alpha = 0,
              objective="multi:softprob",
              eval_metric="merror", 
              num_class = num_class)


xb.model = xgb.train(data=trainData_xb,
                     nrounds = 100,
                     params=params)

xb.prediction = predict (xb.model,testData_xb)

xb.prediction = matrix (xb.prediction, 
                        ncol = num_class,
                        byrow = TRUE)

xb.pred = max.col (xb.prediction) -1

xb.pred = factor (xb.pred,
                  levels = 0:(length(unique(ytrain))-1),
                  labels = levels (accident_data3$DEGREE_INJURY))
ytest = factor (ytest, 
                levels = 0:(length(unique(ytrain))-1),
                labels = levels (accident_data3$DEGREE_INJURY))

CM.xb = confusionMatrix(xb.pred, ytest)

xb.accuracy.estimate = CM.xb$overall ["Accuracy"]

balanced.acc.xb = mean(CM.xb$byClass [,"Balanced Accuracy"], na.rm =TRUE)
kappa.xb = CM.xb$overall["Kappa"]

#Re-evaluate
tree.accuracy.estimate
rf.accuracy.estimate
xb.accuracy.estimate


balanced.acc.tree
balanced.acc.rf
balanced.acc.xb

kappa.tree
kappa.rf
kappa.xb

