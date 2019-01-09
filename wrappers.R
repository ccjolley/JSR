library(FNN)
library(e1071)
library(xgboost)

###############################################################################
# All of these wrappers share a common interface: they take the train and test
# datasets, followed by any algorithm-specific hyperparameters, and return
# predicted values for the test dataset.
###############################################################################

###############################################################################
# Basic linear model
###############################################################################
lm_wrap <- function(train,test,...) {
  if (!('value' %in% names(test))) { test$value <- NA }
  is_const <- (apply(train,2,var,na.rm=TRUE) == 0)
  is_const['value'] <- FALSE
  train <- train[!is_const]
  test <- test[!is_const]
  my_lm <- lm(value ~ .,data=train)
  predict(my_lm,test)
}

###############################################################################
# k-Nearest neighbors
###############################################################################
knn_wrap <- function(train,test,...) {
  # filter values that are nearly constant
  is_const <- (apply(train,2,var,na.rm=TRUE) == 0)
  is_const['value'] <- FALSE
  train <- train[!is_const]
  test <- test[!is_const]
  # prepare for KNN
  x <- train %>% select(-value)
  xtest <- test %>% select(-value)
  # scale test according to training data
  # TODO: replace this with the scaling function in prepare.R
  for (n in names(xtest)) { 
    xtest[[n]] <- xtest[[n]] - mean(x[[n]])
    xtest[[n]] <- xtest[[n]] / sd(x[[n]])
  }
  x <- scale(x)
  #print(paste0('DEBUG: using k = ',list(...)$k))
  my_knn <- knn.reg(train=x,test=xtest,y=train$value,...)
  my_knn$pred
}

###############################################################################
# Support vector machines
###############################################################################
svm_wrap <- function(train,test,...) {
  is_const <- (apply(train,2,var,na.rm=TRUE) == 0)
  train <- train[!is_const]
  test <- test[!is_const]
  my_svm <- svm(value ~ .,data=train,... = )
  predict(my_svm,test)
}

###############################################################################
# Gradient-boosted machines
###############################################################################
xgb_wrap <- function(train,test,nrounds=100,...) {
  if (!('value' %in% names(test))) { test$value <- NA}
  dtrain <- xgb.DMatrix(data=select(train,-value) %>% as.matrix,
                        label=train$value)
  dtest <- xgb.DMatrix(data=select(test,-value) %>% as.matrix,
                       label=test$value)
  if (is.null(nrounds)) { # using xval to determine nrounds
    watchlist <- list(train=dtrain, test=dtest)
    my_xgb <- xgb.train(data=dtrain,nrounds=200,watchlist=watchlist,  
                        early_stopping_rounds=5,verbose=0,nthread=4,...)
    paste0(my_xgb$best_iteration,' iterations') %>% print
  } else {
    my_xgb <- xgb.train(data=dtrain,nrounds=nrounds,nthread=4,...)
  }
  predict(my_xgb,dtest)
}