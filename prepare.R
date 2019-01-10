###############################################################################
# These preparation functions should each take a named list with the train
# and test datasets, and output a named list with the transformed train and
# test datasets. This will allow them to be chained together with pipes as
# needed.
###############################################################################

###############################################################################
# Normalization
###############################################################################
normalize <- function(l) {
  train <- l$train
  test <- l$test
  for (n in setdiff(names(test),c('country','year','value','varstr'))) { 
    test[[n]] <- test[[n]] - mean(train[[n]],na.rm=TRUE)
    test[[n]] <- test[[n]] / sd(train[[n]],na.rm=TRUE)
    train[[n]] <- scale(train[[n]])
  }
  list(train=train, test=test)
}

###############################################################################
# Feature selection: Get a reduced set of features using LASSO
###############################################################################
lasso_1se <- function(l) {
  train <- l$train
  test <- l$test
  x <- train %>% select(-year,-value) %>% as.matrix
  y <- train$value
  cv.out <- cv.glmnet(x,y,alpha=1,family='gaussian',type.measure = 'mse')
  tmp <- coef(cv.out,s=cv.out$lambda.1se) 
  n <- colnames(x)[tmp@i]
  train <- train %>% select(year,value,n)
  test <- test %>% select(year,value,n)
  list(train=train,test=test)
}

lasso_min <- function(l) {
  train <- l$train
  test <- l$test
  x <- train %>% select(-year,-value) %>% as.matrix
  y <- train$value
  cv.out <- cv.glmnet(x,y,alpha=1,family='gaussian',type.measure = 'mse')
  tmp <- coef(cv.out,s=cv.out$lambda.min)
  n <- colnames(x)[tmp@i]
  train <- train %>% select(year,value,n)
  test <- test %>% select(year,value,n)
  list(train=train,test=test)
}
