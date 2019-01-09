source('predict_systematic.R')

open_gov_models <- list(
  linear=list(label='Open Government', wrapper=lm_wrap, prepare=NULL),
  lasso_1se=list(label='Open Government', wrapper=lm_wrap, prepare=lasso_1se),
  lasso_min=list(label='Open Government', wrapper=lm_wrap, prepare=lasso_min),
  knn7=list(label='Open Government', wrapper=knn_wrap, prepare=NULL, k=7),
  knn7_1se=list(label='Open Government', wrapper=knn_wrap, prepare=lasso_1se, k=7),
  knn7_min=list(label='Open Government', wrapper=knn_wrap, prepare=lasso_min, k=7),
  svm=list(label='Open Government', wrapper=svm_wrap, prepare=NULL),
  xgb=list(label='Open Government', wrapper=xgb_wrap, prepare=NULL))
  

model_comp <- map_dbl(open_gov_models,xval_new)
# KNN7 with LASSO 1se seems to be the best option

###############################################################################
# Hyperparameter tuning with KNN
###############################################################################
knn_plain <- map_dbl(1:20,function(k) {
  in_list <- list(label='Open Government', wrapper=knn_wrap, prepare=NULL, k=k)
  xval_new(in_list)
})
qplot(1:20,knn_plain) + ggtitle('KNN with no feature selection')

knn_1se <- map_dbl(1:20,function(k) {
  in_list <- list(label='Open Government', wrapper=knn_wrap, prepare=lasso_1se, k=k)
  xval_new(in_list)
})
qplot(1:20,knn_1se) + ggtitle('KNN with 1se LASSO')

###############################################################################
# My old KNN hyperparameter code
###############################################################################
knn_xval <- function(d,k,fold=10,test_frac=0.1) {
  message('k = ',k)
  d <- d %>% select(-country,-varstr)
  nrmse <- sapply(1:fold,function(i) {
    split <- (runif(nrow(d)) < test_frac)
    train <- d[!split,]
    test <- d[split,]
    pred <- knn_wrap(train,test,k=k)
    sqrt(mean((test$value-pred)^2))/IQR(test$value)
  }) 
  ci <- t.test(nrmse)$conf.int
  data.frame(k=k,mean=mean(nrmse),lo=ci[1],hi=ci[2])
}

d <- all_jsr %>% 
  filter(varstr=='Open Government') %>%
  ml_prep(ifs_basic) %>%
  na.omit

knn_old <- plyr::ldply(1:25,function(k) knn_xval(d,k))
qplot(1:25,knn_old$mean)

# TODO: I'm really baffled as to why my old code is so much faster