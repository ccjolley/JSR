source('predict_systematic.R')
source('wrappers.R')
source('prepare.R')

ifs_basic <- load_all_ifs()

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

###############################################################################
# Updated version of cross-validation
# Move to predict_systematic.R when finished benchmarking
###############################################################################
xval_new <- function(input_list,fold=10,test_frac=0.1,verbose=TRUE,
                     return_tbl=FALSE) {
  if (verbose) {
    mesg <- as.character(input_list[-c(2,3)]) # don't show wrapper, preparation
    print(mesg)
  }
  d <- all_jsr %>% 
    filter(varstr==input_list$label) %>%
    ml_prep(ifs_basic) %>%
    na.omit
  if (sum(!is.na(d$value)) < 200) { test_frac = 0.2}
  params <- input_list[-c(1,2,3)]
  ti <- Sys.time()
  nrmse <- map_dbl(1:fold,function(i) {
    split <- (runif(nrow(d)) < test_frac)
    test <- d[split,] %>% select(-country,-varstr)
    train <- d[!split,] %>% select(-country,-varstr)
    if (!is.null(input_list$prepare)) {
      prep <- input_list$prepare(list(train=train,test=test))
      train <- prep$train
      test <- prep$test
    }
    pred <- do.call(input_list$wrapper,c(list(train,test),params))
    sqrt(mean((test$value-pred)^2))/IQR(test$value)
  }) 
  if (return_tbl) {
    ci <- t.test(nrmse)$conf.int
    return(data.frame(params,mean=mean(nrmse),lo=ci[1],hi=ci[2]))
  }
  mean(nrmse)
}

