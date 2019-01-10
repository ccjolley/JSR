source('predict_systematic.R')
source('wrappers.R')
source('prepare.R')
ifs_basic <- load_all_ifs()

tax_admin_models <- list(
  linear=list(label='Tax Administration', wrapper=lm_wrap, prepare=NULL),
  lasso_1se=list(label='Tax Administration', wrapper=lm_wrap, prepare=lasso_1se),
  lasso_min=list(label='Tax Administration', wrapper=lm_wrap, prepare=lasso_min),
  knn7=list(label='Tax Administration', wrapper=knn_wrap, prepare=NULL, k=7),
  knn7_1se=list(label='Tax Administration', wrapper=knn_wrap, prepare=lasso_1se, k=7),
  knn7_min=list(label='Tax Administration', wrapper=knn_wrap, prepare=lasso_min, k=7),
  svm=list(label='Tax Administration', wrapper=svm_wrap, prepare=NULL),
  xgb=list(label='Tax Administration', wrapper=xgb_wrap, prepare=NULL))


model_comp <- map_dbl(tax_admin_models,xval_new)
# lasso_1se seems to do the best