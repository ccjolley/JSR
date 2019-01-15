source('predict_systematic.R')
ifs_basic <- load_all_ifs()

model_compare('Tax Administration')
# looks like all models except linear are within a decent confidence interval of 
# each other. Best is KNN with k=7 and no LASSO

###############################################################################
# k-nearest neighbors
###############################################################################
knn_tune('Tax Administration') 
# best: 0.52, k=24 -- but really, everything above about k=4 looks pretty similar.
knn_tune('Tax Administration',feature=lasso_1se) 
# best: 0.45, k=16 (k=10 also not bad)
knn_tune('Tax Administration',feature=lasso_min) 
# best: 0.50, k=10

knn_best <- list(label='Tax Administration', wrapper=knn_wrap, feature=lasso_1se, k=10)

pred_scatter(knn_best)
# generally goes in the right direction, but with a lot of spread

###############################################################################
# SVM
###############################################################################
svm_tune('Tax Administration',fold=100)
# This looks terrible -- my top-scoring hyperparameter combinations are 
# spread all over the place. Best scores around 0.495 (not beating KNN!)

svm_tune('Tax Administration',prepare=normalize, fold=100)
# best here was 0.494

lasso_norm <- function(d) {
  d %>% lasso_1se %>% normalize
}
svm_tune('Tax Administration',prepare=lasso_norm, fold=100)
# TODO: doesn't quite work yet

# TODO: maybe SVM would benefit from PCA transformation of data?