source('predict_systematic.R')
source('wrappers.R')
source('prepare.R')

model_compare('Tax Administration')
# looks like all models except linear are within a decent confidence interval of 
# each other. Best is KNN with k=7 and no LASSO

knn_tune('Tax Administration') 
# best: 0.52, k=24 -- but really, everything above about k=3 looks pretty similar.
knn_tune('Tax Administration',prepare=lasso_1se) 
# this takes forever, might want to put LASSO step outside of xval loop...
# TODO: best way to do this might be to specify two different types of 
# pre-processing; a feature selection step that can go outside of the xval
# loop and operations like normalization that can go inside of it
knn_tune('Tax Administration',prepare=lasso_min) 