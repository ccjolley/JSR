source('predict_systematic.R')

ifs_basic <- load_all_ifs()
open_gov_ifs <- ml_prep(open_gov,ifs_basic) %>% na.omit

xval(open_gov_ifs,lm_wrap,fold=100) # 32.01; threw rank-deficiency error
xval(open_gov_ifs,svm_wrap,fold=100) # 0.424
xval(open_gov_ifs,norm_svm_wrap,fold=100) # 0.4419; normalization doesn't help much
xval(open_gov_ifs,xgb_wrap,fold=100) # 0.452

# This is the worst performance on any of the models; ideally I'd like to get this down below about 0.25.

# Try selecting a smaller set of predictors with LASSO

og_smaller <- open_gov_ifs %>% select(country,year,value,varstr,get_predictors(open_gov_ifs))
xval(og_smaller,lm_wrap,fold=100) # 0.333 - no longer rank-deficient
xval(og_smaller,svm_wrap,fold=100) # 0.441 
xval(og_smaller,norm_svm_wrap,fold=100) # 0.441
xval(og_smaller,xgb_wrap,fold=100) # 0.401 

more_predictors <- open_gov_ifs %>% 
  add_geo %>% 
  add_gender_disparities %>% 
  get_predictors(cutoff='min')

og_bigger <- open_gov_ifs %>% 
  add_geo %>% 
  add_gender_disparities %>% 
  select(country,year,value,varstr,more_predictors)

xval(og_bigger,lm_wrap,fold=100) # 0.306 - best so far
xval(og_bigger,svm_wrap,fold=100) # 0.417
xval(og_bigger,xgb_wrap,fold=100) # 0.379

# I guess I shouldn't be shocked that with less data the simpler model with
# fewer predictors wins!

pred_scatter(og_bigger,lm_wrap)

countries_wrong(og_smaller,lm_wrap,fold=100) %>% head(5)
# So... what do Malaysia, Hungary, and Turkey have in common that leads to overestimates?
# What do Nepal and South Korea have in common that leads to underestimates?
# In almost all of the component measures, SK is near the top and Nepal near the bottom...
countries_wrong(og_bigger,lm_wrap,fold=100) %>% head(5)
# problems persist when I add more predictors, though avg deviations have gotten smaller

with_derivs <- ifs_basic %>% 
  add_gender_disparities %>% 
  add_derivs %>%
  add_geo

open_gov_deriv <- ml_prep(open_gov,with_derivs) %>% na.omit 
deriv_predictors_1se <- get_predictors(open_gov_deriv)
deriv_predictors_min <- get_predictors(open_gov_deriv,cutoff='min')

og_deriv_small <- open_gov_deriv %>% select(country,year,value,varstr,deriv_predictors_1se)
xval(og_deriv_small,lm_wrap,fold=100) # 0.3299
xval(og_deriv_small,svm_wrap,fold=100) # 0.3988
xval(og_deriv_small,xgb_wrap,fold=100) # 0.39523

og_deriv_big <- open_gov_deriv %>% select(country,year,value,varstr,deriv_predictors_min)
xval(og_deriv_big,lm_wrap,fold=100) # 0.3553
xval(og_deriv_big,svm_wrap,fold=100) # 0.3761
xval(og_deriv_big,xgb_wrap,fold=100) # 0.3756

# So adding in slope predictors doesn't do anything for my accuracy. I might be able
# to do more with a different algorithm (KNN? Ridge regression?) or by better tuning
# of the ones I've got. All of those things will require some work to enable better
# parameterization with the wrapper constructs I'm using.

library(FNN)

knn_tune <- plyr::ldply(1:25,function(k) knn_xval(og_smaller,k,fold=100))
ggplot(knn_tune,aes(x=k,y=mean)) +
  geom_line(color='#BA0C2F') +
  geom_errorbar(aes(ymax=hi,ymin=lo),width=0.5) +
  theme_USAID + colors_USAID

# Looks like my best bet is k=5, with NRMSE = 0.28166

knn_tune2 <- plyr::ldply(1:25,function(k) knn_xval(og_bigger,k,fold=100))
ggplot(knn_tune2,aes(x=k,y=mean)) +
  geom_line(color='#BA0C2F') +
  geom_errorbar(aes(ymax=hi,ymin=lo),width=0.5) +
  theme_USAID + colors_USAID
# Doesn't do as well as og_smaller does.

# This is still my worst-performing model, but improving it further would require
# hyperparameter tuning (for SVM, xgboost) and maybe also model stacking.