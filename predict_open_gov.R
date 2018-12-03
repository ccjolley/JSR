source('predict_systematic.R')

ifs_basic <- load_all_ifs()
open_gov_ifs <- all_jsr %>% filter(varstr=='Open Government') %>%
  ml_prep(ifs_basic) %>% na.omit

xval(open_gov_ifs,lm_wrap,fold=100) # 22.57; threw rank-deficiency error
xval(open_gov_ifs,svm_wrap,fold=100) # 0.461
xval(open_gov_ifs,norm_svm_wrap,fold=100) # 0.445; normalization doesn't help much
xval(open_gov_ifs,xgb_wrap,fold=100) # 0.4036

# This is the worst performance on any of the models; ideally I'd like to get this down below about 0.25.

# Try selecting a smaller set of predictors with LASSO

og_smaller <- open_gov_ifs %>% select(country,year,value,varstr,get_predictors(open_gov_ifs))
names(og_smaller)
xval(og_smaller,lm_wrap,fold=100) # 0.317 - no longer rank-deficient
xval(og_smaller,svm_wrap,fold=100) # 0.354 
xval(og_smaller,norm_svm_wrap,fold=100) # 0.372
xval(og_smaller,xgb_wrap,fold=100) # 0.377
# try getting number of rounds instead
xval(og_smaller,xgb_wrap,fold=10,nrounds=NULL)
xval(og_smaller,xgb_wrap,fold=100,nrounds=18) # 0.3612
# with the number of rounds that low, I should probably slow down 
# the learning rate

more_predictors <- open_gov_ifs %>% 
  add_geo %>% 
  add_gender_disparities %>% 
  get_predictors(cutoff='min')

og_bigger <- open_gov_ifs %>% 
  add_geo %>% 
  add_gender_disparities %>% 
  select(country,year,value,varstr,more_predictors)
names(og_bigger)

xval(og_bigger,lm_wrap,fold=100) # 0.286 - best so far
xval(og_bigger,svm_wrap,fold=100) # 0.400
xval(og_bigger,xgb_wrap,fold=100) # 0.353

# I guess I shouldn't be shocked that with less data the simpler model with
# fewer predictors wins!

pred_scatter(og_bigger,lm_wrap)

countries_wrong(og_smaller,lm_wrap,fold=100) %>% head(5)
# So... what do Malaysia, Hungary, and Turkey have in common that leads to overestimates?
# What do Nepal and South Korea have in common that leads to underestimates?
# In almost all of the component measures, SK is near the top and Nepal near the bottom...
countries_wrong(og_bigger,lm_wrap,fold=100) %>% head(5)
# problems persist when I add more predictors, though avg deviations have gotten smaller

library(FNN)
knn_tune <- plyr::ldply(1:25,function(k) knn_xval_old(og_smaller,k,fold=100))
ggplot(knn_tune,aes(x=k,y=mean)) +
  geom_line(color='#BA0C2F') +
  geom_errorbar(aes(ymax=hi,ymin=lo),width=0.5) +
  theme_USAID + colors_USAID

knn_tune2 <- plyr::ldply(1:25,function(k) knn_xval(og_bigger,k,fold=100))
ggplot(knn_tune2,aes(x=k,y=mean)) +
  geom_line(color='#BA0C2F') +
  geom_errorbar(aes(ymax=hi,ymin=lo),width=0.5) +
  theme_USAID + colors_USAID
# Doesn't do as well as og_smaller does.
