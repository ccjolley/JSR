source('predict_systematic.R')
source('visualizations.R')
ifs_basic <- load_all_ifs()

###############################################################################
# Some basic exploration
###############################################################################
all_jsr %>% filter(varstr=='Tax Administration') %>%
  arrange(value) %>% head(10)
# some of the lowest-scoring countries are very poor (Chad, South Sudan), but
# others are middle-income (Iraq, Libya, Sri Lanka)

all_jsr %>% filter(varstr=='Tax Administration') %>%
  arrange(desc(value)) %>% head(10)
# all rich countries at the top

tax_admin_wide <- all_jsr %>% 
  filter(varstr=='Tax Administration') %>%
  select(country,year,value) %>%
  dcast(country ~ year) %>%
  na.omit 

ggplot(tax_admin_wide,aes(x=`2012`,y=`2016`)) +
  geom_jitter(size=2,color='#002F6C',alpha=0.5) +
  theme_USAID +
  ggtitle('Tax Administration scores')
# variation between the two years measured can be pretty substantial

sqrt(mean((tax_admin_wide$`2016` - tax_admin_wide$`2012`)^2)) / IQR(tax_admin_wide$`2016`)
# So if we just predicted 2016 values based on 2012 values, we'd have an NRMSE of about 0.81

tax_admin_wide %>% 
  mutate(diff=`2016` - `2012`) %>%
  arrange(desc(diff)) %>%
  head(10)

tax_admin_wide %>% 
  mutate(diff=`2016` - `2012`) %>%
  arrange(diff) %>%
  head(10)
# I don't see an obvious trend in who went up or down a lot between 2012 and 2016.

feature_list_lasso <- list(label='Tax Administration') %>% lasso_features(fold=20)
# gives me 31 high-impact features

feature_list_xgb <- list(label='Tax Administration') %>% xgb_features(fold=20)
# gives me 69 high-impact features

intersect(feature_list_lasso$feature,feature_list_xgb$feature)
###############################################################################
# Initial model testing
###############################################################################
model_compare('Tax Administration')
# Most models (except linear) are comparable, with no real consistency in ranking
# when I run this repeatedly. Best is typically around 0.42-0.46
in_sample_compare('Tax Administration')
# xgboost is the only one that's flexible enough to get to nearly zero error 
# on this dataset. SVM also overfits; LASSO helps reduce overfitting in linear.

lasso_best <- list(label='Tax Administration', wrapper=lm_wrap, feature=lasso_1se)
pred_scatter(lasso_best,filter_year=c(2012,2016))
# some are too high...

lasso_max <- list(label='Tax Administration', wrapper=lm_wrap, feature=lasso_1se, max_enforce=TRUE)
pred_scatter(lasso_max,filter_year=c(2012,2016))
countries_wrong(lasso_max,fold=30) %>% head(10)
# Major oil producers figure prominently here (Iraq, Kuwait, Saudi Arabia,
# Venezuela, Nigeria).
# We also see larger errors in countries that saw a lot of change between 
# 2012 and 2016 (Georgia, Turkmenistan, Myanmar). Might suggest that models
# that can get interactions with the year are going to work better.

###############################################################################
# k-nearest neighbors
###############################################################################
knn_simple <- list(label='Tax Administration', wrapper=knn_wrap, k=10)

lasso_select <- function(d) {
  d %>% select(country,varstr,year,value,feature_list_lasso$feature)
}

knn_lasso <- list(label='Tax Administration', wrapper=knn_wrap, k=10, 
                  feature=lasso_select)
xval(knn_simple) # NRMSE = 0.578
xval(knn_lasso) # NRMSE = 0.438

knn_tune('Tax Administration',feature=lasso_select) 
# best: 0.48, k=10 

knn_best <- list(label='Tax Administration', wrapper=knn_wrap, feature=lasso_1se, k=10)

pred_scatter(knn_best,filter_year=c(2012,2016))
# generally goes in the right direction, but with a lot of spread
countries_wrong(knn_best,fold=30) %>% head(10)

###############################################################################
# SVM
###############################################################################
svm_result <- svm_tune('Tax Administration', prepare=normalize, 
                       feature=lasso_select, fold=10, ntry=60)
head(svm_result)
tail(svm_result)
# difference between the best and worst SVM results is *almost* statistically
# significant

svm_check <- list(
  svm_default=list(label='Tax Administration', wrapper=svm_wrap, prepare=normalize, feature=lasso_select),
  svm_best=list(label='Tax Administration', wrapper=svm_wrap, prepare=normalize, feature=lasso_select,
                cost=svm_result$cost[1],epsilon=svm_result$epsilon[1],gamma=svm_result$gamma[1]),
  svm_worst=list(label='Tax Administration', wrapper=svm_wrap, prepare=normalize, feature=lasso_select,
                cost=svm_result$cost[60],epsilon=svm_result$epsilon[60],gamma=svm_result$gamma[60])
) %>% plyr::ldply(xval,return_tbl=TRUE,fold=100)

# SVM just isn't doing it here.

###############################################################################
# xgboost
###############################################################################
# First goal needs to be tuning to get about 100 iterations before early stopping
xval(list(label='Tax Administration',nrounds=NULL,wrapper=xgb_wrap),
     fold=10,test_frac=0.3,verbose=FALSE,return_tbl=TRUE)
# 8-22 iterations
xval(list(label='Tax Administration',nrounds=NULL,wrapper=xgb_wrap,
          eta=0.1),
     fold=10,test_frac=0.3,verbose=FALSE,return_tbl=TRUE)
# 27-72 iterations
xval(list(label='Tax Administration',nrounds=NULL,wrapper=xgb_wrap,
          eta=0.05),
     fold=10,test_frac=0.3,verbose=FALSE,return_tbl=TRUE)
# 55-175 iterations

xgb_try <- xgb_tune('Tax Administration',ntry=200)
# None of these are really dramatically better than what I'd seen before; 
# hyperparameter tuning isn't saving me.

