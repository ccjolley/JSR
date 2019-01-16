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

###############################################################################
# Initial model testing
###############################################################################
model_compare('Tax Administration')
# Most models (except linear) are comparable, with no real consistency in ranking
# when I run this repeatedly.

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
knn_tune('Tax Administration') 
# best: 0.52, k=24 -- but really, everything above about k=4 looks pretty similar.
knn_tune('Tax Administration',feature=lasso_1se) 
# best: 0.45, k=16 (k=10 also not bad)
knn_tune('Tax Administration',feature=lasso_min) 
# best: 0.50, k=10

knn_best <- list(label='Tax Administration', wrapper=knn_wrap, feature=lasso_1se, k=10)

pred_scatter(knn_best,filter_year=c(2012,2016))
# generally goes in the right direction, but with a lot of spread
countries_wrong(knn_best,fold=30) %>% head(10)

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