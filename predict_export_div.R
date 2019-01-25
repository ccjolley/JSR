source('predict_systematic.R')
source('visualizations.R')
ifs_basic <- load_all_ifs()

###############################################################################
# Some basic exploration
###############################################################################
all_jsr %>% 
  filter(varstr=='Export Diversification') %>%
  group_by(year) %>%
  summarize(count=n()) 
# covers years 1995-2016

all_jsr %>% filter(varstr=='Export Diversification') %>%
  group_by(country) %>%
  summarize(value=mean(value)) %>%
  arrange(value) %>% head(10)
# Low-scoring countries are fairly wealthy

all_jsr %>% filter(varstr=='Export Diversification') %>%
  group_by(country) %>%
  summarize(value=mean(value)) %>%
  arrange(desc(value)) %>% head(10)
# Low-scoring countries are major oil producers

all_jsr %>%
  filter(year==2016,varstr %in% c('Export Diversification','GDP per capita')) %>%
  select(country,value,varstr) %>%
  dcast(country ~ varstr) %>%
  rename(export=2,gdppc=3) %>%
  na.omit %>%
  ggplot(aes(x=export,y=gdppc)) +
    geom_point(size=2,color='#A7C6ED') +
    theme_USAID
# It's complicated.

# TODO: turn this into a function I can re-use with other indices
get_trend <- function(row) {
  year <- names(row)[2:ncol(row)] %>% as.numeric
  value <- row[1,2:ncol(row)] %>% as.numeric
  coef <- lm(value ~ year)$coefficients[2]
  data_frame(country=row$country,coef=coef)
}

plotme <- all_jsr %>%
  filter(varstr==v) %>%
  select(-varstr) %>%
  dcast(country ~ year) %>%
  na.omit %>%
  plyr::adply(1,get_trend) %>%
  select(country,as.character(maxyr),coef) %>%
  rename(recent=2)

v <- 'Export Diversification'
minyr <- all_jsr[all_jsr$varstr==v,'year'] %>% min
maxyr <- all_jsr[all_jsr$varstr==v,'year'] %>% max
ylabel <- paste0('Average change per year, ',minyr,'-',maxyr)
xlabel <- paste0(v,' (',maxyr,')')
val_extreme <- quantile(plotme$recent,probs=c(0.025,0.975))  
coef_extreme <- quantile(plotme$coef,probs=c(0.025,0.975))  
plotme %>%
  fix_adm0 %>%
  mutate(label=ifelse(recent < val_extreme[1] | recent > val_extreme[2], country,NA),
         label=ifelse(coef < coef_extreme[1] | coef > coef_extreme[2], country,label)) %>%
  ggplot(aes(x=recent,y=coef)) +
  geom_point(size=2,color='#A7C6ED') +
  geom_text_repel(aes(label=label),color='#651D32') +
  theme_USAID +
  xlab(xlabel) +
  ylab(ylabel)

# The lowest-scoring countries (mostly rich) don't change much. 
# It looks like an annual change of 0.02 is pretty dramatic; that would
# correspond to an overall change of 0.42 over the 20-year window. 
# A lot of the outliers are small island nations that have small economies and
# could see a lot of fluctuation. Not sure what the common thread is
# amoung countries showing decreases in the score.

###############################################################################
# Initial model testing
###############################################################################
model_compare('Export Diversification')
# All of the linear models underperform; others are pretty close together 
# but xgboost comes out in the lead.

###############################################################################
# Hyperparameter tuning: xgboost
###############################################################################
xgb_try <- xgb_tune('Export Diversification',ntry=200)

#TODO: move this function to predict_systematic.R
get_param_ranges <- function(d,sig_level=0.01) {
  n <- nrow(d)
  d <- d %>% arrange(mean)
  if (d[1,'hi'] < d[n,'lo']) {
    message('First and last rows differ significantly.')
  } else {
    message('No significant difference between first and last rows.')
  }
  params <- d %>% select(-mean,-lo,-hi) %>% names
  plyr::ldply(params,function(p) {
    tt1 <- t.test(d[1:10,p],d[(n-9):n,p])
    sig <- tt1$p.value 
    lo <- NA
    hi <- NA
    if (sig < sig_level) {
      tt2 <- t.test(d[1:20,p])
      lo <- tt2$conf.int[1]
      hi <- tt2$conf.int[2]
    }
    data.frame(param=p,sig=sig,lo=lo,hi=hi)
  })
}

get_param_ranges(xgb_try)

xgb_try2 <- xgb_tune('Export Diversification',ntry=200,
                     eta_range=c(0.19,0.3),subsample_range=c(0.65,0.84))
get_param_ranges(xgb_try2)

xgb_try3 <- xgb_tune('Export Diversification',ntry=200,
                     eta_range=c(0.19,0.3),subsample_range=c(0.65,0.84),
                     max_depth_range=c(6,7),nrounds_range=c(90,115))
get_param_ranges(xgb_try3)

xgb_try4 <- xgb_tune('Export Diversification',ntry=200,
                     eta_range=c(0.19,0.3),subsample_range=c(0.65,0.84),
                     max_depth_range=c(6,7),nrounds_range=c(90,115),
                     colsample_bytree_range=c(0.475,0.692))
get_param_ranges(xgb_try4,sig_level=0.05)

xgb_check <- list(
  xgb_default=list(label='Export Diversification', wrapper=xgb_wrap),
  xgb_best=list(label='Export Diversification', wrapper=xgb_wrap,
                eta=0.1953746, max_depth=7, subsample=0.7507471, 
                colsample_bytree=0.4867667,nrounds=96),
  xgb_worst=list(label='Export Diversification', wrapper=xgb_wrap,
                 eta=0.9621509, max_depth=7, subsample=0.008948478, 
                 colsample_bytree=0.5762655,nrounds=126)
) %>% plyr::ldply(xval,return_tbl=TRUE,fold=20)

# So... it looks like the xgboost defaults are pretty good in this case;
# even after tuning hyperparameters I'm not doing significantly better.
# range is 0.163 to 0.183

###############################################################################
# Hyperparameter tuning: SVM
###############################################################################
system.time(svm_tune('Export Diversification',ntry=1) %>% print)
# took 65 seconds

svm_initial <- list(
  svm_default=list(label='Export Diversification', wrapper=svm_wrap),
  svm_norm=list(label='Export Diversification', wrapper=svm_wrap, prepare=normalize),
  svm_1se=list(label='Export Diversification', wrapper=svm_wrap, feature=lasso_1se),
  svm_min=list(label='Export Diversification', wrapper=svm_wrap, feature=lasso_min),
  svm_1se_norm=list(label='Export Diversification', wrapper=svm_wrap, feature=lasso_1se, prepare=normalize),
  svm_min_norm=list(label='Export Diversification', wrapper=svm_wrap, feature=lasso_min, prepare=normalize)
) %>% plyr::ldply(xval,return_tbl=TRUE,fold=10)

# Looks like normalization helps a little, but restricting the feature set
# doesn't so much. 

svm_try1 <- svm_tune('Export Diversification',ntry=60) # let it run for an hour
svm_try1 %>% 
  mutate(lcost=log(cost),
         lgamma=log(gamma)) %>%
  get_param_ranges
# no significant differences -- hyperparameters aren't helping much

###############################################################################
# Hyperparameter tuning: KNN
###############################################################################
# TODO: make a function to get the LASSO min set and then tune KNN
knn_try1 <- knn_tune('Export Diversification')
