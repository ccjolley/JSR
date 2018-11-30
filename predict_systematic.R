source('utils.R')
source('extract_indices.R')
source('IFs_plots.R')
library(ggrepel)
library(glmnet)

###############################################################################
# Load all IFs series. I should take a closer look at these to see if some
# might need further engineering, e.g., by normalizing by population or 
# calculating gender differences. Put these in a different function, though.
###############################################################################
load_all_ifs <- function() {
  wd <- getwd()
  setwd('./IFs_exports/biz')
  biz_vars <- load_ifs(list.files(pattern='.*txt$')) 
  setwd('../demo')
  demo_vars <- load_ifs(list.files(pattern='.*txt$'))
  setwd('../ed')
  ed_vars <- load_ifs(list.files(pattern='.*txt$'))
  setwd('../gender')
  gender_vars <- load_ifs(list.files(pattern='.*txt$'))
  setwd('../gov_finance')
  govfin_vars <- load_ifs(list.files(pattern='.*txt$'))
  setwd('../governance')
  gov_vars <- load_ifs(list.files(pattern='.*txt$'))
  setwd('../health')
  health_vars <- load_ifs(list.files(pattern='.*txt$'))
  setwd('../ICT')
  ict_vars <- load_ifs(list.files(pattern='.*txt$'))
  setwd('../trade')
  trade_vars <- load_ifs(list.files(pattern='.*txt$'))
  setwd('..')
  root_vars <- load_ifs(c('gini.txt','urban.txt','pop_density.txt','population.txt'))
  setwd(wd)
  full_join(biz_vars,demo_vars,by=c('country','year')) %>%
    full_join(ed_vars,by=c('country','year')) %>%
    full_join(gender_vars,by=c('country','year')) %>%
    full_join(govfin_vars,by=c('country','year')) %>%
    full_join(gov_vars,by=c('country','year')) %>%
    full_join(health_vars,by=c('country','year')) %>%
    full_join(ict_vars,by=c('country','year')) %>%
    full_join(trade_vars,by=c('country','year')) %>%
    full_join(root_vars,by=c('country','year'))
}

###############################################################################
# Join a JSR series and an IFs dataframe into an ML-ready dataframe. Keep
# specification of the ifs dataframe flexible (instead of just calling 
# load_all_ifs) so that I can customize as needed.
###############################################################################
ml_prep <- function(jsr,ifs) {
  res <- left_join(fix_adm0(jsr),
                   fix_adm0(ifs),
                   by=c('country','year')) %>% 
    filter(year > 1959) 
  # remove variables that are NA for all countries & years
  res[,colMeans(is.na(res)) < 1] 
}

################################################################################
# To fill in missing IFs values, call an impute function. For now just using
# something simple (median); could try something more sophisticated later.
#
# NOTE: Always call this *after* the train-test split; otherwise we are data
# snooping!
###############################################################################
median_sub <- function(s) {
  s[is.na(s)] <- median(s[!is.na(s)])
  s
}

loess_sub <- function(df) {
  # assumes that df contains two columns, with the variable of interest and the year
  names(df) <- c('x','year')
  loess_x <- loess(x ~ year,data=df)
  pred_yr <- df$year[is.na(df$x)]
  pred_x <- predict(loess_x,pred_yr)
  j <- left_join(df,data.frame(xpred=pred_x,year=pred_yr),by='year') %>%
    mutate(x=ifelse(is.na(x),xpred,x)) %>%
    select(-xpred)
  if (sum(is.na(j$x)) == 0) { return(j) }
  # if there are still NAs, extrapolate using a linear function, but bound 
  # extrapolation at the historical min and max values
  lm_x <- lm(x ~ year,data=j)
  new_data <- data.frame(year = j$year[is.na(j$x)])
  new_data$xpred <- predict(lm_x,new_data)
  x_min <- min(j$x,na.rm=TRUE)
  x_max <- max(j$x,na.rm=TRUE)
  new_data <- new_data %>%
    mutate(xpred=ifelse(xpred < x_min,x_min,xpred),
           xpred=ifelse(xpred > x_max,x_max,xpred))
  left_join(j,new_data,by='year') %>%
    mutate(x=ifelse(is.na(x),xpred,x)) %>%
    select(-xpred)
}

ml_impute <- function(d,impute_fn=median_sub) {
  res <- d %>% 
    group_by(country) %>%
    arrange(year) %>%
    mutate_at(vars(-value,-varstr,-country),impute_fn) %>% 
    ungroup %>%
    na.omit
}

ml_impute_loess <- function(d,impute_fn=loess_sub) {
  # TODO: need to modify this to work with my new loess imputation...
}

###############################################################################
# Reusable cross-validation code
###############################################################################
xval <- function(d,wrap_fun,fold=10,test_frac=0.1,lo=NULL,hi=NULL) {
  if (sum(!is.na(d$value)) < 200) { test_frac = 0.2}
  sapply(1:fold,function(i) {
    n <- nrow(d)
    split <- (runif(n) < test_frac)
    test <- d[split,] %>% ml_impute %>% select(-country,-varstr)
    train <- d[!split,]  %>% ml_impute %>% select(-country,-varstr)
    if (!is.null(lo) & !is.null(hi)) {
      train$value <- make_infinite(train$value,lo,hi)
    }
    pred <- wrap_fun(train,test)
    if (!is.null(lo) & !is.null(hi)) {
      pred <- make_finite(pred,lo,hi)
    }
    iqr <- quantile(test$value,probs=0.75) - quantile(test$value,probs=0.25)
    sqrt(mean((test$value-pred)^2))/iqr
  }) %>% mean
}

###############################################################################
# Scatterplot visualization
###############################################################################
pred_scatter <- function(d,wrap_fun,filter_year=NULL,test_frac=0.3,lo=NULL,hi=NULL) {
  if (is.null(filter_year)) { filter_year <- max(d$year)}
  if (length(filter_year)==1) {
    title <- paste0(d$varstr[1],' (',filter_year,')')
  } else {
    title <- paste0(d$varstr[1],' (',first(filter_year),'-',last(filter_year),')')
  }
  n <- nrow(d)
  split <- (runif(n) < test_frac)
  if (!is.null(lo) & !is.null(hi)) {
    d$value <- make_infinite(d$value,lo,hi)
  }
  train <- d[!split,]  %>% ml_impute %>% select(-country,-varstr)
  tmp <- d[split,] %>% ml_impute 
  test <- tmp %>% select(-country,-varstr)
  plotme <- tmp %>% select(country,year,value)
  plotme$pred <- wrap_fun(train,test)
  if (!is.null(lo) & !is.null(hi)) {
    plotme$value <- make_finite(plotme$value,lo,hi)
    plotme$pred <- make_finite(plotme$pred,lo,hi)
  }  
  iqr <- quantile(plotme$value,probs=0.75) - quantile(plotme$value,probs=0.25)
  plotme %>%
    filter(year %in% filter_year) %>%
    mutate(ndev=abs(value-pred)/iqr,
           highlight=ndev > quantile(ndev,probs=0.95),
           label=ifelse(highlight,country,NA)) %>%
    ggplot(aes(x=value,y=pred,color=highlight)) +
      geom_point() +
      theme(legend.position='none') +
      geom_text_repel(aes(label=label)) +
      xlab('Measured value') +
      ylab('Predicted value') +
      ggtitle(title) +
      theme_USAID + colors_USAID
}

###############################################################################
# Single-country trendline visualization
###############################################################################
country_trend <- function(d,country_,wrap_fun) {
  title <- paste0(d$varstr[1],': ',country_)
  tmp <- d %>% ml_impute
  plotme <- tmp %>% select(country,year,value)
  fun_me <- tmp %>% select(-country,-varstr)
  plotme$pred <- wrap_fun(fun_me,fun_me)
  plotme %>%
    filter(country==country_) %>%
    melt(id.vars=c('country','year')) %>%
    ggplot(aes(x=year,y=value,group=variable,color=variable)) +
      geom_point(size=2) +
      geom_line(size=1) +
      theme_USAID + colors_USAID +
      xlab('Year') + ylab('Value') +
      ggtitle(title)
}

###############################################################################
# Forecasting
###############################################################################
jsr_forecast <- function(jsr,ifs,wrap_fun,lo=NULL,hi=NULL) {
  train <- ml_prep(jsr,ifs) %>% ml_impute %>% select(-varstr)
  test <- fix_adm0(ifs) %>%
    filter(year >= 2015) %>%
    na.omit 
  if (!is.null(lo) & !is.null(hi)) {
    train$value <- make_infinite(train$value,lo,hi)
  }
  test$value <- wrap_fun(select(train,-country),
                         select(test,-country))
  if (!is.null(lo) & !is.null(hi)) {
    train$value <- make_finite(train$value,lo,hi)
    test$value <- make_finite(test$value,lo,hi)
  }
  rbind(mutate(train,forecast='History'),mutate(test,forecast='Forecast'))
}

forecast_plot <- function(jsr,ifs,country_,wrap_fun) {
  title <- paste0(jsr$varstr[1],': ',country_)
  plotme <- jsr_forecast(jsr,ifs,wrap_fun) %>%
    select(country,year,value,forecast) %>%
    filter(country==country_)
  ggplot(plotme,aes(x=year,y=value)) +
    geom_line(size=0.5,linetype=2,color='#6C6463') +
    geom_line(size=1,aes(color=forecast)) +
    theme_USAID + colors_USAID +
    theme(legend.title=element_blank()) +
    xlab('Year') + ylab('Value') +
    ggtitle(title)
}

###############################################################################
# Model wrappers
###############################################################################
lm_wrap <- function(train,test) {
  if (!('value' %in% names(test))) { test$value <- NA }
  is_const <- (apply(train,2,var,na.rm=TRUE) == 0)
  is_const['value'] <- FALSE
  train <- train[!is_const]
  test <- test[!is_const]
  my_lm <- lm(value ~ .,data=train)
  predict(my_lm,test)
}

library(e1071)
svm_wrap <- function(train,test,eps=0.1,cost=64) {
  is_const <- (apply(train,2,var,na.rm=TRUE) == 0)
  train <- train[!is_const]
  test <- test[!is_const]
  my_svm <- svm(value ~ .,data=train,epsilon=eps,cost=cost)
  predict(my_svm,test)
}

norm_svm_wrap <- function(train,test,eps=0.1,cost=64) {
  is_const <- (apply(train,2,var,na.rm=TRUE) == 0)
  train <- train[!is_const]
  test <- test[!is_const]
  # scale test according to training data
  for (n in setdiff(names(test),c('country','year','value','varstr'))) { 
    test[[n]] <- test[[n]] - mean(train[[n]])
    test[[n]] <- test[[n]] / sd(train[[n]])
    train[[n]] <- scale(train[[n]])
  }
  my_svm <- svm(value ~ .,data=train,epsilon=eps,cost=cost)
  predict(my_svm,test)
}

library(xgboost)
xgb_wrap <- function(train,test,eta=0.3,max_depth=6,subsample=1,colsample_bytree=0.8,
                     verbose=0,nrounds=80) {
  if (!('value' %in% names(test))) { test$value <- NA}
  dtrain <- xgb.DMatrix(data=select(train,-value) %>% as.matrix,
                        label=train$value)
  dtest <- xgb.DMatrix(data=select(test,-value) %>% as.matrix,
                       label=test$value)
  if (is.null(nrounds)) { # using xval to determine nrounds
    watchlist <- list(train=dtrain, test=dtest)
    my_xgb <- xgb.train(data=dtrain,nrounds=200,watchlist=watchlist,  
                        eta=eta,max_depth=max_depth,subsample=subsample,colsample_bytree=subsample,
                        nthread=4,early_stopping_rounds=5,verbose=verbose)
    paste0(my_xgb$best_iteration,' iterations') %>% print
  } else {
    my_xgb <- xgb.train(data=dtrain,nrounds=nrounds,  
                        eta=eta,max_depth=max_depth,subsample=subsample,colsample_bytree=subsample,
                        nthread=4,verbose=verbose)
  }
  predict(my_xgb,dtest)
}

###############################################################################
# Eventually this will be a wrapper for KNN, but not until I figure out a
# better way to pass parameters into these wrappers.
# I'll also want some way to get a verson of xval() that gets me the sort of
# ldply-friendly output I get here for hyperparameter tuning.
###############################################################################
library(FNN)
knn_xval <- function(d,k,fold=10,test_frac=0.1) {
  nrmse <- sapply(1:fold,function(i) {
    split <- (runif(nrow(d)) < test_frac)
    train <- d[!split,]
    test <- d[split,]
    x <- train %>% select(-country,-year,-value,-varstr)
    xtest <- test %>% select(-country,-year,-value,-varstr) 
    # scale test according to training data
    for (n in names(xtest)) { 
      xtest[[n]] <- xtest[[n]] - mean(x[[n]])
      xtest[[n]] <- xtest[[n]] / sd(x[[n]])
    }
    x <- scale(x)
    y <- train$value
    knn_i <- knn.reg(train=x,test=xtest,y=train$value,k=k)
    iqr <- quantile(y,probs=0.75) - quantile(y,probs=0.25)
    sqrt(mean((test$value-knn_i$pred)^2))/iqr
  }) 
  ci <- t.test(nrmse)$conf.int
  data.frame(k=k,mean=mean(nrmse),lo=ci[1],hi=ci[2])
}

###############################################################################
# Feature engineering: Get indicators for membership in geographic regions
###############################################################################
add_geo <- function(d) {
  d %>% mutate(
    africa = ifelse(country %in% africa,1,0),
    asia = ifelse(country %in% asia,1,0),
    lac = ifelse(country %in% lac,1,0),
    me = ifelse(country %in% me,1,0),
    e_and_e = ifelse(country %in% e_and_e,1,0),
    usaid_country = ifelse(country %in% usaid_countries,1,0),
    high_income = ifelse(country %in% high_income,1,0),
    umic = ifelse(country %in% umic,1,0),
    lmic = ifelse(country %in% lmic,1,0),
    low_income = ifelse(country %in% low_income,1,0))
}

###############################################################################
# Feature engineering: Convert gender-disaggregated variables into disparity measures.
# Scaling such that they should be close to one if male and female rates are
# equal, 0 if females are zero, 2 if males are zero.
###############################################################################
add_gender_disparities <- function(d) {
  eps <- 1e-6 # avoid dividing by zero
  death_causes <- c('aids','cancer','diabetes','diarrhea','digestive','intinj',
                    'malaria','mental','othcomm','othnoncomm','respinfec','respiratory','traffic','unintinj')
  for (dc in death_causes) {
    male_str <- paste0('deaths_male_',dc)
    female_str <- paste0('deaths_female_',dc)
    new_str <- paste0('gender_disparity_',dc)
    d[,new_str] <- 2* d[,female_str]/(d[,female_str] + d[,male_str] + eps)
  }
  d$gender_disparity_smoking <- 2* d$smoking_female / (d$smoking_female + d$smoking_male + eps)
  d
}

###############################################################################
# Feature engineering: Add derivatives
###############################################################################
add_derivs <- function(d) {
  out <- d 
  ignore_vars <- c('country','varstr','value','year')
  get_deriv <- function(d) {
    # assumes columns are country, year, var
    names(d) <- c('country','year','x')
    countries <- d$country %>% unique
    plyr::ldply(countries,function(i) {
      di <- d %>% filter(country==i)
      x_loess <- loess(x ~ year,data=di)
      t_new <- min(di$year):max(di$year)
      pred <- predict(x_loess,t_new)
      dx_dt <- diff(pred) / diff(t_new)
      data.frame(country=i,year=t_new,slope=c(dx_dt,dx_dt[length(dx_dt)]))
    })
  }
  for (n in setdiff(names(d),ignore_vars)) {
    print(n)
    tmp_n <- d %>% select(country,year,n) 
    gd <- get_deriv(tmp_n)
    names(gd) <- c('country','year',paste0('slope_',n))
    out <- left_join(out,gd,by=c('country','year')) 
  }
  out
}

###############################################################################
# Feature selection: Get a reduced set of features using LASSO
###############################################################################
get_predictors <- function(d,show_plot=TRUE,cutoff='1se') {
  x <- d %>% select(-country,-year,-value,-varstr) %>% as.matrix
  y <- d$value
  cv.out <- cv.glmnet(x,y,alpha=1,family='gaussian',type.measure = 'mse')
  if (show_plot) { plot(cv.out) }
  if (cutoff=='1se') { tmp <- coef(cv.out,s=cv.out$lambda.1se) }
  else { tmp <- coef(cv.out,s=cv.out$lambda.min) }
  colnames(x)[tmp@i]
}

###############################################################################
# Handy function to spot systematic errors
###############################################################################
countries_wrong <- function(d,wrap_fun,filter_year=NULL,test_frac=0.3,fold=10) {
  plyr::ldply(1:fold,function(i) {
    n <- nrow(d)
    split <- (runif(n) < test_frac)
    train <- d[!split,]  %>% ml_impute %>% select(-country,-varstr)
    tmp <- d[split,] %>% ml_impute 
    test <- tmp %>% select(-country,-varstr)
    pred <- wrap_fun(train,test)
    tmp$ndev <- abs(pred - test$value)
    tmp$dev <- pred - test$value
    cutoff <- quantile(tmp$ndev,probs=0.95)
    worst <- tmp %>% 
      filter(ndev > cutoff) %>% 
      select(country,dev)
  }) %>%
    group_by(country) %>%
    summarize(count=n(),mean_dev=mean(dev)) %>%
    arrange(desc(count))
}
