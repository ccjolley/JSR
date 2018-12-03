source('utils.R')
source('extract_indices.R')
source('IFs_plots.R')
library(ggrepel)
library(glmnet)

load_ifs <- function() {
  if(!file.exists('IFs_imputed.RData')) {
    
  }
}

###############################################################################
# Load all IFs series. I should take a closer look at these to see if some
# might need further engineering, e.g., by normalizing by population or 
# calculating gender differences. Put these in a different function, though.
###############################################################################
load_raw_ifs <- function() {
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
  setwd('../diplo')
  diplo_vars <- load_ifs(list.files(pattern='.*txt$'))
  setwd('../state_failure')
  state_fail_vars <- load_ifs(list.files(pattern='.*txt$'))
  setwd('../poverty')
  poverty_vars <- load_ifs(list.files(pattern='.*txt$'))
  setwd('..')
  root_vars <- load_ifs(c('gini.txt','urban.txt','pop_density.txt','population.txt',
                          'gdppc_mer.txt','gdppc_ppp.txt'))
  setwd(wd)
  full_join(biz_vars,demo_vars,by=c('country','year')) %>%
    full_join(ed_vars,by=c('country','year')) %>%
    full_join(gender_vars,by=c('country','year')) %>%
    full_join(govfin_vars,by=c('country','year')) %>%
    full_join(gov_vars,by=c('country','year')) %>%
    full_join(health_vars,by=c('country','year')) %>%
    full_join(ict_vars,by=c('country','year')) %>%
    full_join(trade_vars,by=c('country','year')) %>%
    full_join(diplo_vars,by=c('country','year')) %>%
    full_join(state_fail_vars,by=c('country','year')) %>%
    full_join(poverty_vars,by=c('country','year')) %>%
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
################################################################################
median_sub <- function(s) {
  s[is.na(s)] <- median(s[!is.na(s)])
  s
}

loess_sub <- function(df,extrapolation_window=10) {
  if (length(intersect(c('country','year','x'),names(df))) != 3) {
    message('ERROR in loess_sub, columns should be country, year, x')
    return(NULL)
  }
  all_years <- data.frame(year=min(df$year):max(df$year),country=df$country[1])
  df <- right_join(df,all_years,by=c('country','year'))
  if (sum(is.na(df$x)) == 0) { return(df) }
  loess_x <- loess(x ~ year,data=df)
  pred_yr <- df$year[is.na(df$x)]
  pred_x <- predict(loess_x,pred_yr)
  j <- left_join(df,data.frame(xpred=pred_x,year=pred_yr),by='year') %>%
    mutate(x=ifelse(is.na(x),xpred,x)) %>%
    select(-xpred)
  if (sum(is.na(j$x)) == 0) { return(j) }
  # if there are still NAs, they will be at the beginning of the series
  # use linear extrapolation, but bound extrapolation at the historical min 
  # and max values, and use a finite extrapolation window rather than the
  # entire series
  extrap_df <- j %>% na.omit %>% arrange(year) %>% head(extrapolation_window)
  lm_x <- lm(x ~ year,data=extrap_df)
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

# # test code for extrapolation
# ifs_basic %>% filter(country=='Russia') %>% select(fdi_inflows,year,country) %>%
#   rename(x=fdi_inflows) %>%
#   loess_sub %>%
#   ggplot(aes(x=year,y=x)) + geom_point() + geom_line()

ml_impute <- function(d,impute_fn=median_sub) {
  res <- d %>% 
    group_by(country) %>%
    arrange(year) %>%
    mutate_at(vars(-value,-varstr,-country),impute_fn) %>% 
    ungroup %>%
    na.omit
  res
}

library(purrr)
ml_impute_loess <- function(df,impute_fn=loess_sub) {
  col_list <- setdiff(names(df),c('country','year')) 
  for (n in col_list) {
    new_df <- df %>% 
      select(country,year,n) %>%
      rename(x=n) 
    imputed <- split(new_df,new_df$country) %>%
      map(loess_sub) %>%
      plyr::rbind.fill() %>%
      rename(xpred=x) 
    j <- left_join(new_df,imputed,by=c('country','year'))
    df[[n]] <- j$xpred
  }
  df
}
# TODO: do I still need ml_impute now that I have loess? Check to see what
# it's actually doing in the places where it's being called. To me, it makes
# sense (now) to impute just once on the IFs data at the beginning, rather
# than doing so after the train-test split, since I know that the future 
# data won't contain any missing values.


###############################################################################
# Reusable cross-validation code
###############################################################################
xval <- function(d,wrap_fun,fold=10,test_frac=0.1,lo=NULL,hi=NULL,...) {
  if (sum(!is.na(d$value)) < 200) { test_frac = 0.2}
  sapply(1:fold,function(i) {
    n <- nrow(d)
    split <- (runif(n) < test_frac)
    test <- d[split,] %>% ml_impute %>% select(-country,-varstr)
    train <- d[!split,]  %>% ml_impute %>% select(-country,-varstr)
    if (!is.null(lo) & !is.null(hi)) {
      train$value <- make_infinite(train$value,lo,hi)
    }
    pred <- wrap_fun(train,test,...)
    if (!is.null(lo) & !is.null(hi)) {
      pred <- make_finite(pred,lo,hi)
    }
    sqrt(mean((test$value-pred)^2))/IQR(test$value)
  }) %>% mean
}

###############################################################################
# Scatterplot visualization
###############################################################################
pred_scatter <- function(d,wrap_fun,filter_year=NULL,test_frac=0.3,lo=NULL,hi=NULL,...) {
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
  plotme$pred <- wrap_fun(train,test,...)
  if (!is.null(lo) & !is.null(hi)) {
    plotme$value <- make_finite(plotme$value,lo,hi)
    plotme$pred <- make_finite(plotme$pred,lo,hi)
  }  
  plotme %>%
    filter(year %in% filter_year) %>%
    mutate(ndev=abs(value-pred)/IQR(plotme$value),
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
  plotme$pred <- wrap_fun(fun_me,fun_me,...)
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
                         select(test,-country),...)
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
lm_wrap <- function(train,test,...) {
  if (!('value' %in% names(test))) { test$value <- NA }
  is_const <- (apply(train,2,var,na.rm=TRUE) == 0)
  is_const['value'] <- FALSE
  train <- train[!is_const]
  test <- test[!is_const]
  my_lm <- lm(value ~ .,data=train)
  predict(my_lm,test)
}

library(FNN)
knn_wrap <- function(train,test,...) {
  # filter values that are nearly constant
  is_const <- (apply(train,2,var,na.rm=TRUE) == 0)
  is_const['value'] <- FALSE
  train <- train[!is_const]
  test <- test[!is_const]
  # prepare for KNN
  x <- train %>% select(-value)
  xtest <- test %>% select(-value)
  # scale test according to training data
  for (n in names(xtest)) { 
    xtest[[n]] <- xtest[[n]] - mean(x[[n]])
    xtest[[n]] <- xtest[[n]] / sd(x[[n]])
  }
  x <- scale(x)
  my_knn <- knn.reg(train=x,test=xtest,y=train$value,...)
  my_knn$pred
}

library(e1071)
svm_wrap <- function(train,test,...) {
  is_const <- (apply(train,2,var,na.rm=TRUE) == 0)
  train <- train[!is_const]
  test <- test[!is_const]
  my_svm <- svm(value ~ .,data=train,... = )
  predict(my_svm,test)
}

norm_svm_wrap <- function(train,test,...) {
  is_const <- (apply(train,2,var,na.rm=TRUE) == 0)
  train <- train[!is_const]
  test <- test[!is_const]
  # scale test according to training data
  for (n in setdiff(names(test),c('country','year','value','varstr'))) { 
    test[[n]] <- test[[n]] - mean(train[[n]])
    test[[n]] <- test[[n]] / sd(train[[n]])
    train[[n]] <- scale(train[[n]])
  }
  my_svm <- svm(value ~ .,data=train,...)
  predict(my_svm,test)
}

library(xgboost)
xgb_wrap <- function(train,test,nrounds=100,...) {
  if (!('value' %in% names(test))) { test$value <- NA}
  dtrain <- xgb.DMatrix(data=select(train,-value) %>% as.matrix,
                        label=train$value)
  dtest <- xgb.DMatrix(data=select(test,-value) %>% as.matrix,
                       label=test$value)
  if (is.null(nrounds)) { # using xval to determine nrounds
    watchlist <- list(train=dtrain, test=dtest)
    my_xgb <- xgb.train(data=dtrain,nrounds=200,watchlist=watchlist,  
                        early_stopping_rounds=5,verbose=0,nthread=4,...)
    paste0(my_xgb$best_iteration,' iterations') %>% print
  } else {
    my_xgb <- xgb.train(data=dtrain,nrounds=nrounds,nthread=4,...)
  }
  predict(my_xgb,dtest)
}

###############################################################################
# Eventually this will be a wrapper for KNN, but not until I figure out a
# better way to pass parameters into these wrappers.
# I'll also want some way to get a verson of xval() that gets me the sort of
# ldply-friendly output I get here for hyperparameter tuning.
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
