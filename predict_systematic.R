source('utils.R')
source('extract_indices.R')
source('IFs_plots.R')
source('wrappers.R')
source('prepare.R')
library(ggrepel)
library(glmnet)
library(purrr)

load_all_ifs <- function() {
  if(!file.exists('IFs_imputed.rds')) {
    imputed_ifs <- load_raw_ifs() %>% ml_impute_loess
    saveRDS(imputed_ifs,'IFs_imputed.rds')
    imputed_ifs
  } else {
    readRDS('IFs_imputed.rds')
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
xval <- function(input_list,fold=10,test_frac=0.1,verbose=TRUE,
                     return_tbl=FALSE) {
  if (verbose) {
    show <- names(input_list) %in% c('wrapper','prepare','feature')
    mesg <- as.character(input_list[!show]) # don't show wrapper, preparation
    print(mesg)
  }
  d <- all_jsr %>% 
    filter(varstr==input_list$label) %>%
    ml_prep(ifs_basic) %>%
    na.omit
  if (sum(!is.na(d$value)) < 200) { test_frac = 0.2}
  param_ind <- !(names(input_list) %in% c('label','wrapper','prepare','feature'))
  params <- input_list[param_ind]
  if (!is.null(input_list$feature)) {
    d <- input_list$feature(d)
  }
  nrmse <- map_dbl(1:fold,function(i) {
    split <- (runif(nrow(d)) < test_frac)
    test <- d[split,] %>% select(-country,-varstr)
    train <- d[!split,] %>% select(-country,-varstr)
    if (!is.null(input_list$prepare)) {
      prep <- input_list$prepare(list(train=train,test=test))
      train <- prep$train
      test <- prep$test
    }
    pred <- do.call(input_list$wrapper,c(list(train,test),params))
    sqrt(mean((test$value-pred)^2))/IQR(test$value)
  }) 
  if (return_tbl) {
    ci <- t.test(nrmse)$conf.int
    return(data.frame(mean=mean(nrmse),lo=ci[1],hi=ci[2]))
  }
  mean(nrmse)
}



###############################################################################
# As a first step toward model selection, compare how different algorithms
# do with default parameters.
###############################################################################
model_compare <- function(varstr) {
  models <- list(
    linear=list(label=varstr, wrapper=lm_wrap, prepare=NULL),
    lasso_1se=list(label=varstr, wrapper=lm_wrap, prepare=lasso_1se),
    lasso_min=list(label=varstr, wrapper=lm_wrap, prepare=lasso_min),
    knn7=list(label=varstr, wrapper=knn_wrap, prepare=NULL, k=7),
    knn7_1se=list(label=varstr, wrapper=knn_wrap, prepare=lasso_1se, k=7),
    knn7_min=list(label=varstr, wrapper=knn_wrap, prepare=lasso_min, k=7),
    svm=list(label=varstr, wrapper=svm_wrap, prepare=NULL),
    xgb=list(label=varstr, wrapper=xgb_wrap, prepare=NULL)
  )
  ifs_basic <- load_all_ifs()
  lyr::ldply(models,xval,return_tbl=TRUE) %>%
    mutate(.id=fct_reorder(.id,desc(mean)),
           label=ifelse(mean==min(mean),round(mean,3),NA)) %>%
    ggplot(aes(x=.id,y=mean)) +
      geom_bar(stat='identity',fill='#A7C6ED') +
      theme_USAID +
      ylab('NRMSE') + xlab(NULL) +
      geom_errorbar(aes(ymin=lo,ymax=hi), width=0.5) +
      coord_flip() +
      geom_text(aes(y=0.05,label=label),hjust=-0.1)
}

###############################################################################
# KNN hyperparameter tuning
###############################################################################
knn_tune <- function(varstr, prepare=NULL, feature=NULL, kmax=25) {
  models <- plyr::llply(1:kmax,function(k) {
    list(label=varstr, wrapper=knn_wrap, prepare=prepare, feature=feature, k=k)
  })
  names(models) <- paste0('k_',1:kmax)
  compare <- plyr::ldply(models,xval,fold=100,return_tbl=TRUE) 
  result <- paste0('Best: ',round(min(compare$mean),3),', k = ',which.min(compare$mean))
  compare %>% arrange(mean) %>% print
  compare %>%
    mutate(k=row_number()) %>%
    ggplot(aes(x=k,y=mean)) +
    geom_errorbar(aes(ymin=lo,ymax=hi), width=0.5,color='#6C6463') +
    geom_point(size=2,color='#BA0C2F') +
    theme_USAID +
    ylab('NRMSE') + xlab('k') +
    annotate('label',y=(max(compare$mean)+max(compare$hi))/2,x=kmax/5,label=result)
}

###############################################################################
# SVM hyperparameter tuning
# With just two parameters, a grid search with 8 in each direction probably
# makes more sense than a random search.
###############################################################################
## Delete this later
varstr <- "Tax Administration"
ntry <- 60
cost_range=c(1,1e5)
eps_range=c(0,1)
prepare <- NULL
feature <- NULL

svm_tune <- function(varstr, ntry=60, fold=10, prepare=NULL, feature=NULL,
                     cost_range=c(1,1e5),eps_range=c(0,1)) {
  models <- plyr::llply(1:ntry,function(i) {
    eps <- runif(1,min=eps_range[1],max=eps_range[2]) %>% round(4)
    lcost_range <- log(cost_range)
    cost <- runif(1,min=lcost_range[1],max=lcost_range[2]) %>% exp %>% round(1)
    list(label=varstr, wrapper=svm_wrap, prepare=prepare, feature=feature, cost=cost, epsilon=eps)
  })
  names(models) <- map_chr(models,function(x) paste0('svm_',x$cost,'_',x$epsilon))
  compare <- plyr::ldply(models,xval,fold=fold,return_tbl=TRUE) 
  compare$cost <- map_dbl(models,'cost')
  compare$epsilon <- map_dbl(models,'epsilon')
  compare %>% arrange(mean) %>% head(10) %>% print
  ggplot(compare,aes(x=epsilon,y=cost,color=mean)) +
    geom_point(size=2) +
    theme_USAID +
    scale_color_distiller(palette = "Spectral") +
    scale_y_log10() +
    ylab('Cost') + xlab('Epsilon') 
}

###############################################################################
# xgboost hyperparameter tuning
# I feel like this needs to take a couple of steps; first some manual tuning
# to get close to the ideal of 100 iterations before automatic stopping, then
# tuning parameters (including nrounds) by randomly exploring 60 points close
# to the set of parameters that gets me 100 rounds.
###############################################################################
svm_tune <- function(varstr, prepare=NULL, feature=NULL, kmax=25) {
  
}
