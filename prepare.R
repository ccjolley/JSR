###############################################################################
# These preparation functions should each take a named list with the train
# and test datasets, and output a named list with the transformed train and
# test datasets. This will allow them to be chained together with pipes as
# needed.
###############################################################################

###############################################################################
# Normalization
###############################################################################
normalize <- function(l) {
  train <- l$train
  test <- l$test
  for (n in setdiff(names(test),c('country','year','value','varstr'))) { 
    test[[n]] <- test[[n]] - mean(train[[n]],na.rm=TRUE)
    test[[n]] <- test[[n]] / sd(train[[n]],na.rm=TRUE)
    train[[n]] <- scale(train[[n]])
  }
  list(train=train, test=test)
}

###############################################################################
# These feature-engineering functions should take a single argument, which is
# a dataframe passed to them *before* the train/test split. Rather than 
# transforming existing features (e.g. through normalization), these functions
# should be creating new features in a way that won't constitute data snooping.
###############################################################################

###############################################################################
# Feature selection: LASSO 
###############################################################################
lasso_1se <- function(d) {
  x <- d %>% select(-year,-value,-country,-varstr) %>% as.matrix
  y <- d$value
  cv.out <- cv.glmnet(x,y,alpha=1,family='gaussian',type.measure = 'mse')
  tmp <- coef(cv.out,s=cv.out$lambda.1se) 
  n <- colnames(x)[tmp@i]
  d %>% select(country,varstr,year,value,n)
}

lasso_min <- function(d) {
  x <- d %>% select(-year,-value,-country,-varstr) %>% as.matrix
  y <- d$value
  cv.out <- cv.glmnet(x,y,alpha=1,family='gaussian',type.measure = 'mse')
  tmp <- coef(cv.out,s=cv.out$lambda.min) 
  n <- colnames(x)[tmp@i]
  d %>% select(country,varstr,year,value,n)
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
