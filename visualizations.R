library(ggplot2)
library(ggrepel)
library(dplyr)

###############################################################################
# Scatterplot visualization
###############################################################################
pred_scatter <- function(input_list,filter_year=NULL,test_frac=0.3,lo=NULL,hi=NULL,...) {
  d <- xval_prep(input_list)
  if (is.null(filter_year)) { filter_year <- max(d$year)}
  if (length(filter_year)==1) {
    title <- paste0(d$varstr[1],' (',filter_year,')')
  } else {
    title <- paste0(d$varstr[1],' (',first(filter_year),'-',last(filter_year),')')
  }
  test <- get_test(d,input_list,test_frac)
  plotme <- test %>% select(country,year,value,pred)
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
# Handy function to spot systematic errors
###############################################################################
countries_wrong <- function(input_list,test_frac=0.3,fold=10) {
  # TODO: this is now getting used in a bunch of different places; I should
  # factor it out
  d <- all_jsr %>% 
    filter(varstr==input_list$label) %>%
    ml_prep(ifs_basic) %>%
    na.omit
  if (!is.null(input_list$feature)) {
    d <- input_list$feature(d)
  }
  plyr::ldply(1:fold,function(i) {
    n <- nrow(d)
    split <- (runif(n) < test_frac)
    train <- d[!split,]  %>% select(-country,-varstr)
    tmp <- d[split,] 
    test <- tmp %>% select(-country,-varstr)
    if (!is.null(input_list$prepare)) {
      prep <- input_list$prepare(list(train=train,test=test))
      train <- prep$train
      test <- prep$test
    }
    pred <- input_list$wrapper(train,test)
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


