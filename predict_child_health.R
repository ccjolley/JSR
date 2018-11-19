source('predict_systematic.R')

ifs_basic <- load_all_ifs()
jsr <- child_health
my_ifs <- ml_prep(jsr,ifs_basic)

xval(my_ifs,lm_wrap) # 0.175
pred_scatter(my_ifs,lm_wrap,filter_year=2012:2016)
country_trend(my_ifs,'Kenya',lm_wrap)
forecast_plot(jsr,ifs_basic,'Kenya',lm_wrap) # keeps going up-up-up

# TODO: try knn -- don't remember how to do this

# svm_tune <- tune(svm,value ~ .,
#                  data=select(lib_dem_ifs,-country,-varstr),
#                  ranges=list(epsilon=0.1*(1:10),
#                              cost=2^(0:9)))
# plot(svm_tune)
# print(svm_tune)

# this step is much more time-consuming for variables like this with lots of historical data
xval(my_ifs,svm_wrap) # 0.102
pred_scatter(my_ifs,svm_wrap,filter_year=2012:2016)
# because this splits into train and test now, results are sort of stochastic
# TODO: make plot use open circles instead of filled ones
# TODO: need to figure out how to pass arbitrary parameters into pred_scatter() and on to wrapper
country_trend(my_ifs,'Kenya',svm_wrap)
forecast_plot(jsr,ifs_basic,'Kenya',svm_wrap) # peaks, then comes back down...

xval(my_ifs,xgb_wrap) # 0.078 -- maybe better than SVM
pred_scatter(my_ifs,xgb_wrap,filter_year=2012:2016)
country_trend(my_ifs,'Kenya',xgb_wrap)
forecast_plot(jsr,ifs_basic,'Kenya',xgb_wrap)

###############################################################################
# Rough plot for Kenya report
###############################################################################
country_ <- 'Kenya'
fore <- jsr_forecast(jsr,ifs_basic,xgb_wrap)
# TODO: probably a more elegant way to do this
c_fore <- fore %>% filter(country==country_) %>% select(year,value,country)
umic_fore <- fore %>% 
  filter(country %in% umic) %>%
  group_by(year) %>%
  summarize(value=mean(value)) %>%
  mutate(country='UMIC')
lmic_fore <- fore %>% 
  filter(country %in% lmic) %>%
  group_by(year) %>%
  summarize(value=mean(value)) %>%
  mutate(country='LMIC')
low_fore <- fore %>% 
  filter(country %in% low_income) %>%
  group_by(year) %>%
  summarize(value=mean(value)) %>%
  mutate(country='Low-income')
cutoff=xintercept=max(jsr$year)+0.5
all_fore <- rbind(c_fore,umic_fore,lmic_fore,low_fore) %>%
  mutate(country=fct_reorder(country,value,.desc=TRUE,.fun=median)) 
ggplot(all_fore,aes(x=year,y=value,group=country,color=country)) +
  geom_line(size=1) +
  theme_USAID + colors_USAID +
  geom_vline(xintercept=cutoff,linetype=2,color='black') +
  annotate('text',x=cutoff,y=85,hjust=-0.2,label='Forecast') +
  annotate('text',x=cutoff,y=85,hjust=1.2,label='History') +
  xlab('Year') + ylab('Score') +
  labs(title='Child Health Index',
       caption='History Source: Some acronym')

# This looks good for Kenya

###############################################################################
# Importance
###############################################################################
test_frac = 0.3
d <- my_ifs
n <- nrow(d)
split <- (runif(n) < test_frac)
train <- d[!split,]  %>% ml_impute %>% select(-country,-varstr)
test <- d[split,] %>% ml_impute %>% select(-country,-varstr)

dtrain <- xgb.DMatrix(data=select(train,-value) %>% as.matrix,
                      label=train$value)
dtest <- xgb.DMatrix(data=select(test,-value) %>% as.matrix,
                     label=test$value)
my_xgb <- xgb.train(data=dtrain,nrounds=100,nthread=4,verbose=1)
imp <- xgb.importance(model=my_xgb) %>%
  as.data.frame %>%
  mutate(logGain = log(Gain),
         label=ifelse(logGain > -5,Feature,NA))

ggplot(imp,aes(y=logGain,x=Cover)) +
  geom_point() +
  theme_USAID + colors_USAID +
  geom_text_repel(aes(label=label))
######## old stuff below here

###############################################################################
# Bias check: compare across different categories of countries
###############################################################################
nrmse_bar(j2) + ggtitle('Accuracy: Child Health')
bias_bar(j2) + ggtitle('Bias: Child Health')


###############################################################################
# Illustrative plot for Kenya
###############################################################################

plotme <- future %>% 
  filter(country=='Kenya') %>% 
  select(country,year,forecast) %>%
  rename(value=forecast) %>%
  mutate(varstr='Forecast') %>%
  rbind(child_health %>% filter(country=='Kenya') %>% mutate(varstr='Measured')) %>%
  mutate(varstr=factor(varstr,levels=c('Measured','Forecast'))) %>%
  arrange(year)

ggplot(plotme,aes(x=year,y=value)) +
  geom_line(color='#6C6463',linetype=2) +
  geom_point(data=filter(plotme,varstr=='Measured'),color='#002F6C') +
  geom_line(aes(group=varstr,color=varstr)) +
  theme_USAID +
  colors_USAID +
  theme(legend.title=element_blank()) +
  xlab('Year') + ylab('Child Health index') +
  ggtitle('Kenya: Child Health index')


