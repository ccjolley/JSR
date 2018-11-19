source('predict_systematic.R')

ifs_basic <- load_all_ifs()
jsr <- business_environment
my_ifs <- ml_prep(jsr,ifs_basic)

xval(my_ifs,lm_wrap) # 0.288
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
xval(my_ifs,svm_wrap) # 0.136
pred_scatter(my_ifs,svm_wrap,filter_year=2012:2016)
# because this splits into train and test now, results are sort of stochastic
# TODO: make plot use open circles instead of filled ones
# TODO: need to figure out how to pass arbitrary parameters into pred_scatter() and on to wrapper
country_trend(my_ifs,'Kenya',svm_wrap)
forecast_plot(jsr,ifs_basic,'Kenya',svm_wrap) # peaks, then comes back down...

xval(my_ifs,xgb_wrap) # 0.131 -- maybe better than SVM
pred_scatter(my_ifs,xgb_wrap,filter_year=2012:2016)
country_trend(my_ifs,'Kenya',xgb_wrap)
forecast_plot(jsr,ifs_basic,'Kenya',xgb_wrap) # not much net change

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
  annotate('text',x=cutoff,y=68,hjust=-0.2,label='Forecast') +
  annotate('text',x=cutoff,y=68,hjust=1.2,label='History') +
  xlab('Year') + ylab('Score') +
  labs(title='Business Environment Index',
       caption='History Source: Legatum Institute')

# With xgb, Kenya comes quickly back down to UMIC average level and tracks it on a slow increase
# with svm, everyone smoothly peaks and comes back down