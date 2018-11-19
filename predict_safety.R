source('predict_systematic.R')

ifs_basic <- load_all_ifs()
safety_ifs <- ml_prep(safety,ifs_basic)

xval(safety_ifs,lm_wrap) # 0.269
pred_scatter(safety_ifs,lm_wrap,filter_year=2012:2016)
country_trend(safety_ifs,'Kenya',lm_wrap)
forecast_plot(safety,ifs_basic,'Kenya',lm_wrap) # keeps going up-up-up

# TODO: try knn -- don't remember how to do this

# svm_tune <- tune(svm,value ~ .,
#                  data=select(lib_dem_ifs,-country,-varstr),
#                  ranges=list(epsilon=0.1*(1:10),
#                              cost=2^(0:9)))
# plot(svm_tune)
# print(svm_tune)

# this step is much more time-consuming for variables like this with lots of historical data
xval(safety_ifs,svm_wrap) # never finished
pred_scatter(safety_ifs,svm_wrap,filter_year=2012:2016)
# because this splits into train and test now, results are sort of stochastic
# TODO: make plot use open circles instead of filled ones
# TODO: need to figure out how to pass arbitrary parameters into pred_scatter() and on to wrapper
country_trend(safety_ifs,'Kenya',svm_wrap)
forecast_plot(safety,ifs_basic,'Kenya',svm_wrap) # gets worse!

xval(safety_ifs,xgb_wrap) # 0.181
pred_scatter(safety_ifs,xgb_wrap,filter_year=2012:2016)
country_trend(safety_ifs,'Kenya',xgb_wrap)
forecast_plot(safety,ifs_basic,'Kenya',xgb_wrap) # more modest increase

###############################################################################
# Rough plot for Kenya report
###############################################################################
jsr <- safety
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
  labs(title='Safety & Security Index',
       caption='History Source: Legatum Institute')

