source('predict_systematic.R')

###############################################################################
# Trying to improve model performance
###############################################################################

ifs_basic <- load_all_ifs()
group_equality <- all_jsr %>% filter(varstr=='Group Equality')
group_equality_ifs <- ml_prep(group_equality,ifs_basic)

xval(group_equality_ifs,lm_wrap) # 0.568
xval(group_equality_ifs,xgb_wrap) # 0.295 -- better than linear, but slow

# this won't work until I've interpolated missing values
ge_pred_1se <- get_predictors(group_equality_ifs)


###############################################################################
# Rough plot for Kenya report
###############################################################################
jsr <- group_equality
country_ <- 'Kenya'
fore <- jsr_forecast(jsr,ifs_wlast,xgb_wrap)
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
  mutate(country=fct_reorder(country,value,.desc=TRUE,.fun=last)) 
ggplot(all_fore,aes(x=year,y=value,group=country,color=country)) +
  geom_line(size=1) +
  theme_USAID + colors_USAID +
  geom_vline(xintercept=cutoff,linetype=2,color='black') +
  annotate('text',x=cutoff,y=-0.5,hjust=-0.2,label='Forecast') +
  annotate('text',x=cutoff,y=-0.5,hjust=1.2,label='History') +
  xlab('Year') + ylab('Score') +
  labs(title='Group Equality Index',
       caption='History Source: Varieties of Democracy')

