source('extract_indices.R')
source('utils.R')

###############################################################################
# Load relevant IFs series
###############################################################################

wd <- getwd()
setwd('./IFs_exports/governance')
gov_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd('../ed')
ed_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd('../demo')
demo_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd('..')
gini <- load_ifs('gini.txt')
setwd(wd)

da_vars <- full_join(gov_vars,ed_vars,by=c('country','year')) %>%
  full_join(demo_vars,by=c('country','year')) %>%
  full_join(gini,by=c('country','year'))


###############################################################################
# Join with diag_acc
###############################################################################

diag_acc <- fix_adm0(diag_acc)
da_vars <- fix_adm0(da_vars)
# Do country names match up?
setdiff(da_vars$country,diag_acc$country)
setdiff(diag_acc$country,da_vars$country)

j <- left_join(diag_acc,da_vars,by=c('country','year')) %>% filter(year > 1959)
nrow(j)
nrow(na.omit(j))
colSums(is.na(j))

# I need some way of inferring sparsely-measured indices (like gender empowerment)
# for now, just fill in missing values with median for the same country

medians <- j %>%
  select(-year,-varstr) %>%
  group_by(country) %>%
  summarise_all(median,na.rm=TRUE)
  
# TODO: find a faster way to do this
j2 <- j
for (i in 1:nrow(j2)) {
  for (n in names(j2)) {
    if (is.na(j2[[i,n]])) {
      c <- j2[[i,'country']]
      j2[i,n] <- medians[medians$country==c,n]
    }
  }
}
j2 <- na.omit(j2) # need this because of non-standard names
lm_me <- j2 %>% select(-country,-varstr) # no fixed effects

my_lm <- lm(value ~ .,data=lm_me)
summary(my_lm)
# R^2 of 0.79, and many are significant
j2$pred <- predict(my_lm,lm_me)

###############################################################################
# Visualize agreement for an individual country
###############################################################################
j2 %>%
  filter(country=='Spain') %>%
  select(year,value,pred) %>%
  melt(id.vars='year') %>%
  ggplot(aes(x=year,y=value,group=variable,color=variable)) +
  geom_line()
# TODO: I'm getting some things right, but overall the agreement is only OK.
# TODO: Eventually make these graphics LPA-compliant
  
###############################################################################
# Visualize agreement, calculate NRMSE
###############################################################################
iqr <- quantile(j2$value,probs=0.75) - quantile(j2$value,probs=0.25)
j2 <- j2 %>%
  mutate(nrmse=sqrt((value-pred)^2)/iqr,
         highlight=nrmse > quantile(nrmse,probs=0.95),
         label=ifelse(highlight,country,NA))

j2 %>%
  filter(year==2016) %>%
  ggplot(aes(x=value,y=pred,color=highlight)) +
  geom_point() +
  theme(legend.position='none') +
  geom_text_repel(aes(label=label)) +
  xlab('Measured value') +
  ylab('Predicted value') +
  ggtitle('Diagonal accountability')

mean(j2$nrmse) # 0.2105737

###############################################################################
# Save median deviations from best model
###############################################################################
j2 %>%
  group_by(country,varstr) %>%
  summarize(pred = median(pred),
            value = median(value)) %>%
  ungroup %>%
  mutate(pred = (pred - mean(value))/sd(value),
         value = scale(value)) %>%
  write.csv('predict_diag_acc.csv',row.names=FALSE)

###############################################################################
# Forecast based on IFs data
###############################################################################
# get future the same column ordering as lm_me
future <- da_vars %>% filter(year > 2018) 
future <- cbind(data.frame(year=future$year,value=NA),select(future,-year,-country))

future$forecast <- predict(my_lm,future)
future <- cbind(da_vars %>% filter(year > 2018) %>% select(country),future)

write.csv(future,'forecast_diag_acc.csv',row.names=FALSE)

###############################################################################
# Illustrative plot for Kenya
###############################################################################

future

plotme <- future %>% 
  filter(country=='Kenya') %>% 
  select(country,year,forecast) %>%
  rename(value=forecast) %>%
  mutate(varstr='Forecast') %>%
  rbind(diag_acc %>% filter(country=='Kenya') %>% mutate(varstr='Measured')) %>%
  mutate(varstr=factor(varstr,levels=c('Measured','Forecast'))) %>%
  arrange(year)

ggplot(plotme,aes(x=year,y=value)) +
  geom_line(color='#6C6463') +
  geom_point(data=filter(plotme,varstr=='Measured'),color='#002F6C') +
  geom_line(aes(group=varstr,color=varstr)) +
  theme_USAID +
  colors_USAID +
  theme(legend.title=element_blank()) +
  xlab('Year') + ylab('Diagonal Accountability index') +
  ggtitle('Kenya: Diagonal Accountability index')



