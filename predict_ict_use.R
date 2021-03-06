source('extract_indices.R')
source('utils.R')

###############################################################################
# Load relevant IFs series
###############################################################################

wd <- getwd()
setwd('./IFs_exports/biz')
biz_vars <- load_ifs(list.files(pattern='.*txt$')) %>% select(-pop_internet)
setwd('../governance')
gov_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd('../ICT')
ict_vars <- load_ifs(list.files(pattern='.*txt$')) %>%
  full_join(biz_vars,by=c('country','year')) %>%
  full_join(gov_vars,by=c('country','year')) 
setwd(wd)

###############################################################################
# Join with ict_use
###############################################################################

ict_use <- fix_adm0(ict_use)
ict_vars <- ict_vars %>%  fix_adm0() 
# Do country names match up?
setdiff(ict_vars$country,ict_use$country)
setdiff(ict_use$country,ict_vars$country)

j <- left_join(ict_use,ict_vars,by=c('country','year')) %>% filter(year > 1959)
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
j2 <- na.omit(j2) 
lm_me <- j2 %>% select(-country,-varstr) # no fixed effects

my_lm <- lm(value ~ .,data=lm_me)
summary(my_lm)
# R^2 of 0.95; lots of significant variables
j2$pred <- predict(my_lm,lm_me)

###############################################################################
# Visualize agreement for an individual country
###############################################################################
j2 %>%
  filter(country=='Suriname') %>%
  select(year,value,pred) %>%
  melt(id.vars='year') %>%
  ggplot(aes(x=year,y=value,group=variable,color=variable)) +
  geom_line()
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
  filter(year==2013) %>% 
  ggplot(aes(x=value,y=pred,color=highlight)) +
  geom_point() +
  theme(legend.position='none') +
  geom_text_repel(aes(label=label)) +
  xlab('Measured value') +
  ylab('Predicted value') +
  ggtitle('ICT use')


mean(j2$nrmse) # 0.1300795

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
  write.csv('predict_ict_use.csv',row.names=FALSE)

###############################################################################
# Forecast based on IFs data
###############################################################################
future <- ict_vars %>% filter(year > 2018) 
future <- cbind(data.frame(year=future$year,value=NA),select(future,-year,-country))

future$forecast <- predict(my_lm,future)
future <- cbind(ict_vars %>% filter(year > 2018) %>% select(country),future)

write.csv(future,'forecast_ict_use.csv',row.names=FALSE)
glimpse(future)
