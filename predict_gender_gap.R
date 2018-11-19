source('extract_indices.R')
source('utils.R')

###############################################################################
# Load relevant IFs series
###############################################################################

wd <- getwd()
setwd('./IFs_exports/gender')
gender_vars <- load_ifs(list.files(pattern='.*txt$')) 
setwd(wd)

###############################################################################
# Join with lib_dem
###############################################################################

gender_gap <- fix_adm0(gender_gap)
gender_vars <- fix_adm0(gender_vars)
# Do country names match up?
setdiff(gender_vars$country,gender_gap$country)
setdiff(gender_gap$country,gender_vars$country)

j <- left_join(gender_gap,gender_vars,by=c('country','year')) %>% filter(year > 1959)
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
# R^2 of 0.75, and most are significant
j2$pred <- predict(my_lm,lm_me)

###############################################################################
# Visualize agreement for an individual country
###############################################################################
j2 %>%
  filter(country=='Nigeria') %>%
  select(year,value,pred) %>%
  melt(id.vars='year') %>%
  ggplot(aes(x=year,y=value,group=variable,color=variable)) +
  geom_line()
# TODO: These aren't great -- don't capture ups and downs much.
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
  filter(year==2012) %>%
  ggplot(aes(x=value,y=pred,color=highlight)) +
  geom_point() +
  theme(legend.position='none') +
  geom_text_repel(aes(label=label)) +
  xlab('Measured value') +
  ylab('Predicted value') +
  ggtitle('Economic gender gap')
# There's definitely a clustering effect, with some countries lagging far 
# behind others. Also a couple of countries (Nepal and Philippines jump out)
# seem to be consistently misjudged; may need to add more predictors to
# get those right.


mean(j2$nrmse) # 0.317694

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
  write.csv('predict_gender_gap.csv',row.names=FALSE)

###############################################################################
# Forecast based on IFs data
###############################################################################
future <- gender_vars %>% filter(year > 2018) 
future <- cbind(data.frame(year=future$year,value=NA),select(future,-year,-country))

future$forecast <- predict(my_lm,future)
future <- cbind(gender_vars %>% filter(year > 2018) %>% select(country),future)

write.csv(future,'forecast_gender_gap.csv',row.names=FALSE)
glimpse(future)
