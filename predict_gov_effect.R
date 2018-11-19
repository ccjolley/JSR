source('extract_indices.R')
source('utils.R')

###############################################################################
# Load relevant IFs series
###############################################################################

wd <- getwd()
setwd('./IFs_exports/governance')
gov_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd(wd)

###############################################################################
# Join with lib_dem
###############################################################################

gov_effect <- fix_adm0(gov_effect)
gov_vars <- fix_adm0(gov_vars)
# Do country names match up?
setdiff(gov_vars$country,gov_effect$country)
setdiff(lib_dem$country,gov_vars$country)

j <- left_join(gov_effect,gov_vars,by=c('country','year')) %>% filter(year > 1959)
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
# R^2 of 0.99, and almost all are significant
j2$pred <- predict(my_lm,lm_me)

###############################################################################
# Visualize agreement for an individual country
###############################################################################
j2 %>%
  filter(country=='Japan') %>%
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
  filter(year==2000) %>%
  ggplot(aes(x=value,y=pred,color=highlight)) +
  geom_point() +
  theme(legend.position='none') +
  geom_text_repel(aes(label=label)) +
  xlab('Measured value') +
  ylab('Predicted value') +
  ggtitle('Government Effectiveness')

# The V-Dem and WB government effectiveness indices track each other really well, which makes this 
# pretty easy.

mean(j2$nrmse) # 0.02675007
# TODO: Fixed a normalization problem by dividing by IQR instead of median. Change
# the others to do this as well.

# This is weird:
j2$year[j2$highlight] %>% table
# For some reason my model doesn't do as well at the beginnng or end of the series

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
  write.csv('predict_gov_effect.csv',row.names=FALSE)

###############################################################################
# Forecast based on IFs data
###############################################################################
future <- gov_vars %>% filter(year > 2018) 
future <- cbind(data.frame(year=future$year,value=NA),select(future,-year,-country))

future$forecast <- predict(my_lm,future)
future <- cbind(gov_vars %>% filter(year > 2018) %>% select(country),future)

write.csv(future,'forecast_gov_effect.csv',row.names=FALSE)
glimpse(future)
