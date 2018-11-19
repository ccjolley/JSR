source('extract_indices.R')
source('utils.R')

library(ggrepel)

###############################################################################
# Load relevant IFs series
###############################################################################

wd <- getwd()
setwd('./IFs_exports/governance')
gov_vars <- load_ifs(list.files(pattern='.*txt$')) %>%
  fix_adm0
setwd(wd)

###############################################################################
# Join with open_gov
###############################################################################

open_gov <- fix_adm0(open_gov)
# Do country names match up?
setdiff(gov_vars$country,open_gov$country)
setdiff(open_gov$country,gov_vars$country)

j <- left_join(open_gov,gov_vars,by=c('country','year')) %>% filter(year > 1959)
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
lm_me <- j2 %>% select(-country,-varstr,-year) # no fixed effects, data for 2015 only

my_lm <- lm(value ~ .,data=lm_me)
summary(my_lm)
# R^2 of 0.86, and almost all are significant
j2$pred <- predict(my_lm,lm_me)

###############################################################################
# Visualize agreement, calculate NRMSE
###############################################################################
iqr <- quantile(j2$value,probs=0.75) - quantile(j2$value,probs=0.25)
j2 <- j2 %>%
  mutate(nrmse=sqrt((value-pred)^2)/iqr,
         highlight=nrmse > quantile(nrmse,probs=0.95),
         label=ifelse(highlight,country,NA))

j2 %>%
  ggplot(aes(x=value,y=pred,color=highlight)) +
  geom_point() +
  theme(legend.position='none') +
  geom_text_repel(aes(label=label)) +
  xlab('Measured value') +
  ylab('Predicted value') +
  ggtitle('Open Government')

mean(j2$nrmse) # 0.1996353

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
  write.csv('predict_open_gov.csv',row.names=FALSE)

###############################################################################
# Forecast based on IFs data
###############################################################################
future <- gov_vars %>% filter(year > 2018) 
future <- cbind(data.frame(year=future$year,value=NA),select(future,-year,-country))

future$forecast <- predict(my_lm,future)
future <- cbind(gov_vars %>% filter(year > 2018) %>% select(country),future)

write.csv(future,'forecast_open_gov.csv',row.names=FALSE)
glimpse(future)