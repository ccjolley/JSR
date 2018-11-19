source('extract_indices.R')
source('utils.R')
library(ggrepel)

###############################################################################
# Load relevant IFs series
###############################################################################

wd <- getwd()
setwd('./IFs_exports/biz')
biz_vars <- load_ifs(list.files(pattern='.*txt$')) 
setwd('../governance')
gov_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd('../gov_finance')
gov_fin <- load_ifs(list.files(pattern='.*txt$'))
setwd('..')
gini <- load_ifs('gini.txt')
setwd(wd)
tax_vars <- full_join(biz_vars,gov_vars,by=c('country','year')) %>%
  full_join(gov_fin,by=c('country','year')) %>%
  full_join(gini,by=c('country','year'))

###############################################################################
# Join with tax_admin
###############################################################################

tax_admin <- fix_adm0(tax_admin)
tax_vars <- tax_vars %>%  fix_adm0()
# Do country names match up?
setdiff(tax_vars$country,tax_admin$country)
setdiff(tax_admin$country,tax_vars$country)

j <- left_join(tax_admin,tax_vars,by=c('country','year')) %>% filter(year > 1959)

nrow(j)
nrow(na.omit(j))
colSums(is.na(j)) / nrow(j)

# I need some way of inferring sparsely-measured indices (like gender empowerment)
# for now, just fill in missing values with median for the same country

# because I have so few time samples, I can't just take the median across 2001
# and 2006 -- too many missing variables. Unlike previous models, build medians
# with tax_vars rather than j (might be a better way to do this anyway).
medians <- tax_vars %>%
  select(-year) %>%
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
lm_me <- j2 %>% select(-country,-varstr) # no fixed effects

my_lm <- lm(value ~ .,data=lm_me)
summary(my_lm)
# R^2 of 0.76; most variables not so significant
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
  ggtitle('Efficiency of tax administration')



mean(j2$nrmse) # 0.235708

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
  write.csv('predict_tax_admin.csv',row.names=FALSE)

###############################################################################
# Forecast based on IFs data
###############################################################################
future <- tax_vars %>% filter(year > 2018) 
future <- cbind(data.frame(year=future$year,value=NA),select(future,-year,-country))

future$forecast <- predict(my_lm,future)
future <- cbind(tax_vars %>% filter(year > 2018) %>% select(country),future)

write.csv(future,'forecast_tax_admin.csv',row.names=FALSE)
glimpse(future)
