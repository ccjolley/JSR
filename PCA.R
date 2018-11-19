source('utils.R')
source('extract_indices.R')
library(ggrepel)

jsr_commitment <- rbind(lib_dem,open_gov,group_equality,gender_gap,
                        business_environment,trade_freedom,
                        biodiversity_habitat) %>% 
  mutate(value=as.numeric(as.character(value))) %>%
  filter(year >= 1995) %>%
  fix_adm0

jsr_capacity <- rbind(gov_effect,tax_admin,safety,diag_acc,poverty,
                      child_health,gdppc,ict_use,export_div) %>% 
  mutate(value=as.numeric(as.character(value))) %>%
  filter(year >= 1995) %>%
  fix_adm0
# NOTE: still missing education quality data

jsr_commit_sq <- dcast(jsr_commitment,country + year ~ varstr,
                       fun.aggregate = mean,na.rm=TRUE)
jsr_capacity_sq <- dcast(jsr_capacity,country + year ~ varstr,
                         fun.aggregate = mean,na.rm=TRUE)
# NOTE: using an aggregation function because V-Dem indices contain separate
# values for West Bank and Gaza. Taking the average of these seems to make sense
# for now.

# There's no single year where everything is avaiable. Until we have models
# in place for all variables, just take the most recent.

jsr_commitment_recent <- jsr_commitment %>%
  arrange(year) %>%
  group_by(country,varstr) %>%
  summarize(value=last(value)) %>%
  dcast(country ~ varstr,fun.aggregate = mean,na.rm=TRUE) %>%
  na.omit

jsr_capacity_recent <- jsr_capacity %>%
  arrange(year) %>%
  group_by(country,varstr) %>%
  summarize(value=last(value)) %>%
  dcast(country ~ varstr,fun.aggregate = mean,na.rm=TRUE) %>%
  na.omit

# Now comes the PCA

commitment_pr <- jsr_commitment_recent %>%
  select(-country) %>%
  prcomp(scale=TRUE,center=TRUE)
summary(commitment_pr) # 53% of variance in first component
qplot(commitment_pr$x[,1],commitment_pr$x[,2])
# some heteroskedasticity -- not sure if that's an issue

capacity_pr <- jsr_capacity_recent %>%
  select(-country) %>%
  prcomp(scale=TRUE,center=TRUE)
summary(capacity_pr) # 67% of variance in first component
qplot(capacity_pr$x[,1],capacity_pr$x[,2])

###############################################################################
# Visualizations
###############################################################################
jsr_commitment_recent$commitment <- commitment_pr$x[,1]
jsr_commitment_recent %>%
  arrange(commitment) %>%
  select(country,commitment) %>%
  head
# so low values of commitment indicate low levels of commitment. Good.

jsr_capacity_recent$capacity <- -capacity_pr$x[,1]
jsr_capacity_recent %>%
  arrange(capacity) %>%
  select(country,capacity) %>%
  head
# Needed to invert to get this pointing the right direction

jsr_join <- jsr_commitment_recent %>%
  select(country,commitment) %>%
  inner_join(jsr_capacity_recent %>% select(country,capacity),by='country') %>%
  mutate(label=ifelse(capacity > 3.5 & commitment > 3,country,NA),
         label=ifelse(commitment < -2.5,country,label),
         label=ifelse(capacity < -3,country,label))

ggplot(jsr_join,aes(x=capacity,y=commitment)) +
  geom_point() +
  xlab('Capacity') + ylab('Commitment') +
  geom_text_repel(aes(label=label))
# This looks different than the ones on the JSR portal website. Part of this is
# because I've also included rich countries here; part is because I used PCA
# instead of scaling and averaging.

# Would be useful to annotate plots like this by income category, region,
# USAID presence, etc. 

###############################################################################
# PCA diagnostics - weight of each indicator
###############################################################################
commitment_pr$rotation %>% as.data.frame %>%
  select(PC1) %>%
  mutate(var=attr(commitment_pr$rotation,'dimnames')[[1]]) %>%
  ggplot(aes(x=var,y=PC1)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ylab('Contribution to commitment') +
  xlab(NULL)

capacity_pr$rotation %>% as.data.frame %>%
  select(PC1) %>% 
  mutate(var=attr(capacity_pr$rotation,'dimnames')[[1]],
         PC1 = -PC1) %>%    # because we inverted above as well
  ggplot(aes(x=var,y=PC1)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ylab('Contribution to capacity') +
  xlab(NULL)

