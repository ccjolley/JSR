source('extract_indices.R')
source('utils.R')

jsr <- rbind(biodiversity_habitat,business_environment,child_health,diag_acc,
             export_div,gender_gap,gov_effect,group_equality,ict_use,lib_dem,
             open_gov,safety,tax_admin,trade_freedom) %>% 
  mutate(value=as.numeric(as.character(value))) %>%
  filter(year > 1959) # should get rid of a few ex-countries

###############################################################################
# Geographic coverage
###############################################################################
jsr <- fix_adm0(jsr)

jsr_recent <- jsr %>%
  group_by(country,varstr) %>%
  summarize(year=last(year))

geo_count <- table(jsr_recent$country) %>% 
  as.data.frame %>%
  rename(country=Var1,value=Freq)

# so who only gets one? For the most part, places that aren't really countries
geo_count %>% filter(value==1)
geo_count %>% filter(value==2) # small island countries 
geo_count %>% filter(value==3) # ex-countries (from V-Dem) or small islands
geo_count %>% filter(value==4) # small islands
geo_count %>% filter(value==5) # South Sudan is a real country
geo_count %>% filter(value==6)

geo_count <- geo_count %>%
  mutate(label=ifelse(value < 3,'1 - 2',NA),
         label=ifelse(value > 2 & value < 10,'3 - 9',label),
         label=ifelse(value > 9 & value < 13,'10 - 12',label),
         label=ifelse(value > 12,'13 - 14',label),
         label=factor(label,c('1 - 2','3 - 9','10 - 12','13 - 14')))

geo_count %>% mutate(value = label) %>%
  world_map('JSR metric coverage')

###############################################################################
# Most recent data point
###############################################################################
ggplot(jsr_recent,aes(x=year)) +
  geom_histogram(binwidth=1) 

# The peak at 2006 is from the Tax Administration series; these are only there for
# 2001 and 2006. There's a peak at 2012 from ICT use; only a few have 2013 or 2014 data.

###############################################################################
# Overall time coverage
###############################################################################
ggplot(jsr,aes(x=year)) +
  geom_histogram(binwidth=1)

# Things pick up after about 1995.

plot_me <- jsr %>%
  filter(country %in% usaid_countries) %>%
  group_by(varstr,year) %>%
  summarize(count=n()) %>%
  mutate(label=ifelse(count < 70,'< 70',NA),
         label=ifelse(count > 69 & count < 83,'70 - 82',label),
         label=ifelse(count > 82 & count < 94,'83 - 93',label),
         label=ifelse(count > 93 & count < 100,'94 - 99',label),
         label=ifelse(count > 99,'100 - 101',label),
         label=factor(label,c('< 70','70 - 82','83 - 93','94 - 99','100 - 101'))) %>%
  rename(Country_count=label)

ggplot(plot_me,aes(x=count)) + geom_histogram()
# bin into <70,70-82,83-93,94-99,100-101

ggplot(plot_me,aes(x=year,y=varstr,color=Country_count)) +
  geom_point() + ylab(NULL) +
  scale_x_continuous(limits=c(1960,2018))


