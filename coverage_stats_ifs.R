source('utils.R')

###############################################################################
# Load relevant IFs series
###############################################################################

wd <- getwd()
setwd('./IFs_exports/biz')
biz_vars <- load_ifs(list.files(pattern='.*txt$')) 
setwd('../demo')
demo_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd('../gender')
gender_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd('../gov_finance')
gov_finance_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd('../governance')
governance_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd('../health')
health_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd('../ICT')
ict_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd('../trade')
trade_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd('..')
other_vars <- load_ifs(list.files(pattern='.*txt$'))
setwd(wd)
all_vars <- biz_vars %>%
  full_join(demo_vars,by=c('country','year')) %>%
  full_join(gender_vars,by=c('country','year')) %>%
  full_join(gov_finance_vars,by=c('country','year')) %>%
  full_join(governance_vars,by=c('country','year')) %>%
  full_join(health_vars,by=c('country','year')) %>%
  full_join(ict_vars,by=c('country','year')) %>%
  full_join(trade_vars,by=c('country','year')) %>%
  full_join(other_vars,by=c('country','year')) 

###############################################################################
# Geographic coverage
###############################################################################
all_vars <- fix_adm0(all_vars)

geo_count <- all_vars %>% 
  melt(id.vars=c('country','year')) %>%
  group_by(country) %>%
  summarize(value=sum(!is.na(value)))

q <- quantile(geo_count$value)

geo_count <- geo_count %>%
  mutate(label=ifelse(value <= q[2],'Q1',NA),
         label=ifelse(value > q[2] & value <= q[3],'Q2',label),
         label=ifelse(value > q[3] & value <= q[4],'Q3',label),
         label=ifelse(value > q[4],'Q4',label),
         label=factor(label,c('Q1','Q2','Q3','Q4')))

geo_count %>% mutate(value = label) %>%
  world_map('IFs metric coverage')


###############################################################################
# Overall time coverage
###############################################################################

plot_me <- all_vars %>%
  filter(country %in% usaid_countries) %>%
  melt(id.vars=c('country','year')) %>%
  na.omit %>%
  group_by(variable,year) %>%
  summarize(count=n()) %>%
  mutate(label=ifelse(count < 20,'< 20',NA),
         label=ifelse(count > 20 & count < 40,'20 - 40',label),
         label=ifelse(count > 40 & count < 60,'40 - 60',label),
         label=ifelse(count > 60 & count < 80,'60 - 80',label),
         label=ifelse(count > 80,'> 80',label),
         label=factor(label,c('< 20','20 - 40','40 - 60','60 - 80','> 80'))) %>%
  rename(Country_count=label)

ggplot(plot_me,aes(x=count)) + geom_histogram()
# bin into <70,70-82,83-93,94-99,100-101

ggplot(plot_me,aes(x=year,y=variable,color=Country_count)) +
  geom_point() + ylab(NULL) +
  scale_x_continuous(limits=c(1960,2018))


