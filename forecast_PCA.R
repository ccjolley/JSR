source('PCA.R')
source('predict_systematic.R')

# Haven't tested this to make sure it works yet
ifs_basic <- load_all_ifs()

fc_commitment <- list(lib_dem,open_gov,group_equality,gender_gap,
                      business_environment,trade_freedom,biodiversity_habitat) %>%
  plyr::ldply(function(x) {
    v <- x$varstr[1]
    print(v)
    if (v == 'Biodiversity & Habitat') { 
      lo <- 0
      hi <- 100
    } else {
      lo <- NULL
      hi <- NULL
    }
    jsr_forecast(x,ifs_basic,xgb_wrap,lo=lo,hi=hi) %>%
      filter(year >= 2015) %>%
      select(country,year,value) %>%
      mutate(varstr=v)
  }) %>%
  dcast(country + year ~ varstr,fun.aggregate = mean,na.rm=TRUE) 

commit_pc1 <- fc_commitment %>%
  predict(commitment_pr,newdata=.) %>%
  as.data.frame %>%
  select(PC1) %>%
  rename(commitment=PC1) %>%
  cbind(fc_commitment %>% select(country,year),.)

# Need to tweak tax admin a little
ta_countries <- tax_admin$country %>% unique
tax_admin2 <- tax_admin %>%
  mutate(country=as.character(country)) %>%
  right_join(data.frame(country=rep(ta_countries,16),
                        year=rep(2000:2015,each=length(ta_countries)),
                        stringsAsFactors=FALSE),
             by=c('country','year')) %>%
  mutate(varstr=tax_admin$varstr[1])

fc_capacity <- list(gov_effect,tax_admin2,safety,diag_acc,
                    child_health,ict_use,export_div) %>%
  plyr::ldply(function(x) {
    v <- x$varstr[1]
    print(v)
    if (v %in% c('Child Health','Poverty rate')) { 
      lo <- 0
      hi <- 100
    } else {
      lo <- NULL
      hi <- NULL
    }
    jsr_forecast(x,ifs_basic,xgb_wrap,lo=lo,hi=hi) %>%
      filter(year >= 2015) %>%
      select(country,year,value) %>%
      mutate(varstr=x$varstr[1])
  }) %>% 
  rbind(read_tsv('IFs_exports/poverty_forecast.txt',col_names = c('country','year','value')) %>%
          filter(year >= 2015) %>% fix_adm0 %>%
          mutate(varstr='Poverty rate')) %>%
  rbind(read_tsv('IFs_exports/gdppc.txt',col_names = c('country','year','value')) %>%
          filter(year >= 2015) %>% fix_adm0 %>%
          mutate(varstr='GDP per capita',
                 value=value*1000)) %>% # need to change units to match WB data
  dcast(country + year ~ varstr,fun.aggregate = mean,na.rm=TRUE)

capacity_pc1 <- fc_capacity %>%
  predict(capacity_pr,newdata=.) %>%
  as.data.frame %>%
  select(PC1) %>%
  transmute(capacity=-PC1) %>%
  cbind(fc_capacity %>% select(country,year),.)

forecast_2d <- left_join(commit_pc1,capacity_pc1,by=c('country','year'))

## Sanity checks
jsr_commit_sq %>% as.data.frame %>% summarize_all(max,na.rm=TRUE)
fc_commitment %>% as.data.frame %>% summarize_all(max,na.rm=TRUE)

jsr_capacity_sq %>% as.data.frame %>% summarize_all(max,na.rm=TRUE)
fc_capacity %>% as.data.frame %>% summarize_all(min,na.rm=TRUE)

fc_capacity %>% filter(country=='China',year<2019) %>% select(year,`GDP per capita`)
gdppc %>% filter(country=='China',year>2014) %>% select(year,value)

fc_capacity %>% filter(country=='China',year<2019) %>% select(year,`Poverty rate`)
poverty %>% filter(country=='China',year>2014) %>% select(year,value)

###############################################################################
# The plot of awesomeness
###############################################################################
jsr_pca_plot <- function(show_country) {
  addline <- forecast_2d %>% 
    filter(country==show_country) %>%
    mutate(label=ifelse(year %% 5 == 0,year,NA))
  ggplot(jsr_join,aes(x=capacity,y=commitment)) +
    geom_point(color='#A7C6ED') +
    xlab('Capacity') + ylab('Commitment') +
    theme_USAID +
    geom_line(data=addline,aes(x=capacity,y=commitment),size=1,color='#BA0C2F') +
    geom_text_repel(data=addline,aes(x=capacity,y=commitment,label=label),color='#BA0C2F') +
    ggtitle(show_country)
}

jsr_pca_plot('Kenya')
jsr_pca_plot('Norway')
jsr_pca_plot('Somalia')
jsr_pca_plot('Nigeria')
