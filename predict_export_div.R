source('predict_systematic.R')
source('visualizations.R')
ifs_basic <- load_all_ifs()

###############################################################################
# Some basic exploration
###############################################################################
all_jsr %>% 
  filter(varstr=='Export Diversification') %>%
  group_by(year) %>%
  summarize(count=n()) 
# covers years 1995-2016

all_jsr %>% filter(varstr=='Export Diversification') %>%
  group_by(country) %>%
  summarize(value=mean(value)) %>%
  arrange(value) %>% head(10)
# Low-scoring countries are fairly wealthy

all_jsr %>% filter(varstr=='Export Diversification') %>%
  group_by(country) %>%
  summarize(value=mean(value)) %>%
  arrange(desc(value)) %>% head(10)
# Low-scoring countries are major oil producers

all_jsr %>%
  filter(year==2016,varstr %in% c('Export Diversification','GDP per capita')) %>%
  select(country,value,varstr) %>%
  dcast(country ~ varstr) %>%
  rename(export=2,gdppc=3) %>%
  na.omit %>%
  ggplot(aes(x=export,y=gdppc)) +
    geom_point(size=2,color='#A7C6ED') +
    theme_USAID
# It's complicated.

# TODO: turn this into a function I can re-use with other indices
get_trend <- function(row) {
  year <- names(row)[2:ncol(row)] %>% as.numeric
  value <- row[1,2:ncol(row)] %>% as.numeric
  coef <- lm(value ~ year)$coefficients[2]
  data_frame(country=row$country,coef=coef)
}

all_jsr %>%
  filter(varstr=='Export Diversification') %>%
  select(-varstr) %>%
  dcast(country ~ year) %>%
  na.omit %>%
  plyr::adply(1,get_trend) %>%
  select(country,`2016`,coef) %>%
  rename(y2016=2) %>%
  ggplot(aes(x=y2016,y=coef)) +
  geom_point(size=2,color='#A7C6ED') +
  theme_USAID +
  xlab('Export Diversification') +
  ylab('Average change per year 1995-2016')

# The lowest-scoring countries (mostly rich) don't change much. 
# It looks like an annual change of 0.02 is pretty dramatic; that would
# correspond to an overall change of 0.42 over the 20-year window. 
# TODO: add some labels to this plot so we know who those outliers were.
