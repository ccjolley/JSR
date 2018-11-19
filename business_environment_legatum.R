###############################################################################
# Extract Business Environment Index from Legatum dataset
###############################################################################
library(readxl)
library(dplyr)
library(reshape2)

business_environment <- read_excel('~/Projects/JSR/Legatum_All_data_2007-2017.xlsx',sheet='busi') %>%
  select(country,region,region2,starts_with('busi')) %>% 
  melt(id.vars=c('country','region','region2')) %>%
  mutate(year = sub('busi','',variable) %>% as.numeric) %>%
  rename(busi=value) %>%
  select(country,region,region2,year,busi)


