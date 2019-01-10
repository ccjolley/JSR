library(readxl)
library(dplyr)
library(reshape2)
library(readr)
library(ggplot2)
library(vdem)


# In addition to what's below, there are other indices
# - Poverty rate (already in IFs)
# - Per-capita GDP (already in IFs)
# - Education quality (still don't have data)

###############################################################################
# Liberal democracy index - VDem v2x_libdem
###############################################################################
lib_dem <- extract_vdem(name_pattern='v2x_libdem') %>%
  select(vdem_country_name,year,v2x_libdem) %>%
  rename(country=vdem_country_name,value=v2x_libdem) %>%
  mutate(varstr='Liberal Democracy') %>%
  na.omit

lib_dem %>%
  filter(country=='Japan') %>%
  ggplot(aes(x=year,y=value)) +
  geom_line() 

# IFs series to use:
# Civil and Political Freedom
# Polity Index
# Gender Empowerment
# Governance Corruption
# Governance Effectiveness
# Governance Regulatory Quality

###############################################################################
# Open government (WJP)
# Couldn't find a codebook, but from comparing with their PDF report it looks 
# like the column I want is f_3
###############################################################################
og_2013 <- read_excel('data/FINAL_2017-2018_wjp_rule_of_law_index_HISTORICAL_DATA_FILE.xlsx',
                      sheet='WJP ROL Index 2012-2013 Scores') %>%
  rename(country=Country, value=`Factor 5: Open Government`) %>%
  mutate(year=2013) %>% 
  select(country,year,value)

get_og <- function(year,sheetname=NULL) {
  if (is.null(sheetname)) {
    sheetname <- paste0('WJP ROL Index ',year,' Scores')
  }
  read_excel('data/FINAL_2017-2018_wjp_rule_of_law_index_HISTORICAL_DATA_FILE.xlsx',
             sheet=sheetname,col_names=FALSE) %>%
    filter(X__1=='Country' | X__1 =='Factor 3: Open Government') %>%
    t %>% as_tibble() %>%
    filter(V1 != 'Country') %>%
    rename(country=V1, value=V2) %>%
    mutate(year=year) %>%
    select(country,year,value)
}

open_gov <- rbind(og_2013,get_og(2014),get_og(2015),get_og(2016),
                  get_og(2018,'WJP ROL Index 2017-2018 Scores')) %>%
  mutate(value=as.numeric(value), 
         varstr='Open Government')

open_gov %>%
  filter(country %in% c('Japan','Iran','Kenya')) %>%
  ggplot(aes(x=year,y=value,group=country,color=country)) +
  geom_line() 
  
##############################################################################
# Social group equality- VDem v2clsocgrp
##############################################################################
group_equality <- extract_vdem(name_pattern='v2clsocgrp') %>%
  select(vdem_country_name,year,v2clsocgrp) %>%
  rename(country=vdem_country_name,value=v2clsocgrp) %>%
  mutate(varstr='Group Equality') %>%
  na.omit

group_equality %>%
  filter(country %in% c('United States','South Africa','Germany')) %>%
  ggplot(aes(x=year,y=value,group=country,color=country)) +
  geom_line() 

# IFs series to use:
# Civil and Political Freedom
# Polity Index
# Gender Empowerment
# Domestic Gini

##############################################################################
# Economic gender gap (think I got it from WB)
##############################################################################
gender_gap <- read_csv('data/Global_Gender_Gap.csv') %>%
  filter(Indicator=='Global Gender Gap Economic Participation and Opportunity Subindex') %>%
  filter(`Subindicator Type`=='Index') %>%
  select('Country Name',starts_with('20')) %>%
  rename(country=`Country Name`) %>%
  melt(id.vars='country') %>%
  mutate(year=as.numeric(as.character(variable)),
         varstr='Economic gender gap') %>%
  select(country,year,value,varstr) %>%
  na.omit

gender_gap %>%
  filter(country %in% c('United States','Denmark','Saudi Arabia')) %>%
  ggplot(aes(x=year,y=value,group=country,color=country)) +
  geom_line()

# IFs series to use:
# Gender empowerment
# Gender development index
# Gender gaps in health (F-M for BMI, death rates, years of disability, etc.)
# Gender gaps in education (F-M for all ed. metrics)
# HDI by sex (difference F-M)
# Informal Labor by sex

###############################################################################
# Business Environment Index (Legatum)
###############################################################################
business_environment <- read_excel('data/Legatum_All_data_2007-2017.xlsx',sheet='busi') %>%
  select(country,region,region2,starts_with('busi')) %>% 
  melt(id.vars=c('country','region','region2')) %>%
  mutate(year = sub('busi','',variable) %>% as.numeric) %>%
  select(country,year,value) %>%
  mutate(varstr='Business Environment')

# IFs series to use:
# Informal & shadow economy variables
# government consumption, revenue, taxes, debt
# education level
# population with internet
# governance variables, esp. regulatory quality
# labor productivity, multifactor contributions

# CHECK: are informal economy gdp driver variables duplicates?

###############################################################################
# Trade freedom  (Heritage)
###############################################################################
tf_read <- function(fname,year) {
  read_excel(fname) %>%
    select(`Country Name`,`Trade Freedom`) %>%
    rename(country=`Country Name`,
           value=`Trade Freedom`) %>%
    mutate(year=year,
           value=as.numeric(value),
           varstr='Trade Freedom') %>%
    na.omit
}

trade_freedom <- plyr::ldply(2013:2018,function(x) tf_read(paste0('data/Heritage_index',x,'_data.xls'),x))

trade_freedom %>%
  filter(country %in% c('Venezuela','Mexico','Brazil')) %>%
  ggplot(aes(x=year,y=value,group=country,color=country)) +
  geom_line()

# IFs indices to use:
# Exports, imports as % of GDP
# FDI
# maybe similar to biz environment index

###############################################################################
# Biodiversity & Habitat (EPI) - use backcasted scores from 2016 report
###############################################################################
epi_backcast <- function(year) {
  sheetname=paste0('EPI2016_',year,'.csv')
  read_excel('data/2016EPI_Backcasted_Scores.xls',sheet=sheetname) %>%
    select(country,starts_with('EV.BioHab')) %>%
    rename(value=starts_with('EV.BioHab')) %>%
    mutate(year=year,
           varstr='Biodiversity & Habitat') %>%
    select(country,year,value,varstr) %>%
    na.omit
}

biodiversity_habitat <- read_excel('data/2016_epi_framework_indicator_scores_friendly.xls',
                                   sheet='Indicator Scores') %>%
  select(Country,`EV- Biodiversity and Habitat`) %>%
  rename(country=Country,
         value=`EV- Biodiversity and Habitat`) %>%
  mutate(year=2016,
         varstr='Biodiversity & Habitat') %>%
  select(country,year,value,varstr) %>%
  rbind(plyr::ldply(2007:2015,epi_backcast))
  
biodiversity_habitat %>%
  filter(country %in% c('Venezuela','Mexico','Brazil')) %>%
  ggplot(aes(x=year,y=value,group=country,color=country)) +
  geom_line()

# IFs variables to use
# water demand by type
# land use?
# governance (esp regulatory quality)
# population density
# value added from ag, manufacturing, services
# maybe energy production variables?

###############################################################################
# Government effectiveness (WGI) - WB data, I think
###############################################################################
gov_effect <- read_csv('data/government_effectiveness.csv') %>%
  filter(Indicator=='Government Effectiveness',
         `Subindicator Type`=='Estimate') %>%
  rename(country=`Country Name`) %>%
  select(country,starts_with('19'),starts_with('20')) %>%
  melt(id.vars='country') %>%
  mutate(year=as.numeric(as.character(variable)),
         varstr='Government Effectiveness') %>%
  select(country,year,value,varstr) %>% 
  na.omit

gov_effect %>%
  filter(country %in% c('United States','Denmark','Japan')) %>%
  ggplot(aes(x=year,y=value,group=country,color=country)) +
  geom_line()

# same governance variables used for lib dem

###############################################################################
# Efficiency of tax administration (IPD)
# From J2SR website:
# Measures the efficiency of tax collection in relation to corporate taxes, 
# household income taxes, national geographic consistency and reach, and the 
# government's ability to limit tax evasion.
# Looks to me like there are four different indicators that cover this, all 
# collected in 2012 and 2016. These take on values from 0-4, or 99. It looks
# like the 99 is a "not applicable" result. I'll take the mean of these four
# indicators, dropping any missing values from the mean.
###############################################################################
ipd_all <- read_csv('data/tax_admin.csv')
names(ipd_all) <- c('country3','country','indicator','type','2001','2006','2009','2012','2016')
short_names <- data.frame(indicator=c('Ability to limit tax evasion',
                                 'Efficiency of the tax administration: corporation tax',
                                 'Efficiency of the tax administration: income tax',
                                 'Efficiency of the tax administration: national territory'),
                          short=c('evasion','corp','income','territory'))
tax_admin <- ipd_all %>% 
  filter(indicator %in% short_names$indicator) %>%
  select(country,indicator,starts_with('20')) %>%
  melt(id.vars=c('country','indicator')) %>% 
  mutate(value=ifelse(value==99,NA,value)) %>%
  na.omit %>% 
  left_join(short_names,by='indicator') %>%
  rename(year=variable) %>%
  select(country,year,value,short) %>%
  mutate(value=as.numeric(value),
         year=as.numeric(as.character(year))) %>%
  dcast(country + year ~ short) %>%
  rowwise() %>%
  mutate(value=mean(c(corp,evasion,income,territory),na.rm=TRUE)) %>%
  mutate(varstr='Tax Administration') %>%
  select(country,year,value,varstr)

###############################################################################
# Safety & Security (Legatum)
###############################################################################
safety <- read_excel('data/Legatum_All_data_2007-2017.xlsx',sheet='safe') %>%
  select(country,starts_with('safe')) %>% 
  melt(id.vars='country') %>%
  mutate(year = sub('safe','',variable) %>% as.numeric) %>%
  select(country,year,value) %>%
  mutate(varstr='Safety & Security')

safety %>%
  filter(country %in% c('Venezuela','Honduras','El Salvador')) %>%
  ggplot(aes(x=year,y=value,group=country,color=country)) +
  geom_line()

# IFs series to use:
# --> look at documentation; where does gov security come from?
# other governance variables
# injury death rate?
# demographics?
# inequality?
# state failure indices

###############################################################################
# Diagonal accountability (VDem) - v2x_diagacc
###############################################################################
diag_acc <- extract_vdem(name_pattern='v2x_diagacc') %>%
  select(vdem_country_name,year,v2x_diagacc) %>%
  rename(country=vdem_country_name,value=v2x_diagacc) %>%
  mutate(varstr='Diagonal Accountability') %>%
  na.omit

diag_acc %>%
  filter(country %in% c('United States','Japan','France')) %>%
  ggplot(aes(x=year,y=value,group=country,color=country)) +
  geom_line()

# IFs indices:
# try indices related to goverance, literacy, internet access, gender...?

###############################################################################
# Child health (CIESIN)
###############################################################################
child_health <- read_excel('data/nrpi-chi-2016.xlsx',sheet='NRPI & CHI, 2016') %>%
  rename(country=CountryName) %>%
  select(country,starts_with('CHI_v2016')) %>%
  melt(id.vars='country') %>%
  mutate(year = as.numeric(sub('CHI_v2016_','',variable))+2000,
         varstr='Child Health') %>%
  select(country,year,value,varstr) %>%
  na.omit

child_health %>%
  filter(country %in% c('Venezuela','Honduras','El Salvador')) %>%
  ggplot(aes(x=year,y=value,group=country,color=country)) +
  geom_line()

# IFs series to use:
# Infant mortality
# SAM prevalence
# primary ed enrollment
# death rate from communicable disease
# malnourished children
# susceptibility?
# median age?

###############################################################################
# Export sector diversification (UNCTAD))
###############################################################################
export_div <- read_csv('data/us_concentdiversindices_43313314970963.csv',skip=2,
                       col_names=FALSE) %>% 
  filter(X1 != 'ECONOMY')
export_div <- export_div[,c(1,(1:22)*3)]
names(export_div) <- export_div[1,] 
export_div <- export_div %>%
  rename(country=YEAR) %>%
  filter(country != 'YEAR',country != 'MEASURE') %>%
  melt(id.vars='country') %>%
  rename(year=variable) %>%
  mutate(year=as.numeric(as.character(year)),
         value=as.numeric(value),
         varstr='Export Diversification') %>%
  na.omit

export_div %>%
  filter(country %in% c('Mexico','Honduras','El Salvador')) %>%
  ggplot(aes(x=year,y=value,group=country,color=country)) +
  geom_line()

# IFs variables to use
# imports, exports as fraction of economy
# FDI inflows?


###############################################################################
# ICT use (WEF) -- subindex C of networked readiness
###############################################################################
ict_use <- read_excel('data/WEF_NRI_2012-2016_Historical_Dataset.xlsx',sheet='Data',skip=3,col_types='text') %>%
  filter(`GLOBAL ID`=='NRI.C',
         Attribute=='Value') %>%
  mutate(year=as.numeric(Edition)) %>%
  select(-Placement,-Dataset,-Edition,-`GLOBAL ID`,-`Code NRI 2016`,-Series,
         -`Series unindented`,-Attribute) %>%
  melt(id.var='year') %>%
  mutate(value=as.numeric(value),
         country=as.character(variable),
         varstr='ICT Use') %>%
  select(country,year,value,varstr) %>%
  na.omit

# IFs series to use
# "Cyber benefit"
# ICT development index
# population with internet (under globalization)
# electricity, ICT infrastructure variables
# technology power

###############################################################################
# GDP per capita
# This one is in IFs, but I'll extract the WB series anyway to use for PCA
##############################################################################
gdppc <- read_csv('data/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_9984840.csv',skip=3) %>%
  filter(`Country Name` != "Country Name") %>%
  rename(country=`Country Name`) %>%
  select(country,starts_with('19'),starts_with('20')) %>%
  melt(id.vars='country') %>%
  mutate(year=as.numeric(as.character(variable)),
         varstr='GDP per capita') %>%
  select(country,year,value,varstr) %>% 
  na.omit()
  
gdppc %>%
  filter(country %in% c('Mexico','Honduras','El Salvador')) %>%
  ggplot(aes(x=year,y=value,group=country,color=country)) +
  geom_line()

###############################################################################
# Poverty rate
# Note that the JSR roadmap mentions $5/day, but the WB dataset cutoff is at
# $5.50 a day. I went with the WB data rather than calculating a custom 
# dataset.
# This one is in IFs, but I'll extract the WB series anyway to use for PCA
##############################################################################
poverty <- read_csv('data/API_SI.POV.UMIC_DS2_en_csv_v2_10143666.csv',skip=3) %>%
  filter(`Country Name` != "Country Name") %>%
  rename(country=`Country Name`) %>%
  select(country,starts_with('19'),starts_with('20')) %>%
  melt(id.vars='country') %>%
  mutate(year=as.numeric(as.character(variable)),
         varstr='Poverty rate') %>%
  select(country,year,value,varstr) %>% 
  na.omit()

poverty %>%
  filter(country %in% c('Denmark','China','Mexico')) %>%
  ggplot(aes(x=year,y=value,group=country,color=country)) +
  geom_line()

###############################################################################
# rbind everything into a single data frame; I can filter them back out later
###############################################################################
all_jsr <- rbind(biodiversity_habitat, business_environment, child_health,
                 diag_acc, export_div, gdppc, gender_gap, gov_effect,
                 group_equality, ict_use, lib_dem, open_gov, poverty, safety,
                 tax_admin, trade_freedom)
rm(biodiversity_habitat, business_environment, child_health,
   diag_acc, export_div, gdppc, gender_gap, gov_effect,
   group_equality, ict_use, lib_dem, open_gov, poverty, safety,
   tax_admin, trade_freedom,
   ipd_all, og_2014, short_names, epi_backcast, get_og, tf_read)
