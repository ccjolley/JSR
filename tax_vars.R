library(readr)
library(dplyr)
source('utils.R')

get_tax_vars <- function() {
  wd <- getwd()
  setwd("~/Projects/JSR/IFs_exports/tax")
  tax_vars <- load_ifs(list.files(pattern='.*txt$')) %>% 
    mutate(tax_firm_to_revenue = tax_firm_usd / gov_revenue_total_usd,
           tax_firm_to_gdp_ppp = tax_firm_usd / gdp_ppp,
           tax_firm_to_gdp_mer = tax_firm_usd / gdp_mer,
           tax_hh_total_to_revenue = tax_hh_total / gov_revenue_total_usd,
           tax_hh_total_to_gdp_ppp = tax_hh_total / gdp_ppp,
           tax_hh_total_to_gdp_mer = tax_hh_total / gdp_mer,
           tax_hh_skilled_to_revenue = tax_hh_skilled / gov_revenue_total_usd,
           tax_hh_skilled_to_gdp_ppp = tax_hh_skilled / gdp_ppp,
           tax_hh_skilled_to_gdp_mer = tax_hh_skilled / gdp_mer,
           tax_hh_unskilled_to_revenue = tax_hh_unskilled / gov_revenue_total_usd,
           tax_hh_unskilled_to_gdp_ppp = tax_hh_unskilled / gdp_ppp,
           tax_hh_unskilled_to_gdp_mer = tax_hh_unskilled / gdp_mer,
           tax_indirect_to_revenue = tax_indirect_usd / gov_revenue_total_usd,
           tax_indirect_to_gdp_ppp = tax_indirect_usd / gdp_ppp,
           tax_indirect_to_gdp_mer = tax_indirect_usd / gdp_mer,
           tax_sswelf_to_revenue = tax_sswelf_usd / gov_revenue_total_usd,
           tax_sswelf_to_gdp_ppp = tax_sswelf_usd / gdp_ppp,
           tax_sswelf_to_gdp_mer = tax_sswelf_usd / gdp_mer) %>%
    select(country,year,ends_with('_to_revenue'),ends_with('_to_gdp_ppp'),
           ends_with('to_gdp_mer'))
  setwd(wd)
  tax_vars
}


