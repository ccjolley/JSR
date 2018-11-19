library(dplyr)
library(ggplot2)
library(forcats)
library(reshape2)
library(readr)

###############################################################################
# Define USAID themes & colors
###############################################################################

theme_USAID <- theme( 
  axis.ticks = element_blank(), 
  axis.ticks.length = unit(0, units = "points"), 
  
  strip.text = element_blank(),
  strip.background = element_blank(),
  
  panel.border = element_blank(), 
  panel.grid = element_blank(), 
  panel.background = element_blank(), 
  plot.background = element_blank(),
  legend.key = element_blank(),
  text=element_text(family="Liberation Sans")) 

approved_colors <-c(
  '#002F6C', # USAID blue
  '#BA0C2F',  # USAID red
  '#6C6463',   # dark gray
  '#A7C6ED', # light blue
  '#651D32', # dark red
  '#CFCDC9' # light gray
) 

colors_USAID <- scale_color_manual(values=approved_colors)
fill_USAID <- scale_fill_manual(values=approved_colors)

###############################################################################
# Rename countries and groups
###############################################################################
geo_rename <- function(x) {
  x %>%
    sub('WB LowMidIncome Economies','LMIC',.) %>%
    sub('WB UpMidIncome Economies','UMIC',.) %>%
    sub('WB Low-Income Economies','Low-Income',.) %>%
    sub('AfrCul-West','West Africa',.) %>%
    sub('Cote dIvoire','Cote d\'Ivoire',.)
}

###############################################################################
# Country-comparison plot
###############################################################################
country_compare <- function(fname,ytitle,dots=TRUE,label_split=0.2, label_height=0.9,ontop=NULL) {
  ifs_df <- read_tsv(fname, col_names = c('varstr','year','value')) %>%
    na.omit %>%
    mutate(Country = sub('^.*\\(','',varstr) %>% sub(',.*?\\)','',.) %>% sub('\\)','',.),
           Country = geo_rename(Country),
           Country = fct_reorder(Country,value,.desc=TRUE)) %>%
    select(Country,year,value)
  
  p <- ggplot(data=ifs_df,aes(x=year,y=value,group=Country,color=Country)) +
    geom_line(size=1) +
    xlab('Year') +
    ylab(ytitle) +
    theme_USAID + colors_USAID
  cutoff <- 2014.5 # last year of historical data is 2014
  if (max(ifs_df$year) > cutoff & min(ifs_df$year) < cutoff) {
    lh <- min(ifs_df$value) + label_height*(max(ifs_df$value) - min(ifs_df$value))
    p <- p + 
      geom_vline(xintercept=cutoff,color='black',linetype=2) +
      annotate('text',y=lh,x=cutoff,hjust=1+label_split,label='History') +
      annotate('text',y=lh,x=cutoff,hjust=-label_split,label='Forecast')   
  }
  if (dots) { p <- p + geom_point(size=2)}
  if (!is.null(ontop)) {
    p <- p + geom_line(data=ifs_df[ifs_df$Country==ontop,],size=1)
  }
  p
}

###############################################################################
# Scenario-comparison plot
###############################################################################
scenario_compare <- function(fname,ytitle,cutoff=2014.5,text_orient='horizontal',
                             base_label='Base') {
  ifs_df <- read_tsv(fname, col_names = c('varstr','year','value')) %>%
    na.omit %>%
    mutate(Scenario = sub('^.*\\[','',varstr) %>% sub('\\].*','',.),
           Scenario = ifelse(Scenario=='Sustain','Sustainability',Scenario),
           Scenario = ifelse(Scenario==base_label,'Base',Scenario),
           Scenario = fct_reorder(Scenario,value,.desc=TRUE)) %>%
    select(Scenario,year,value)
  label_height <- min(ifs_df$value) + 0.9*(max(ifs_df$value) - min(ifs_df$value))
  p <- ggplot(data=ifs_df,aes(x=year,y=value,group=Scenario,color=Scenario)) +
    geom_line(size=1) +
    xlab('Year') +
    ylab(ytitle) +
    theme_USAID + colors_USAID +
    geom_vline(xintercept=cutoff,color='black',linetype=2) 
  if (text_orient=='horizontal') {
    p + annotate('text',y=label_height,x=cutoff,hjust=1.2,label='History') +
      annotate('text',y=label_height,x=cutoff,hjust=-0.2,label='Forecast')    
  } else if (text_orient == 'vertical') {
    p + annotate('text',y=label_height,x=cutoff,vjust=-0.7,angle=90,label='History') +
      annotate('text',y=label_height,x=cutoff,vjust=1.7,angle=90,label='Forecast') 
  } else { p }
}




