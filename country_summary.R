library(dplyr)
library(ggplot2)
library(reshape2)
library(forcats)
source('utils.R')

median_dev <- list.files(pattern='predict.*csv') %>%
  plyr::ldply(read_csv) %>%
  filter(!(country=='North Korea' & varstr=='Trade Freedom'))

dev_plot <- function(cname) {
  lines <- median_dev %>%
    filter(country==cname) %>%
    mutate(direction=ifelse(pred > value,'Positive','Negative'),
           direction=factor(direction,levels=c('Positive','Negative')),
           varstr=fct_reorder(varstr,value)) 
  medians <- median_dev %>%
    mutate(varstr=factor(varstr,levels=levels(lines$varstr)))
  ggplot(lines,aes(y=varstr,yend=varstr)) +
    geom_point(data=medians,aes(y=varstr,x=pred),size=1,color='#CFCDC9') +
    geom_segment(aes(x=pred,xend=value,color=direction),size=3) +
    geom_point(aes(x=pred),color='black',size=4) +
    geom_point(aes(x=value),color='black',size=4) +
    geom_point(aes(x=pred),color='white',size=3) + 
    colors_USAID +
    theme_USAID +
    xlab('Normalized JSR metric value') + ylab(NULL)
