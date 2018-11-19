source('predict_systematic.R')
ifs_basic <- load_all_ifs()

###############################################################################
# For each JSR series, get 10-fold cross validation of xgboost with defaults
# left tax_admin out; there just isn't enough data for this approach.
###############################################################################
all_jsr <- list(biodiversity_habitat,business_environment,child_health,diag_acc,
                export_div,gender_gap,gov_effect,group_equality,ict_use,
                lib_dem,open_gov,safety,trade_freedom)
xgb_scores <- plyr::ldply(all_jsr,function(x) {
  print(x$varstr[1])
  data.frame(
    var=x$varstr[1],
    score=xval(ml_prep(x,ifs_basic),xgb_wrap)
)})

xval(ml_prep(tax_admin2,ifs_basic),xgb_wrap,fold=1)

xgb_scores %>%
  mutate(highlight=ifelse(score>quantile(xgb_scores$score,0.95),TRUE,FALSE),
         var=fct_reorder(var,score)) %>%
    ggplot(aes(x=var,y=score,fill=highlight)) +
    geom_bar(stat='identity') +
    coord_flip() +
    theme(axis.title.y = element_blank(),
          legend.position = 'none') +
    ylab('Normalized RMSE') +
    theme_USAID +
    fill_USAID
