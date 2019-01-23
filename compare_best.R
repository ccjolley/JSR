source('predict_systematic.R')
source('wrappers.R')
source('prepare.R')
library(purrr)
ifs_basic <- load_all_ifs()

model_list <- list(
  biodiv=list(label='Biodiversity & Habitat', wrapper=xgb_wrap),
  biz_env=list(label='Business Environment', wrapper=xgb_wrap),
  child_health=list(label='Child Health', wrapper=xgb_wrap),
  diag_acc=list(label='Diagonal Accountability', wrapper=xgb_wrap),
  export_div=list(label='Export Diversification', wrapper=xgb_wrap),
  gender_gap=list(label='Economic gender gap', wrapper=xgb_wrap),
  gov_effect=list(label='Government Effectiveness', wrapper=xgb_wrap),
  group_eq=list(label='Group Equality', wrapper=xgb_wrap),
  ict_use=list(label='ICT Use', wrapper=xgb_wrap),
  lib_dem=list(label='Liberal Democracy', wrapper=xgb_wrap),
  open_gov=list(label='Open Government', wrapper=xgb_wrap),
  safety=list(label='Safety & Security', wrapper=xgb_wrap),
  tax_admin=list(label='Tax Administration', wrapper=lm_wrap, feature=lasso_min),
  trade_freedom=list(label='Trade Freedom', wrapper=xgb_wrap)
)

###############################################################################
# Visualize results
###############################################################################
best_scores <- map_dbl(model_list,xval)

data.frame(model=map_chr(model_list,"label"),score=best_scores) %>%
  mutate(model=fct_reorder(model,score)) %>%
  ggplot(aes(x=model,y=score)) +
  geom_bar(stat='identity',fill='#BA0C2F') +
  coord_flip() +
  theme_USAID +
  xlab(NULL) + ylab('NRMSE') +
  ggtitle('Scores as of 01/11/2019')

plotme <- data.frame(score=best_scores,
                     data=map_int(model_list, function(x) 
                       { all_jsr %>% filter(varstr==x$label) %>% nrow }),
                     label=map_chr(model_list,'label'))
ggplot(plotme,aes(x=data,y=score,label=label)) +
  geom_point(size=2,color='#BA0C2F') +
  geom_text_repel() +
  theme_USAID +
  scale_x_log10() +
  xlab('Number of data points') + ylab('NRMSE')
