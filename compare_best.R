source('predict_systematic.R')
source('wrappers.R')
source('prepare.R')
library(purrr)
ifs_basic <- load_all_ifs()

model_list <- list(
  biodiv=list(label='Biodiversity & Habitat', wrapper=xgb_wrap, prepare=NULL),
  biz_env=list(label='Business Environment', wrapper=xgb_wrap, prepare=NULL),
  child_health=list(label='Child Health', wrapper=xgb_wrap, prepare=NULL),
  diag_acc=list(label='Diagonal Accountability', wrapper=xgb_wrap, prepare=NULL),
  export_div=list(label='Export Diversification', wrapper=xgb_wrap, prepare=NULL),
  gender_gap=list(label='Economic gender gap', wrapper=xgb_wrap, prepare=NULL),
  gov_effect=list(label='Government Effectiveness', wrapper=xgb_wrap, prepare=NULL),
  group_eq=list(label='Group Equality', wrapper=xgb_wrap, prepare=NULL),
  ict_use=list(label='ICT Use', wrapper=xgb_wrap, prepare=NULL),
  lib_dem=list(label='Liberal Democracy', wrapper=xgb_wrap, prepare=NULL),
  open_gov=list(label='Open Government', wrapper=xgb_wrap, prepare=NULL),
  safety=list(label='Safety & Security', wrapper=xgb_wrap, prepare=NULL),
  tax_admin=list(label='Tax Administration', wrapper=lm_wrap, prepare=lasso_1se),
  trade_freedom=list(label='Trade Freedom', wrapper=xgb_wrap, prepare=NULL)
)

###############################################################################
# Visualize results
###############################################################################
best_scores <- map_dbl(model_list,xval_new)

map_dbl(model_list['open_gov'],xval_new)

data.frame(model=map_chr(model_list,"label"),score=best_scores) %>%
  mutate(model=fct_reorder(model,score)) %>%
  ggplot(aes(x=model,y=score)) +
  geom_bar(stat='identity',fill='#BA0C2F') +
  coord_flip() +
  theme_USAID +
  xlab(NULL) + ylab('NRMSE') +
  ggtitle('Scores as of 01/09/2019')


