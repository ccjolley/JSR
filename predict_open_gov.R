source('predict_systematic.R')

ifs_basic <- load_all_ifs()
open_gov_ifs <- ml_prep(open_gov,ifs_basic) 

xval(open_gov_ifs,lm_wrap) # 20.306; threw rank-deficiency error
xval(open_gov_ifs,svm_wrap) # 0.399
xval(open_gov_ifs,xgb_wrap) # 0.417

# This is the worst performance on any of the models; ideally I'd like to get this down below about 0.25.
