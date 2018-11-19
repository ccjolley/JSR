source('predict_systematic.R')

ifs_basic <- load_all_ifs()
bio_ifs <- ml_prep(biodiversity_habitat,ifs_basic)

xval(bio_ifs,lm_wrap,lo=0,hi=100) # 0.484 - adding limits doesn't help accuracy
pred_scatter(bio_ifs,lm_wrap,filter_year=2012:2016,lo=0,hi=100)
country_trend(bio_ifs,'Kenya',lm_wrap)
forecast_plot(biodiversity_habitat,ifs_basic,'Kenya',lm_wrap) # gets better!

# TODO: try knn -- don't remember how to do this

svm_tune <- tune(svm,value ~ .,
                 data=select(bio_ifs,-country,-varstr),
                 ranges=list(epsilon=0.1*(1:10),
                             cost=2^(0:9)))
plot(svm_tune)
print(svm_tune)
xval(bio_ifs,svm_wrap) # 0.272
pred_scatter(bio_ifs,svm_wrap,filter_year=2012:2016)
# because this splits into train and test now, results are sort of stochastic
# TODO: make plot use open circles instead of filled ones
# TODO: need to figure out how to pass arbitrary parameters into pred_scatter() and on to wrapper
country_trend(bio_ifs,'Kenya',svm_wrap)
forecast_plot(biodiversity_habitat,ifs_basic,'Kenya',svm_wrap) # gets worse!

xval(bio_ifs,xgb_wrap,lo=0,hi=100) # 0.251 - adding limits doesn't change things much
pred_scatter(bio_ifs,xgb_wrap,filter_year=2012:2016)
country_trend(bio_ifs,'Kenya',xgb_wrap)
forecast_plot(biodiversity_habitat,ifs_basic,'Kenya',xgb_wrap) # gets worse suddenly

