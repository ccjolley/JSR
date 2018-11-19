Plots I need for technical writeup:

1. Path on the scatterplot, revised for better algorithms (and contrasting algorithms)
  Got it for xgboost; see how it looks for SVM
2. Single-country trace, contrasting different models
  I think group equality would be good for this
3. Accuracy comparison for different models (and possibly for stacked one)
  I have the code I need to make the plot
4. Accuracy and bias checks for improved model, maybe showing effects of fixed effects

Things to do to improve models:

2. Get more features -- export any historical/forecast series I've missed (esp. GDP variants), get gender disparaties where possible,    
   find a way to calculate (approximate) first derivatives (e.g. by taking slope off of loess smoothing) 
3. Can I do importance calculations for SVM the way I do for xgboost? I could write something that would do it.
3. Principled way of shortening my feature list (e.g. with LASSO or importance) -- will be important for KNN, maybe others
3. Try out models I haven't yet (esp. KNN)
4. Random parameter search for xgboost

      

6. Can I do something smarter by dipping into time series methods? Probably...