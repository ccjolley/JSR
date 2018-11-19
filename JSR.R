# JSR plan of attack:
#
# 1. Write code to extract all data series from the various Excel files they currently live in.
#    This should result in a set of tidy data frames with consistent column names (country, year, varname, value) 
########### DONE
# 2. rbind all of these together, and count how many times country names appear. Use this to create a map of geographic
#    JSR data coverage, as well as visualizations of temporal coverage. This will also be an opportunity to standardize
#    country names; I shoulds standardize them to match (for now) with the Natural Earth admin-0 shapefile.
########## DONE
# 3. Figure out how to fill in gaps in a data series with cubic interpolation
########## USE spline FUNCTION
# 4. For each JSR metric, export a set of relevant data series from IFs, interpolate as needed, and fit a 
#    model (just lm for now). This will allow me to generate JSR metric values for any country in any year
#   (including the future).
########## DID THIS FOR LIB DEM -- NOW I HAVE A PIPELINE
# 5. Come up with a standardized visualization to show the measured and imputed values for a particular metric
#    and country -- these will go in future Futures reports.
######### I HAVE A MOCK-UP EXAMPLE
# 6. Once I can get spatially and temporally-complete metric sets, I can use PCA to evaluate the extent to which they really do 
#    align into discrete metrics for capacity and commitment. Document the weighted contribution of each variable to the first PC,
#    and the overall fraction of variance captured by this PC. 
######### STARTED ON THIS
# 7. Improve on modeling of metrics from IFs, exploring other correlates, models, hyperparameters, etc.

