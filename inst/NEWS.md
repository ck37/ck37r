# News for ck37r

## 1.0.3 (2020-02-03)

### Features

* impute_missing_values
   - initial support for GLRM imputation

* prauc_table
   - table of precision-recall AUCs for SL or CV.SL ensembles.
   
### Other changes

* doMC is no longer supported.

## 1.0.1 (2018-06-10)

### Features

* auc_table - default null hypothesis is now the highest AUC.

* csvl_auc_table() - table of AUCs for each learner, plus Discrete SL and SL.

* factors_to_indicators
   - return indicators as integers rather than numerics.
   - significant speed improvements
   - add max_levels limit, default to 200.
   - allow missing values in factors
   
* h2o_init_multinode - create a multi-node h2o cluster e.g. on a SLURM system.

* impute_missing_values
  - add all_vars argument; if T save the imputation values even when variable is
    not missing data. This allows new datasets to be imputed to the values from the
    training data. Treated labelled columns as numerics.
  - support integer64 numeric type (data.table) 

* impute_missing_values
  - treat haven::labelled values as numerics.
  - save variable names into the impute list.
  - support supplying imputation values from another dataset, for use when
  scoring a model to a larger dataset.
  
* load_all_code() - changed default environment from baseenv() to .GlobalEnv,
which will make explicit package references unnecessary.

* missingness_indicators - return matrix of integers rather than numerics.

* set_java_memory() - specify maximum memory usage for rJava JVM.

* get_java_memory() - get maximum memory allocated to a rJava JVM.

* Mode - option to not choose NA as a mode, set TRUE by default.

* plot_roc - plot the best learner, not necessarily the SuperLearner.

* sl_h2o_auto - automatic machine learning via h2o.

* sl_mgcv - add mgcv wrapper for splines.

* sl_xgboost_cv - integrate support for gaussian family outcomes.

* vim_corr - new function to rank covariates by their (possibly weighted)
  univariate correlation with the outcome.

### Bug fixes

* factors_to_indicators
   - don't drop matrix to a vector, e.g. when using analyzing a single covariate.

* impute_missing_values
   - fixed important bug where incorrect column indices could be used when skip_vars was not blank.
   - fix missingness indicators when no indicators need to be created. 

* missingness_indicators
   - Fix when only one column of missingness indicators needs to be created.

* plot_roc - fix axis labels for updated ggplot2.

## 1.0.0 (2017-06-03)

Initial release on CRAN.
