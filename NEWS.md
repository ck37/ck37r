# News for ck37r

## Development

### Features

* csvl_auc_table() - table of AUCs for each learner, plus Discrete SL and SL.

* load_all_code() - changed default environment from baseenv() to .GlobalEnv,
which will make explicit package references unnecessary.

* impute_missing_values - add all_vars argument; if T save the imputation values
even when variable is not missing data. This allows new datasets to be imputed
to the values from the training data. Treated labelleded columns as numerics.

* impute_missing values - support supplying imputation values from another dataset,
for use when scoring a model to a larger dataset.

* h2o_init_multinode - create a multi-node h2o cluster e.g. on a SLURM system.

* set_java_memory() - specify maximum memory usage for rJava JVM.

* get_java_memory() - get maximum memory allocated to a rJava JVM.

* Mode - option to not choose NA as a mode, set TRUE by default.

### Bug fixes

* impute_missing_values - fixed important bug where incorrect column indices could be used when skip_vars was not blank.

## 1.0.0 (2017-06-03)

Initial release on CRAN.
