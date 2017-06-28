# News for ck37r

## Development

* impute_missing_values - fixed important bug where incorrect column indices could be used when skip_vars was not blank.

* impute_missing_values - add all_vars argument; if T save the imputation values
even when variable is not missing data. This allows new datasets to be imputed
to the values from the training data.

## 1.0.0 (2017-06-03)

Initial release on CRAN.
