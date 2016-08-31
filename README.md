# ckTools
Custom R functions for various things

## Functions

* `parallelize` - starts a multicore or multinode parallel cluster. Automatically detects parallel nodes in a SLURM environment, which makes code work seemlessly on a laptop or a cluster.
* `stop_cluster` - stops a cluster started by `parallelize()`.
* `load_packages` - load a list of packages; for the ones that fail it will attempt to install them automatically from CRAN, then load them again.
* `impute_missing_values` - impute missing values in a dataframe (median for numerics and mode for factors), add missingness indicators.
* `missingness_indicators` - return a matrix of missingness indicators for a dataframe.

## Examples

To be added.
