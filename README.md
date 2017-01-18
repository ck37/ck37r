# ckTools
Custom R functions for various things that I use across multiple projects.

## Functions

* `parallelize` - starts a multicore or multinode parallel cluster. Automatically detects parallel nodes in a SLURM environment, which makes code work seemlessly on a laptop or a cluster.
* `stop_cluster` - stops a cluster started by `parallelize()`.
* `load_packages` - load a list of packages; for the ones that fail it can attempt to install them automatically from CRAN, then load them again.
* `impute_missing_values` - impute missing values in a dataframe (median for numerics and mode for factors), add missingness indicators.
* `missingness_indicators` - return a matrix of missingness indicators for a dataframe.
* `load_all_code` - source() all R files in a given directory.
* `import_csvs` - import all CSV files in a given directory.
* `gen_superlearner` - create a SuperLearner and CV.SuperLearner function setup according to a given parallelization configuration.
* `tmle_parallel` - allows the SuperLearner estimation in TMLE to be customized, esp. to support parallel estimation via mcSuperLearner and snowSuperLearner.
* `setup_parallel_tmle` - helper function to start a cluster and setup SuperLearner and tmle_parallel to use the created cluster.

## Examples

### Parallel TMLE

```r
library(ckTools)
library(tmle)

# Use multiple cores as available.
ckTools::setup_parallel_tmle()

# Basic SL library.
sl_lib = c("SL.mean", "SL.rpart", "SL.glmnet")

# Set a parallel-compatible seed so cross-validation folds are deterministic.
set.seed(1, "L'Ecuyer-CMRG")
result = run_tmle(Y = Y, A = A, W = W, family = "binomial",
                  g.SL.library = sl_lib, Q.SL.library = sl_lib)
```

More examples to be added.
