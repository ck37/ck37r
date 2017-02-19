[![Build Status](https://travis-ci.org/ck37/varImpact.svg?branch=master)](https://travis-ci.org/ck37/varImpact)
# ck37r

My R toolkit for organizing analyses, parallelizing code for multiple cores or in a SLURM cluster, and extended functionality for [SuperLearner](http://github.com/ecpolley/SuperLearner) and [TMLE](github.com/cran/tmle). Some of the SuperLearner functions may eventually be migrated into the SuperLearner package.

## Installation

Install from github:

```{r}
install.packages("devtools") # Install devtools if needed.
devtools::install_github("ck37/ck37r")
```

## Functions

- General
  * `import_csvs` - import all CSV files in a given directory.
  * `impute_missing_values` - impute missing values in a dataframe (median for numerics and mode for factors; or k-nearest neighbors), add missingness indicators.
  * `load_all_code` - source() all R files in a given directory.
  * `load_packages` - load a list of packages; for the ones that fail it can attempt to install them automatically from CRAN, then load them again.
  * `missingness_indicators` - return a matrix of missingness indicators for a dataframe, (optionally) omitting any constant or collinear columns.
  * `standardize` - standardize a dataset (center, scale), optionally omitting certain variables.
- Parallelization
  * `parallelize` - starts a multicore or multinode parallel cluster. Automatically detects parallel nodes in a SLURM environment, which makes code work seemlessly on a laptop or a cluster.
  * `stop_cluster` - stops a cluster started by `parallelize()`.
- Random Forest
  * `rf_count_terminal_nodes` - count the number of terminal nodes in each tree in a random forest. That information can then be used to grid-search the maximum number of nodes allowed in a Random Forest (along with mtry).
- SuperLearner
  * `gen_superlearner` - create a SuperLearner and CV.SuperLearner function setup to transparently use a certain parallelization configuration.
  * `cvsl_weights` - table of the meta-weight distribution for each learner in a CV.SuperLearner analysis.
  * `cvsl_auc` - cross-validated AUC for a CV.SuperLearner analysis.
  * `cvsl_plot_roc` - ROC plot with AUC for a CV.SuperLearner analysis.
  * `plot.SuperLearner` - plot risk estimates and CIs for a SuperLearner, similar to CV.Superlearner except without SL or Discrete SL.
  * `sl_auc` - table of cross-validated AUCs for each learner in a SuperLearner ensemble, including SE, CI, and p-value.
  * `sl_stderr` - calculate standard error for each learner's risk in SL.
  * `sl_plot_roc` - ROC curve plot for one learner in an SL ensemble, plus AUC and CI.
- TMLE
  * `tmle_parallel` - allows the SuperLearner estimation in TMLE to be customized, esp. to support parallel estimation via mcSuperLearner and snowSuperLearner.
  * `setup_parallel_tmle` - helper function to start a cluster and setup SuperLearner and tmle_parallel to use the created cluster.

## Examples

### Parallel TMLE

```r
library(ck37r)
library(tmle)

# Use multiple cores as available.
ck37r::setup_parallel_tmle()

# Basic SL library.
sl_lib = c("SL.mean", "SL.rpart", "SL.glmnet")

# Set a parallel-compatible seed so cross-validation folds are deterministic.
set.seed(1, "L'Ecuyer-CMRG")
result = run_tmle(Y = Y, A = A, W = W, family = "binomial",
                  g.SL.library = sl_lib, Q.SL.library = sl_lib)
```

More examples to be added.
