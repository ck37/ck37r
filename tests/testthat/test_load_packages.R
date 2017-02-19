library(ck37r)

# Load these 4 packages but don't install any.
load_packages(c("MASS", "SuperLearner", "tmle", "doParallel"))

# Load these 4 packages and install them if necessary.
load_packages(c("MASS", "SuperLearner", "tmle", "doParallel"), auto_install = TRUE)
