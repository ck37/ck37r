library(ck37r)
library(SuperLearner)
library(testthat)

context("RF count terminal nodes")

data(Boston, package = "MASS")

set.seed(1, "L'Ecuyer-CMRG")

# Subset rows to speed up example computation.
row_subset = sample(nrow(Boston), 200)

Boston = Boston[row_subset, ]

# Outcome needs to be a binary variable.
X = subset(Boston, select = -chas)

sl = SuperLearner(Boston$medv, X[, 1:3], family = gaussian(),
                  cvControl = list(V = 3),
                  SL.library = c("SL.mean", "SL.glm", "SL.randomForest"))

sl

summary(rf_count_terminal_nodes(sl$fitLibrary$SL.randomForest_All$object))

max_terminal_nodes = max(rf_count_terminal_nodes(sl$fitLibrary$SL.randomForest_All$object))

max_terminal_nodes

# Now run create.Learner() based on that maximum.

# It is often handy to convert to log scale of a hyperparameter before testing a ~linear grid.
# NOTE: -0.7 ~ 0.69 ~ log(0.5) which is the multiplier that yields sqrt(max)
maxnode_seq = unique(round(exp(log(max_terminal_nodes) *
                                 exp(c(-0.97, -0.7, -0.45, -0.15, 0)))))
maxnode_seq

rf =
  SuperLearner::create.Learner("SL.randomForest", detailed_names = T,
                               name_prefix = "rf",
                               # fewer trees for testing speed only.
                               params = list(ntree = 10),
                               tune = list(maxnodes = maxnode_seq))

sl = SuperLearner(Boston$medv, X[, 1:3], family = gaussian(),
                  cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.glm", rf$names))

sl

