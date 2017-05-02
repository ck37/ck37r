library(SuperLearner)
library(ck37r)

data(Boston, package = "MASS")

set.seed(1)
sl = SuperLearner(Boston$chas, subset(Boston, select = -chas), family = binomial(),
                       SL.library = c("SL.mean", "SL.glmnet"))

sl_auc_table(sl, Y = Boston$chas)
